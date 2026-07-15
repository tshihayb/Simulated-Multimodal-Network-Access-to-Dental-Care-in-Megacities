# =============================================================================
#  _r13_enhanced.R  --  R1-3 ENHANCED routing capture + EXACT selection-changing
#                        sweeps (metro line-change 50->200m, combo-priority,
#                        TOL_PCT) + per-type transfer-penalty split.
# -----------------------------------------------------------------------------
#  Sourced by the RUN_R13_ENH-gated hooks in block 17b of Section 18 (unweighted)
#  and Section 19 (weighted), AFTER the cheap-pass (_r13_orchestrate.R). Runs in
#  the global env so it sees the memoized routers (get_L2_distance_simple/real,
#  get_path_details, .PCACHE), graphs (g_metro_only, g_multimodal), the selection
#  helpers (select_best_by_pct_tolerance, get_combo_priority, TOL_PCT), the
#  precomputed lookups (.rp_*, .cl_*, .l1*, .l3*, .ct_*, .rc_*), the L1/L3 lookup
#  fns (get_L1_from_lookup, get_L3_from_lookup) + ratio constants, and results_df.
#
#  WHY a separate replay (not an edit to the validated loop): the certified loop
#  saves only the SELECTED best path per trip; the priority/TOL/metro-xfer sweeps
#  CHANGE selection, so they need ALL candidate combos. This block REPLAYS the
#  Part-12 candidate construction for every trip (reusing the warm .PCACHE), and
#  is PURELY ADDITIVE -- the validated loop, its cache, and certified outputs are
#  untouched. Because it must route every trip's candidates, the enhanced run is
#  a FULL routing pass: run with the loops ENABLED (do NOT set SKIP_S18_LOOP/
#  SKIP_S19_LOOP) so .PCACHE warms; it then adds ~3 get_path_details/trip for the
#  non-selected candidates. Default OFF (RUN_R13_ENH unset).
#
#  EXACTNESS: .impl_l2r (multimodal L2) = sum of real_distance over the shortest
#  path with first/last metro_metro_transfer masked => L2 = d_metro+d_brt+d_std+
#  d_walk, and metro line-changes sit inside d_walk as n_metro_tr*50m. So under a
#  metro line-change of X metres, L2(X) = L2 + n_metro_tr*(X-50), holding the
#  edge-path fixed (50<->200m << km chains => selection-relevant path is stable;
#  this is the locked KEY EFFICIENCY INSIGHT). At X=50, tol=TOL_PCT, default prio
#  the re-selection reproduces the certified best_total_m EXACTLY (validation gate).
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

# -----------------------------------------------------------------------------
# .r13_capture_one() -- ONE ROW PER CANDIDATE (<=4 multimodal + 1 metro-only) for
#   a single trip, enriched with the per-candidate 4-way distance split + per-type
#   transfer counts. Pure: all L1/L3/nodes are passed in (computed by the caller
#   with the SAME method the certified loop used for that anchor class), so this
#   reproduces selection faithfully for both base and new anchors.
# -----------------------------------------------------------------------------
.r13_capture_one <- function(rp_id, dest_type, road_dist_m,
                             L1_metro_m, L1_bus_m, L3_metro_m, L3_bus_m,
                             near_metro_rp_id, near_bus_rp_id, near_bus_rt_type,
                             near_metro_cl_id, near_bus_cl_id, near_bus_cl_rt_type) {
  rows <- list()
  .mk <- function(branch, type, l1_mode, l3_mode, l1, l2, l3, prio, l1_rt, l3_rt, det) {
    data.frame(rp_id=rp_id, dest_type=dest_type, road_dist_m=road_dist_m,
               branch=branch, type=type, l1_mode=l1_mode, l3_mode=l3_mode,
               l1=l1, l2=l2, l3=l3, prio=prio,
               l1_rt=ifelse(is.null(l1_rt), NA_character_, l1_rt),
               l3_rt=ifelse(is.null(l3_rt), NA_character_, l3_rt),
               d_metro=det$dist_metro_m, d_brt=det$dist_brt_m, d_std=det$dist_bus_std_m,
               d_walk=det$dist_walk_transfer_m,
               n_metro_tr=det$n_metro_transfers, n_bus_tr=det$n_bus_transfers,
               n_mode_sw=det$n_mode_switches, n_stops=det$n_stops,
               seg_str=ifelse(is.null(det$seg_str_metro), "", det$seg_str_metro),
               stringsAsFactors=FALSE)
  }

  # ---- metro-only candidate ----
  if (!is.na(near_metro_rp_id) && !is.na(near_metro_cl_id) && near_metro_rp_id != near_metro_cl_id) {
    L2_mo <- get_L2_distance_simple(near_metro_rp_id, near_metro_cl_id, g_metro_only)
    if (!is.na(L2_mo)) {
      det <- get_path_details(g_metro_only, near_metro_rp_id, near_metro_cl_id)
      rows[[length(rows)+1]] <- .mk("metro_only", "Metro-only", "metro", "metro",
                                    L1_metro_m, L2_mo, L3_metro_m, 10, NA, NA, det)
    }
  }

  # ---- multimodal candidates (mirror combos at L7112-7131) ----
  invalid_metro_bus <- (near_bus_cl_id == near_bus_rp_id)
  invalid_bus_metro <- (near_metro_cl_id == near_metro_rp_id)
  combos <- list(
    list(type="Metro-Metro", l1=L1_metro_m, l3=L3_metro_m, l1_mode="metro", l3_mode="metro", s=near_metro_rp_id, e=near_metro_cl_id),
    list(type="Metro-Bus",   l1=L1_metro_m, l3=L3_bus_m,   l1_mode="metro", l3_mode="bus",   s=near_metro_rp_id, e=near_bus_cl_id),
    list(type="Bus-Metro",   l1=L1_bus_m,   l3=L3_metro_m, l1_mode="bus",   l3_mode="metro", s=near_bus_rp_id,   e=near_metro_cl_id),
    list(type="Bus-Bus",     l1=L1_bus_m,   l3=L3_bus_m,   l1_mode="bus",   l3_mode="bus",   s=near_bus_rp_id,   e=near_bus_cl_id)
  )
  for (cmb in combos) {
    if (is.na(cmb$s) || is.na(cmb$e) || cmb$s == cmb$e) next
    if (cmb$type == "Metro-Bus" && invalid_metro_bus) next
    if (cmb$type == "Bus-Metro" && invalid_bus_metro) next
    l2 <- get_L2_distance_real(cmb$s, cmb$e, g_multimodal)
    if (is.na(l2)) next
    prio <- get_combo_priority(l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, l1_rt=near_bus_rt_type, l3_rt=near_bus_cl_rt_type)
    det  <- get_path_details(g_multimodal, cmb$s, cmb$e)
    rows[[length(rows)+1]] <- .mk("multimodal", cmb$type, cmb$l1_mode, cmb$l3_mode,
                                  cmb$l1, l2, cmb$l3, prio, near_bus_rt_type, near_bus_cl_rt_type, det)
  }
  if (!length(rows)) return(NULL)
  bind_rows(rows)
}

# -----------------------------------------------------------------------------
# .r13_select_trip() -- re-select the best path from captured candidates under a
#   given (metro line-change distance, tolerance band). Mirrors
#   select_best_by_pct_tolerance (filter total<=min*(1+tol), arrange prio,total)
#   AND the metro-only-vs-multimodal final_df stage (L7176-7186). At metro_xfer=50,
#   tol=TOL_PCT this returns the certified best_total_m. Returns the selected row
#   (1 candidate) + best_mode + best_total_m for downstream time computation.
# -----------------------------------------------------------------------------
.r13_select_trip <- function(cd, metro_xfer_m = 50, tol_pct = NULL) {
  if (is.null(tol_pct)) tol_pct <- if (exists("TOL_PCT")) TOL_PCT else 0.10
  if (is.null(cd) || !nrow(cd)) return(list(best_total_m=NA_real_, best_mode=NA_character_, sel=NULL))
  cd$l2_adj    <- cd$l2 + cd$n_metro_tr * (metro_xfer_m - 50)
  cd$total_adj <- cd$l1 + cd$l2_adj + cd$l3
  mm <- cd[cd$branch == "multimodal", , drop=FALSE]
  mo <- cd[cd$branch == "metro_only", , drop=FALSE]

  best_mm <- NULL
  if (nrow(mm)) {
    ok <- !is.na(mm$total_adj)
    if (any(ok)) {
      mm <- mm[ok, , drop=FALSE]
      thr <- min(mm$total_adj) * (1 + tol_pct)
      mm  <- mm[mm$total_adj <= thr, , drop=FALSE]
      mm  <- mm[order(mm$prio, mm$total_adj), , drop=FALSE]
      best_mm <- mm[1, , drop=FALSE]
    }
  }
  tot_mo  <- if (nrow(mo)) mo$total_adj[1] else NA_real_
  min_tot <- if (!is.null(best_mm)) best_mm$total_adj[1] else NA_real_

  if (is.na(tot_mo) && is.na(min_tot)) {
    return(list(best_total_m=NA_real_, best_mode=NA_character_, sel=NULL))
  } else if (is.na(min_tot)) {
    return(list(best_total_m=tot_mo, best_mode="Metro-only", sel=mo[1, , drop=FALSE]))
  } else if (is.na(tot_mo)) {
    return(list(best_total_m=min_tot, best_mode="Multimodal", sel=best_mm))
  }
  fdf <- data.frame(mode=c("Metro-only","Multimodal"),
                    total=c(tot_mo, min_tot), prio=c(10, best_mm$prio[1]))
  thr <- min(fdf$total) * (1 + tol_pct)
  fdf <- fdf[fdf$total <= thr, , drop=FALSE]
  fdf <- fdf[order(fdf$prio, fdf$total), , drop=FALSE]
  bm  <- fdf$mode[1]; bt <- fdf$total[1]
  list(best_total_m=bt, best_mode=bm, sel=if (bm == "Metro-only") mo[1, , drop=FALSE] else best_mm)
}

# -----------------------------------------------------------------------------
# r13_enh_capture(tag, asg_key, t1_cache) -- build the unified worklist (base
#   anchors from .ct_* keys + new anchors from the Table-1 assignments), capture
#   all candidate combos per trip, save Data/R13_combos_<tag>.rds, and run the
#   REPRODUCTION-VALIDATION GATE (re-select at baseline must match certified
#   results_df$best_total_m for the deterministic anchors).
# -----------------------------------------------------------------------------
r13_enh_capture <- function(tag, asg_key, t1_cache, out_dir = NULL) {
  ge  <- globalenv()
  msg <- function(...) message(sprintf("[R1-3-ENH %s] %s", tag, sprintf(...)))
  if (is.null(out_dir)) out_dir <- if (exists("base_dir", envir=ge)) get("base_dir", envir=ge) else getwd()
  # CACHE GUARD: the candidate capture is the expensive part (~hours, routes every
  # candidate). If R13_combos_<tag>.rds already exists, reuse it (set FORCE_R13_COMBOS=1
  # to recompute) — lets the post-run recompute regenerate sweeps/bounds cheaply.
  .combos_f <- file.path(out_dir, sprintf("R13_combos_%s.rds", tag))
  if (file.exists(.combos_f) && Sys.getenv("FORCE_R13_COMBOS") != "1") {
    msg("cache guard: loading existing %s (skip capture; FORCE_R13_COMBOS=1 to recompute).", basename(.combos_f))
    return(invisible(readRDS(.combos_f)))
  }
  if (!exists("results_df", envir=ge)) { msg("results_df missing; skipping."); return(invisible(NULL)) }
  results_df <- get("results_df", envir=ge)

  # L3-by-clinic helper for the NEW anchors (mirrors get_L3_from_lookup; 14b copy)
  .L3_from_clinic <- function(clinic_id, transit_type, is_private) {
    if (transit_type == "metro") {
      if (!(clinic_id %in% names(.l3m_geo))) return(NA_real_)
      net <- .l3m_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      ratio <- if (is_private) ratio_L3_metro_priv else ratio_L3_metro_pub
      return(.l3m_geo[[clinic_id]] * ratio * 1000)
    } else {
      if (!(clinic_id %in% names(.l3b_geo))) return(NA_real_)
      net <- .l3b_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      ratio <- if (is_private) ratio_L3_bus_priv else ratio_L3_bus_pub
      return(.l3b_geo[[clinic_id]] * ratio * 1000)
    }
  }

  cap <- list()

  # ============================ BASE anchors ============================
  # Enumerate (rp_id, dest_type) from the certified clinic_targets keys; L1/L3 via
  # the loop's lookup fns; road via the loop's imputation logic (L7077-7088).
  base_keys <- names(.ct_clinic_id)
  msg("capturing BASE anchors: %d (point,anchor) keys ...", length(base_keys))
  for (.tk in base_keys) {
    parts     <- strsplit(.tk, "\\|\\|", perl=TRUE)[[1]]
    rp_id     <- parts[1]; dest_type <- parts[2]
    .dkey     <- .ct_dest_geo[[.tk]]
    if (is.null(.dkey) || !(.dkey %in% names(.cl_metro_node))) next
    if (!(rp_id %in% names(.rp_metro_node))) next
    L1_metro_m <- get_L1_from_lookup(rp_id, "metro", L1_metro_lookup, L1_bus_lookup, ratio_lookup)
    L1_bus_m   <- get_L1_from_lookup(rp_id, "bus",   L1_metro_lookup, L1_bus_lookup, ratio_lookup)
    L3_metro_m <- get_L3_from_lookup(rp_id, dest_type, "metro", clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub)
    L3_bus_m   <- get_L3_from_lookup(rp_id, dest_type, "bus",   clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub)
    road_dist_m <- NA_real_
    if (.tk %in% names(.rc_net)) {
      val_km <- .rc_net[[.tk]]
      imp_factor <- if (grepl("priv", dest_type)) {
        if (grepl("nearest", dest_type)) ratio_lookup["nearest_priv"] else ratio_lookup["median_priv"]
      } else {
        if (grepl("nearest", dest_type)) ratio_lookup["nearest_pub"] else ratio_lookup["median_pub"]
      }
      if (is.na(imp_factor)) imp_factor <- 1.3
      if (is.na(val_km) || is.infinite(val_km)) val_km <- .rc_geo[[.tk]] * imp_factor
      road_dist_m <- val_km * 1000
    }
    cap[[length(cap)+1]] <- .r13_capture_one(
      rp_id, dest_type, road_dist_m, L1_metro_m, L1_bus_m, L3_metro_m, L3_bus_m,
      .rp_metro_node[[rp_id]], .rp_bus_node[[rp_id]], .rp_bus_rt[[rp_id]],
      .cl_metro_node[[.dkey]], .cl_bus_node[[.dkey]], .cl_bus_rt[[.dkey]])
  }

  # ============================ NEW anchors =============================
  # farthest_* (deterministic, 1 clinic/pt) + random_* (N draws/pt), from the
  # SAME Table-1 draws used by 14b. road via the 14b pooled-ratio logic.
  if (file.exists(t1_cache)) {
    .t1  <- readRDS(t1_cache)
    .asg <- .t1$assignments[[asg_key]]
    if (!is.null(.asg)) {
      worklist <- bind_rows(
        .asg$far %>% transmute(rp_id=as.character(id), dest_type, dest_id_geo, road_net_km=net_km_p2t, road_geo_km=geo_km),
        .asg$rnd %>% transmute(rp_id=as.character(id), dest_type, dest_id_geo, road_net_km=net_km_p2t, road_geo_km=geo_km)
      ) %>% filter(rp_id %in% names(.rp_metro_node))
      .ar  <- worklist %>% group_by(dest_type) %>%
        summarise(r = mean(road_net_km, na.rm=TRUE)/mean(road_geo_km, na.rm=TRUE), .groups="drop")
      .arv <- setNames(.ar$r, .ar$dest_type)
      worklist <- worklist %>% mutate(
        clinic_id = gsub("^(priv_|pub_)", "", dest_id_geo),
        .imp = unname(.arv[dest_type]),
        .imp = ifelse(is.na(.imp) | is.infinite(.imp), 1.3, .imp),
        road_dist_m = ifelse(!is.na(road_net_km) & !is.infinite(road_net_km), road_net_km*1000, road_geo_km*.imp*1000))
      msg("capturing NEW anchors: %d (point,clinic,draw) rows ...", nrow(worklist))
      uids <- unique(worklist$rp_id)
      .l1m <- setNames(vapply(uids, function(u) get_L1_from_lookup(u, "metro", L1_metro_lookup, L1_bus_lookup, ratio_lookup), numeric(1)), uids)
      .l1b <- setNames(vapply(uids, function(u) get_L1_from_lookup(u, "bus",   L1_metro_lookup, L1_bus_lookup, ratio_lookup), numeric(1)), uids)
      for (k in seq_len(nrow(worklist))) {
        u  <- worklist$rp_id[k]; dt <- worklist$dest_type[k]; dk <- worklist$dest_id_geo[k]
        cid <- worklist$clinic_id[k]; is_priv <- grepl("priv", dt)
        if (!(dk %in% names(.cl_metro_node))) next
        cc <- .r13_capture_one(
          u, dt, worklist$road_dist_m[k], .l1m[[u]], .l1b[[u]],
          .L3_from_clinic(cid, "metro", is_priv), .L3_from_clinic(cid, "bus", is_priv),
          .rp_metro_node[[u]], .rp_bus_node[[u]], .rp_bus_rt[[u]],
          .cl_metro_node[[dk]], .cl_bus_node[[dk]], .cl_bus_rt[[dk]])
        if (!is.null(cc)) { cc$clinic_id <- cid; cap[[length(cap)+1]] <- cc }
      }
    } else msg("assignment key '%s' missing in %s; new anchors skipped.", asg_key, t1_cache)
  } else msg("Table-1 cache %s not found; new anchors skipped.", t1_cache)

  combos <- bind_rows(cap)
  if (!"clinic_id" %in% names(combos)) combos$clinic_id <- NA_character_
  out_f <- file.path(out_dir, sprintf("R13_combos_%s.rds", tag))
  saveRDS(combos, out_f)
  msg("captured %d candidate rows across %d trips -> %s",
      nrow(combos), nrow(dplyr::distinct(combos, rp_id, dest_type, clinic_id)), basename(out_f))

  # ---- REPRODUCTION VALIDATION GATE: baseline re-selection vs certified results_df ----
  # Deterministic anchors only (nearest/median/farthest priv+pub -> 1 row/(pt,anchor)
  # in results_df). random_* excluded (per-draw clinic not stored in results_df).
  det_anchors <- c("nearest_priv","median_priv","farthest_priv",
                   "nearest_pub","median_pub","farthest_pub")
  vc <- combos %>% filter(dest_type %in% det_anchors)
  if (nrow(vc)) {
    sel <- vc %>% group_by(rp_id, dest_type) %>% group_modify(~{
      r <- .r13_select_trip(.x, metro_xfer_m=50, tol_pct=if (exists("TOL_PCT")) TOL_PCT else 0.10)
      tibble::tibble(best_total_m = r$best_total_m)
    }) %>% ungroup()
    ref <- results_df %>% filter(dest_type %in% det_anchors) %>%
      transmute(rp_id=as.character(rp_id), dest_type, ref=best_total_m)
    cmp <- sel %>% mutate(rp_id=as.character(rp_id)) %>% inner_join(ref, by=c("rp_id","dest_type"))
    if (nrow(cmp)) {
      d <- abs(cmp$best_total_m - cmp$ref)
      both_na <- is.na(cmp$best_total_m) & is.na(cmp$ref)
      one_na  <- xor(is.na(cmp$best_total_m), is.na(cmp$ref))
      msg("[validate] baseline re-selection vs certified best_total_m: max|delta| = %.3e m over %d trips (NA-mismatch=%d, both-NA=%d) -- must be ~0",
          max(d, na.rm=TRUE), nrow(cmp), sum(one_na), sum(both_na))
    } else msg("[validate] no overlap with results_df for validation.")
  }
  invisible(combos)
}

# -----------------------------------------------------------------------------
# .r13_reselect() -- VECTORISED re-selection of the best path per trip under
#   (metro line-change distance, tolerance band, priority scheme). Two stages,
#   both expressed as the SAME tolerance-slice (filter total<=min*(1+tol),
#   arrange by (prio,total), take row 1): stage 1 picks the best multimodal among
#   <=4 candidates; stage 2 picks among {metro-only(prio 10), best-multimodal}.
#   Reproduces the certified selection at (metro_xfer=50, tol=TOL_PCT, default).
#   Group key = (rp_id,dest_type,clinic_id) so random draws stay distinct.
# -----------------------------------------------------------------------------
.r13_reselect <- function(combos, metro_xfer_m = 50, tol_pct = 0.10, prio_scheme = "default") {
  cb <- combos
  cb$total_adj <- cb$l1 + (cb$l2 + cb$n_metro_tr * (metro_xfer_m - 50)) + cb$l3
  cb$prio_use  <- if (prio_scheme == "total_only") 0L else cb$prio
  gk <- c("rp_id","dest_type","clinic_id")
  mm <- cb %>% dplyr::filter(branch == "multimodal", !is.na(total_adj)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gk))) %>%
    dplyr::mutate(.thr = min(total_adj) * (1 + tol_pct)) %>%
    dplyr::filter(total_adj <= .thr) %>%
    dplyr::arrange(prio_use, total_adj, .by_group = TRUE) %>%
    dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::mutate(prio_final = prio)
  mo <- cb %>% dplyr::filter(branch == "metro_only") %>% dplyr::mutate(prio_final = 10)
  finalists <- dplyr::bind_rows(mo, mm)            # mo FIRST -> stable tie-break favours metro-only
  sel <- finalists %>% dplyr::group_by(dplyr::across(dplyr::all_of(gk))) %>%
    dplyr::mutate(.thr2 = min(total_adj, na.rm = TRUE) * (1 + tol_pct)) %>%
    dplyr::filter(total_adj <= .thr2) %>%
    dplyr::arrange(prio_final, total_adj, .by_group = TRUE) %>%
    dplyr::slice(1) %>% dplyr::ungroup()
  sel$best_mode    <- ifelse(sel$branch == "metro_only", "Metro-only", "Multimodal")
  sel$best_total_m <- sel$total_adj
  sel
}

# -----------------------------------------------------------------------------
# .r13_path_time() -- WALK-initiated travel time (min) of each selected path at
#   traffic speed s under params P. Mirrors the certified physics: metro-only ride
#   = calc_agg_metro_speed on the L2 ride (transfer walk carved at metro_xfer);
#   multimodal metro = precise segment time (sel$metro_pre); BRT at calc_brt_speed,
#   std bus at s; waits from path-type + per-type counts (cheap-pass L60-64);
#   dwell = P$dwell*(n_stops-1); per-type transfer penalties added. L1 walked, L3
#   walked. (init="car" would drive L1 + parking; break-even uses walk-init only.)
# -----------------------------------------------------------------------------
.r13_path_time <- function(sel, P, s, init = "walk") {
  walk <- P$walk
  bg <- if (is.null(P$brt_gap)) 0.5 else P$brt_gap        # BRT congestion-escape (R1-3 sweep)
  bc <- if (is.null(P$brt_cap)) 80  else P$brt_cap
  tmlt <- if (is.null(P$transfer_mult)) 1 else P$transfer_mult
  l1km <- sel$l1/1000; l3km <- sel$l3/1000
  t_l1 <- if (init == "car") l1km/s*60 + P$parking else l1km/walk*60
  t_l3 <- l3km/walk*60
  # metro-only
  l2mo     <- sel$l2/1000
  carve_mo <- pmin(sel$n_metro_tr * P$metro_xfer_m/1000, l2mo)
  ride_mo  <- pmax(l2mo - carve_mo, 0)
  t_mo <- ride_mo/calc_agg_metro_speed(ride_mo)*60 + carve_mo/walk*60 +
          (sel$n_metro_tr + 1) * P$metro_wait_mean +
          P$dwell * pmax(0, sel$n_stops - 1) + sel$n_metro_tr * P$pen_metro_tr
  # multimodal: metro line-changes (n_metro_tr) scaled by metro_xfer; other transfers
  # (mode-switch / bus-bus actual walk) scaled by transfer_mult; baseline -> d_walk.
  walk_mm <- pmax(pmax(sel$d_walk - sel$n_metro_tr*50, 0)*tmlt + sel$n_metro_tr*P$metro_xfer_m, 0)/1000
  n_mb <- ifelse(sel$type %in% c("Metro-Metro","Metro-Bus"), 1 + sel$n_metro_tr,
            ifelse(sel$type == "Bus-Metro", sel$n_mode_sw + sel$n_metro_tr, 0))
  n_bb <- ifelse(sel$type == "Bus-Bus", 1 + sel$n_bus_tr,
            ifelse(sel$type == "Metro-Bus", sel$n_mode_sw + sel$n_bus_tr,
            ifelse(sel$type == "Bus-Metro", 1 + sel$n_bus_tr, 0)))
  t_mm <- sel$metro_pre + (sel$d_brt/1000)/pmin(s + bg*(bc - s), bc)*60 + (sel$d_std/1000)/s*60 +
          walk_mm/walk*60 + n_mb*P$metro_wait_mean + n_bb*P$bus_wait_mean +
          P$dwell * pmax(0, sel$n_stops - 1) +
          sel$n_metro_tr*P$pen_metro_tr + sel$n_bus_tr*P$pen_bus_tr + sel$n_mode_sw*P$pen_mode_sw
  t_transit <- ifelse(sel$best_mode == "Metro-only", t_mo,
                ifelse(sel$best_mode == "Multimodal", t_mm, NA_real_))
  t_l1 + t_l3 + t_transit
}

# anchor-level mean travel-time table (Car-only car-init + Best transit walk-init)
# across the speed grid -> feeds r13_breakeven.
.r13_enh_dsum <- function(sel, P, speeds, amap) {
  rows <- vector("list", length(speeds)); i <- 0
  for (s in speeds) {
    i <- i + 1
    tt_w <- .r13_path_time(sel, P, s, "walk")
    tt_c <- .r13_path_time(sel, P, s, "car")   # car-initiated (drive L1 + parking) — manuscript Fig 3/4 basis
    car  <- (sel$road_dist_m/1000)/s*60 + P$parking
    agg <- data.frame(dest_type=sel$dest_type, tt_w=tt_w, tt_c=tt_c, car=car) %>%
      dplyr::group_by(dest_type) %>%
      dplyr::summarise(BestW=mean(tt_w, na.rm=TRUE), BestC=mean(tt_c, na.rm=TRUE), Car=mean(car, na.rm=TRUE), .groups="drop") %>%
      dplyr::left_join(amap, by="dest_type")
    rows[[i]] <- dplyr::bind_rows(
      agg %>% dplyr::transmute(Type,Target,Speed=s,Mode_family="Car-only (direct to clinic)",Initiation="Car-initiated",Time_mean=Car),
      agg %>% dplyr::transmute(Type,Target,Speed=s,Mode_family="Best",Initiation="Walk-initiated",Time_mean=BestW),
      agg %>% dplyr::transmute(Type,Target,Speed=s,Mode_family="Best",Initiation="Car-initiated", Time_mean=BestC))
  }
  dplyr::bind_rows(rows)
}

# per-anchor competitiveness@s_ref: walk-init best transit < car (parks), over the
# reachable set (finite best_total_m).
.r13_enh_compet <- function(sel, P, s_ref = 40) {
  tt  <- .r13_path_time(sel, P, s_ref, "walk")
  car <- (sel$road_dist_m/1000)/s_ref*60 + P$parking
  reach <- !is.na(sel$best_total_m) & !is.na(tt)
  data.frame(anchor=sel$dest_type, wins=ifelse(reach, tt < car, NA)) %>%
    dplyr::group_by(anchor) %>%
    dplyr::summarise(compet40_pct=100*mean(wins, na.rm=TRUE), .groups="drop")
}

# -----------------------------------------------------------------------------
# r13_enh_orchestrate(tag) -- OFAT sweeps that the cheap pass could NOT do:
#   metro line-change 50->200m + combo-priority + TOL_PCT (all RE-SELECT the path)
#   and the per-type transfer-penalty split (time-only). Traffic speed (5-80) is
#   the break-even axis. Writes R13_enh_sensitivity_<tag>.rds + sweep/tornado docx.
#   Reuses r13_breakeven + .fin_* from _r13_orchestrate.R (sourced; no side effects).
# -----------------------------------------------------------------------------
r13_enh_orchestrate <- function(tag, out_suffix = tag, out_dir = NULL, asg_key = NULL, t1_cache = NULL) {
  ge  <- globalenv(); msg <- function(...) message(sprintf("[R1-3-ENH %s] %s", tag, sprintf(...)))
  if (is.null(out_dir)) out_dir <- if (exists("base_dir", envir=ge)) get("base_dir", envir=ge) else getwd()
  combos_f <- file.path(out_dir, sprintf("R13_combos_%s.rds", tag))
  if (!file.exists(combos_f)) { msg("combos cache %s missing; run r13_enh_capture first.", basename(combos_f)); return(invisible(NULL)) }
  combos <- readRDS(combos_f)
  if (!"clinic_id" %in% names(combos)) combos$clinic_id <- NA_character_
  combos$metro_pre <- vapply(combos$seg_str, process_metro_string, numeric(1))  # precise metro time per candidate

  spd <- if (exists("speeds", envir=ge)) get("speeds", envir=ge) else seq(5,80,1)
  g <- function(n,d) if (exists(n,envir=ge)) get(n,envir=ge) else d
  BASE <- list(walk=g("speed_walk",4), metro_wait_mean=g("metro_wait_max",7.5)/2,
               bus_wait_mean=g("bus_wait_max",20)/2, dwell=g("stop_penalty_min",0.5),
               parking=0, pen_metro_tr=0, pen_bus_tr=0, pen_mode_sw=0,
               transfer_mult=1, brt_gap=g("brt_gap_factor",0.5), brt_cap=g("brt_speed_cap",80),
               metro_xfer_m=50, tol_pct=g("TOL_PCT",0.10), prio_scheme="default")
  amap <- tibble::tribble(~dest_type,~Type,~Target,
    "nearest_priv","Private","Nearest","median_priv","Private","Specific","farthest_priv","Private","Farthest","random_priv","Private","Random",
    "nearest_pub","Public","Nearest","median_pub","Public","Specific","farthest_pub","Public","Farthest","random_pub","Public","Random") %>%
    dplyr::filter(dest_type %in% combos$dest_type)

  .selcache <- new.env()
  get_sel <- function(mx, tl, pr) {
    k <- paste(mx, tl, pr, sep="|"); v <- get0(k, .selcache)
    if (is.null(v)) { v <- .r13_reselect(combos, mx, tl, pr); assign(k, v, .selcache) }
    v
  }
  eval_cfg <- function(dim, scen, sel, P) {
    dsum <- .r13_enh_dsum(sel, P, spd, amap)
    be <- amap %>% dplyr::rowwise() %>%
      dplyr::mutate(be_best_walk = r13_breakeven(dsum, Type, Target, "Best", "Walk-initiated"),
                    be_best_car  = r13_breakeven(dsum, Type, Target, "Best", "Car-initiated")) %>% dplyr::ungroup()
    comp <- .r13_enh_compet(sel, P)
    be %>% dplyr::left_join(comp, by=c("dest_type"="anchor")) %>%
      dplyr::transmute(dimension=dim, scenario=scen, anchor=dest_type, be_best_walk, be_best_car, compet40_pct)
  }

  out <- list()
  for (mx in c(50,100,150,200)) { P<-BASE; P$metro_xfer_m<-mx
    out[[length(out)+1]] <- eval_cfg("metro_xfer", sprintf("xfer=%gm",mx), get_sel(mx,BASE$tol_pct,BASE$prio_scheme), P) }
  for (tl in c(0.05,0.10,0.20,0.30)) { P<-BASE; P$tol_pct<-tl
    out[[length(out)+1]] <- eval_cfg("tol_pct", sprintf("tol=%g",tl), get_sel(BASE$metro_xfer_m,tl,BASE$prio_scheme), P) }
  for (pr in c("default","total_only")) {
    out[[length(out)+1]] <- eval_cfg("priority", pr, get_sel(BASE$metro_xfer_m,BASE$tol_pct,pr), BASE) }
  sel0 <- get_sel(BASE$metro_xfer_m, BASE$tol_pct, BASE$prio_scheme)
  pen <- list(none=c(0,0,0), moderate=c(1,2,3), high=c(2,4,6))  # (metro_line, bus_bus, mode_switch) min
  for (nm in names(pen)) { v<-pen[[nm]]; P<-BASE; P$pen_metro_tr<-v[1]; P$pen_bus_tr<-v[2]; P$pen_mode_sw<-v[3]
    out[[length(out)+1]] <- eval_cfg("transfer_penalty", nm, sel0, P) }
  res <- dplyr::bind_rows(out)

  # ---- Monte-Carlo SD/SE over the 10 random facility draws (R1-5 follow-up) ----
  if (exists("r13_enh_se_random") && !is.null(t1_cache) && !is.null(asg_key) && file.exists(t1_cache)) {
    dm <- tryCatch(r13_build_draw_map(t1_cache, asg_key), error=function(e) NULL)
    if (!is.null(dm)) {
      se <- tryCatch(r13_enh_se_random(combos, dm, BASE, spd, amap), error=function(e){ msg("SE-over-draws failed: %s", conditionMessage(e)); NULL })
      if (!is.null(se)) {
        saveRDS(se, file.path(out_dir, sprintf("R13_enh_random_SE_%s.rds", out_suffix)))
        msg("random-draw SD/SE: %s", paste(sprintf("%s be=%.2f±%.2f compet=%.2f±%.2f", se$summary$dest_type, se$summary$be_mean, se$summary$be_se, se$summary$compet_mean, se$summary$compet_se), collapse=" | "))
        if (requireNamespace("flextable",quietly=TRUE) && requireNamespace("officer",quietly=TRUE)) {
          suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
          flextable::save_as_docx(flextable::autofit(flextable::theme_booktabs(flextable::flextable(as.data.frame(
            dplyr::mutate(se$summary, dplyr::across(dplyr::where(is.numeric), ~round(.x,3))))))) |>
            flextable::add_header_lines(sprintf("R1-3 random-anchor Monte-Carlo SD/SE over %d draws (%s)", max(se$per_draw$draw,na.rm=TRUE), tag)),
            path=file.path(out_dir, sprintf("R13_enh_random_SE_%s.docx", out_suffix)))
          msg("wrote R13_enh_random_SE_%s.{rds,docx}", out_suffix)
        }
      }
    }
  }

  tornado <- res %>% dplyr::group_by(dimension, anchor) %>%
    dplyr::summarise(be_min=.fin_min(be_best_walk), be_max=.fin_max(be_best_walk),
                     be_swing=.fin_swing(be_best_walk),
                     compet_min=.fin_min(compet40_pct), compet_max=.fin_max(compet40_pct),
                     n_be_finite=sum(is.finite(be_best_walk)), .groups="drop") %>%
    dplyr::arrange(anchor, dplyr::desc(be_swing))
  msg("sweep complete: %d (dimension x scenario x anchor) rows", nrow(res))
  msg("tornado (break-even swing by dimension, summed over anchors):")
  print(as.data.frame(tornado %>% dplyr::group_by(dimension) %>%
          dplyr::summarise(total_be_swing=round(sum(be_swing,na.rm=TRUE),2),
                           anchors_with_be=sum(!is.na(be_swing)), .groups="drop") %>%
          dplyr::arrange(dplyr::desc(total_be_swing))), row.names=FALSE)

  saveRDS(list(sweep=res, tornado=tornado, base=BASE),
          file.path(out_dir, sprintf("R13_enh_sensitivity_%s.rds", out_suffix)))
  msg("wrote R13_enh_sensitivity_%s.rds", out_suffix)
  if (requireNamespace("flextable",quietly=TRUE) && requireNamespace("officer",quietly=TRUE)) {
    suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
    rnd <- function(d) d %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x,2)))
    save_as_docx(flextable(as.data.frame(rnd(res))) %>% theme_booktabs() %>% autofit() %>%
      add_header_lines(sprintf("R1-3 ENHANCED sweeps (%s): break-even (vs walk-init transit) + competitiveness@40", tag)),
      path=file.path(out_dir, sprintf("R13_enh_sensitivity_sweep_%s.docx", out_suffix)))
    save_as_docx(flextable(as.data.frame(rnd(tornado))) %>% theme_booktabs() %>% autofit() %>%
      add_header_lines(sprintf("R1-3 ENHANCED tornado (%s): break-even swing by assumption", tag)),
      path=file.path(out_dir, sprintf("R13_enh_tornado_%s.docx", out_suffix)))
    msg("wrote R13_enh_sensitivity_sweep_%s.docx + R13_enh_tornado_%s.docx", out_suffix, out_suffix)
  }
  invisible(res)
}
invisible(NULL)
