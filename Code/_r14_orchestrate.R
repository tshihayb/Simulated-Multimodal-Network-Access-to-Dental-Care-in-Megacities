# =============================================================================
#  _r14_orchestrate.R  --  R1-4 / R2-3f orchestration (sourced, not standalone)
# -----------------------------------------------------------------------------
#  Sourced by the RUN_R14-gated hooks placed inside block 17b of Section 18
#  (unweighted) and Section 19 (weighted) of `Analysis clean actual road
#  distance.R`.  Because source() evaluates in the calling (global) environment,
#  this file sees the section's in-scope objects:
#      df_analysis, df_sum, .tt_anchor_summaries, .breakeven, speeds, tm,
#      rp_comparison (unweighted) / weighted comparison, the random-point sf,
#      riyadh_regions, and the R1-5 caches (table1_new_anchors_N<N>.rds,
#      sample_test_results[_weighted]_newanchors_N<N>.rds).
#  It is purely additive: writes new *_R14_* files only; certified outputs
#  untouched.  Default OFF (RUN_R14 unset) so normal/certified runs skip it.
#
#  Method:
#   * Track 1 (MAR road legs) DIRECT point->facility network distance compared
#     under COMPLETE-CASE / RATIO / PMM-MI (engine = _r14_imputation.R).
#   * Car-only travel time is LINEAR in road distance, so mean car time under
#     each method = (that method's mean network distance) / speed * 60.  The
#     transit curves (Metro-only / Multimodal) are held at the certified values
#     from .tt_anchor_summaries (they depend on L1/L2/L3, not the direct leg);
#     break-even = where the method's car curve crosses the transit curve.
#   * Track 2 (MNAR non-reachable) reported via denominator bounds, not imputed.
# =============================================================================

# ---- locate + source the engine (same dir as this file) ---------------------
local({
  .self <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  .dir  <- if (!is.null(.self)) dirname(.self) else file.path(
            "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities", "Code")
  if (!exists("r14_build_leg_table"))
    source(file.path(.dir, "_r14_imputation.R"))
})

# -----------------------------------------------------------------------------
# r14_breakeven_from_means()
#   Break-even traffic speed for a single anchor under one imputation method,
#   using the LINEARITY of car time in distance.
#     mean_netdist_km : mean direct point->facility network distance (this method)
#     transit_curve   : data.frame(Speed, Time_mean) for the transit mode
#                       (certified, from .tt_anchor_summaries; Car-initiated)
#   Returns the speed (km/h) at which mean car time == mean transit time, via the
#   same approx() crossing rule as the pipeline's .breakeven.  NA if no crossing.
# -----------------------------------------------------------------------------
r14_breakeven_from_means <- function(mean_netdist_km, transit_curve) {
  tc <- transit_curve[order(transit_curve$Speed), , drop = FALSE]
  if (nrow(tc) < 2 || is.na(mean_netdist_km)) return(NA_real_)
  car_t <- (mean_netdist_km / tc$Speed) * 60           # tm(km, s) on the mean
  d <- car_t - tc$Time_mean                            # >0 => car slower (transit faster)
  if (all(d > 0, na.rm = TRUE) || all(d < 0, na.rm = TRUE)) return(NA_real_)
  tryCatch(stats::approx(x = d, y = tc$Speed, xout = 0, ties = mean)$y,
           error = function(e) NA_real_)
}

# -----------------------------------------------------------------------------
# r14_attach_origin_region()
#   Add origin_x / origin_y / region to a per-point table keyed by `id`, using
#   the projected random-point sf and the region polygons.  Region via st_join
#   (point-in-polygon); origins via st_coordinates.  Robust to CRS mismatch.
# -----------------------------------------------------------------------------
r14_attach_origin_region <- function(ids, points_sf, regions_sf,
                                     region_col = "new_region") {
  suppressWarnings(suppressMessages({ library(sf); library(dplyr) }))
  pts <- points_sf %>% mutate(id = as.character(id))
  xy  <- sf::st_coordinates(pts)
  pts$origin_x <- xy[, 1]; pts$origin_y <- xy[, 2]
  reg <- regions_sf
  if (!is.na(sf::st_crs(pts)) && !is.na(sf::st_crs(reg)) &&
      sf::st_crs(pts) != sf::st_crs(reg)) reg <- sf::st_transform(reg, sf::st_crs(pts))
  reg2 <- reg %>% transmute(region = as.character(.data[[region_col]]))
  joined <- suppressWarnings(sf::st_join(pts, reg2, join = sf::st_intersects, left = TRUE))
  out <- joined %>% sf::st_drop_geometry() %>%
    distinct(id, .keep_all = TRUE) %>%
    transmute(id, origin_x, origin_y,
              region = ifelse(is.na(region), "Unknown", region))
  out[match(as.character(ids), out$id), c("origin_x","origin_y","region")]
}

# -----------------------------------------------------------------------------
# r14_orchestrate()
#   Main entry. `tag` = "unweighted" | "weighted"; `out_suffix` decorates output
#   filenames.  Reads in-scope objects from the global env; writes deliverables.
# -----------------------------------------------------------------------------
r14_orchestrate <- function(tag, out_suffix = tag,
                            n_mice = NULL, out_dir = NULL) {
  suppressWarnings(suppressMessages({ library(dplyr) }))
  ge <- globalenv()
  msg <- function(...) message(sprintf("[R1-4 %s] %s", tag, sprintf(...)))

  # ---- required in-scope objects (defensive: clear stop if missing) ----------
  need <- c("df_analysis", ".tt_anchor_summaries", "N_RANDOM_DRAWS")
  miss <- need[!vapply(need, function(o) exists(o, envir = ge), logical(1))]
  if (length(miss)) { msg("MISSING objects: %s -- skipping.", paste(miss, collapse=", ")); return(invisible(NULL)) }
  df_analysis        <- get("df_analysis", envir = ge)
  tt_anchor_summaries<- get(".tt_anchor_summaries", envir = ge)
  N <- get("N_RANDOM_DRAWS", envir = ge)
  if (is.null(out_dir)) out_dir <- if (exists("base_dir", envir=ge)) get("base_dir",envir=ge) else getwd()
  if (is.null(n_mice)) {
    n_mice <- suppressWarnings(as.integer(Sys.getenv("N_MICE", "20")))
    if (is.na(n_mice) || n_mice < 2L) n_mice <- 20L
  }

  design_key <- if (tag == "weighted") "Population-weighted" else "Uniform in populated districts only"

  # ---- 1. assemble the DIRECT-leg table (net_km_p2t with NA) ------------------
  #   New anchors (farthest/random): from the Table-1 cache assignments.
  #   Originals (nearest/median):    from the section's comparison object.
  .t1_cache <- file.path(out_dir, sprintf("table1_new_anchors_N%d.rds", N))
  if (!file.exists(.t1_cache)) { msg("Table-1 cache missing (%s); skipping.", .t1_cache); return(invisible(NULL)) }
  .asg <- readRDS(.t1_cache)$assignments[[design_key]]
  if (is.null(.asg)) { msg("assignments[[%s]] missing in cache; skipping.", design_key); return(invisible(NULL)) }

  new_leg <- bind_rows(
    .asg$far %>% transmute(id = as.character(id), anchor = dest_type, geo_km, net_km = net_km_p2t),
    .asg$rnd %>% transmute(id = as.character(id), anchor = dest_type, geo_km, net_km = net_km_p2t)
  )
  # originals comparison: rp_comparison (unweighted) or a weighted twin if present.
  # Try section-appropriate names in order; first that exists with the right cols wins.
  comp_candidates <- if (tag == "weighted")
    c("rp_comparison_weighted","rp_comparison_w","weighted_rp_comparison","rp_comparison")
  else c("rp_comparison")
  comp_obj <- NULL
  for (nm in comp_candidates) {
    if (is.null(comp_obj) && exists(nm, envir = ge)) {
      cand <- get(nm, envir = ge)
      if (all(c("dest_type","geo_km","net_km_p2t") %in% names(cand))) comp_obj <- cand
    }
  }

  orig_dts <- c("nearest_priv","median_priv","nearest_pub","median_pub")
  orig_leg <- NULL
  if (!is.null(comp_obj)) {
    orig_leg <- comp_obj %>% filter(dest_type %in% orig_dts) %>%
      transmute(id = as.character(id), anchor = dest_type, geo_km, net_km = net_km_p2t)
  } else {
    msg("originals comparison not found for this section; 3-method table covers new anchors only.")
  }

  leg_raw <- bind_rows(orig_leg, new_leg)
  leg_raw$ownership <- ifelse(grepl("priv", leg_raw$anchor), "private", "public")

  # origins + region from the section's random-point sf
  pts_obj <- NULL
  for (nm in c("rp_sample","rp_all","rp_sample_weighted")) if (is.null(pts_obj) && exists(nm, envir=ge)) pts_obj <- get(nm, envir=ge)
  reg_obj <- if (exists("riyadh_regions", envir=ge)) get("riyadh_regions", envir=ge) else NULL
  if (!is.null(pts_obj) && !is.null(reg_obj)) {
    oxr <- r14_attach_origin_region(leg_raw$id, pts_obj, reg_obj)
    leg_raw$origin_x <- oxr$origin_x; leg_raw$origin_y <- oxr$origin_y; leg_raw$region <- oxr$region
  } else {
    msg("points/regions sf not found; PMM predictors fall back to geo only.")
    leg_raw$origin_x <- 0; leg_raw$origin_y <- 0; leg_raw$region <- "All"
  }

  tbl <- r14_build_leg_table(leg_raw$net_km, leg_raw$geo_km, leg_raw$origin_x,
                             leg_raw$origin_y, leg_raw$ownership, leg_raw$anchor,
                             leg_raw$region, leg = "direct")

  # ---- 2. adaptive M + 3-method network-distance comparison -------------------
  shares <- tbl %>% group_by(anchor) %>% summarise(s = 100*mean(is_missing), .groups="drop")
  M <- max(n_mice, r14_adaptive_m(shares$s))
  msg("imputed shares by anchor: %s", paste(sprintf("%s=%.1f%%", shares$anchor, shares$s), collapse="  "))
  msg("running PMM-MI with M=%d, donors=5 ...", M)
  pmm  <- r14_pmm_fill(tbl, m = M, donors = 5L)
  comp3 <- r14_network_distance_3method(tbl, pmm)
  msg("3-method network-distance comparison:")
  print(as.data.frame(comp3), digits = 4, row.names = FALSE)

  # ---- 3. travel-time + break-even under the 3 methods (linear car side) ------
  anchor_meta <- list(
    nearest_priv=c("Private","Nearest"), median_priv=c("Private","Specific"),
    farthest_priv=c("Private","Farthest"), random_priv=c("Private","Random"),
    nearest_pub=c("Public","Nearest"), median_pub=c("Public","Specific"),
    farthest_pub=c("Public","Farthest"), random_pub=c("Public","Random"))
  be <- lapply(comp3$anchor, function(a) {
    meta <- anchor_meta[[a]]; if (is.null(meta)) return(NULL)
    if (!(a %in% df_analysis$dest_type)) return(NULL)
    cur <- tt_anchor_summaries(df_analysis %>% filter(dest_type == a), meta[1], meta[2])
    tc_metro <- cur %>% filter(Mode_family=="Metro-only",  Initiation=="Car-initiated") %>% select(Speed, Time_mean)
    tc_multi <- cur %>% filter(Mode_family=="Multimodal",  Initiation=="Car-initiated") %>% select(Speed, Time_mean)
    row <- comp3[comp3$anchor == a, ]
    mk <- function(md) data.frame(
      anchor=a, method=md,
      mean_netdist_km = switch(md, complete_case=row$complete_case, ratio=row$ratio, pmm=row$pmm),
      breakeven_metro = r14_breakeven_from_means(switch(md, complete_case=row$complete_case, ratio=row$ratio, pmm=row$pmm), tc_metro),
      breakeven_multi = r14_breakeven_from_means(switch(md, complete_case=row$complete_case, ratio=row$ratio, pmm=row$pmm), tc_multi),
      stringsAsFactors=FALSE)
    bind_rows(mk("complete_case"), mk("ratio"), mk("pmm"))
  })
  be_tab <- bind_rows(be)
  if (nrow(be_tab)) { msg("break-even speeds by anchor x method (km/h):"); print(as.data.frame(be_tab), digits=4, row.names=FALSE) }

  # ---- 4. accounting (C) + denominator bounds (R2-3f) from results ------------
  res_obj <- NULL
  for (nm in c("results_df_clean","results_df")) if (is.null(res_obj) && exists(nm, envir=ge)) res_obj <- get(nm, envir=ge)
  acct <- NULL; den <- NULL
  if (!is.null(res_obj) && all(c("rp_id","dest_type","best_total_m") %in% names(res_obj))) {
    # per-(point,anchor) DIRECT-leg imputed flag — from leg_raw (it carries `id`;
    # the leg table `tbl` does NOT retain the point id). Collapse random draws.
    miss_direct <- leg_raw %>%
      group_by(id, anchor) %>%
      summarise(imp_direct = any(is.na(net_km) | is.infinite(net_km)), .groups = "drop") %>%
      transmute(rp_id = as.character(id), anchor, imp_direct)
    # L1 access-leg imputed share (point-level): nearest-metro L1 missing in lookup. Guarded.
    imp_l1_map <- NULL
    if (exists("L1_metro_lookup", envir = ge)) {
      .L1m <- get("L1_metro_lookup", envir = ge)
      if (all(c("id","L1_metro_net_km") %in% names(.L1m)))
        imp_l1_map <- .L1m %>% transmute(rp_id = as.character(id),
                                         imp_L1 = is.na(L1_metro_net_km) | is.infinite(L1_metro_net_km))
    }
    has_same <- "metro_same_stn" %in% names(res_obj)
    has_road <- "road_dist_m"    %in% names(res_obj)
    base <- res_obj %>% transmute(
      design = tag, anchor = as.character(dest_type), rp_id = as.character(rp_id),
      same_station  = if (has_same) coalesce(as.logical(metro_same_stn), FALSE) else FALSE,
      # Option A: GENUINE non-reachability excludes same-station trips, which are
      # "too close / walk-preferred" (origin & clinic share the access node, so
      # both transit chains are undefined -> not a transit-desert).
      non_reachable = is.na(best_total_m) & !same_station,
      too_close     = is.na(best_total_m) & same_station,
      best_total_m  = best_total_m,
      road_dist_m   = if (has_road) road_dist_m else NA_real_)
    acct_df <- base %>% left_join(miss_direct, by = c("rp_id","anchor"))
    if (!is.null(imp_l1_map)) {
      acct_df <- acct_df %>% left_join(imp_l1_map, by = "rp_id") %>%
        mutate(imp_L1 = coalesce(imp_L1, FALSE))
    } else { acct_df$imp_L1 <- NA }
    acct_df <- acct_df %>% mutate(imp_direct = coalesce(imp_direct, FALSE), imp_L3 = FALSE)
    acct <- r14_accounting_table(acct_df)
    msg("accounting (C) — imputation & reachability shares:"); print(as.data.frame(acct), digits=3, row.names=FALSE)

  } else {
    msg("results object not found; accounting table skipped this section.")
  }

  # ---- 4b. MNAR / truncation-by-death: TIME-based competitiveness + bounds +
  #          tipping point (R2-3f).  transit_wins = best walk-initiated transit
  #          time < car drive time at S_REF; non-reachable (best_total_m==NA) is
  #          UNDEFINED -> composite estimand counts it as transit-loses, the
  #          conditional estimand drops it, Manski brackets it, tipping quantifies
  #          robustness.  Per-trip times mirror the certified .rng_mcse formula. --
  den <- NULL; tip_tab <- NULL
  .have <- function(v) exists(v, envir = ge)
  if (all(vapply(c("tm","split_legs","speed_walk","metro_wait_max",
                   "stop_penalty_min","calc_brt_speed"), .have, logical(1)))) {
    S_REF <- 40
    tmf <- get("tm", ge); slf <- get("split_legs", ge); sw <- get("speed_walk", ge)
    mwm <- get("metro_wait_max", ge); spn <- get("stop_penalty_min", ge); brt <- get("calc_brt_speed", ge)
    .pertrip <- function(d) {
      ml <- slf(d$chain_metro_km, d$pct_metro_l1, d$pct_metro_l2)
      xl <- slf(d$chain_multi_km, d$pct_multi_l1, d$pct_multi_l2)
      t_metro <- ifelse(!is.na(d$chain_metro_km),
        tmf(pmax(ml$l1,0), sw) + tmf(pmax(ml$l2,0), S_REF) + tmf(pmax(ml$l3,0), sw) +
        (pmax(0L, as.integer(d$metro_only_transfers))+1L)*(mwm/2), NA_real_)
      t_multi <- ifelse(!is.na(d$chain_multi_km),
        tmf(pmax(xl$l1,0), sw) +
        ((d$dist_brt_km/brt(S_REF))*60 + (d$dist_std_km/S_REF)*60 + d$metro_time_precise_min +
         (d$multi_transfer_walk_km/sw)*60) +
        tmf(pmax(xl$l3,0), sw) + spn*d$multi_dwell, NA_real_)
      t_best <- pmin(t_metro, t_multi, na.rm = TRUE); t_best[is.infinite(t_best)] <- NA_real_
      car <- (d$road_dist_km / S_REF) * 60
      data.frame(dest_type = d$dest_type,
                 same_station = if ("metro_same_stn" %in% names(d)) coalesce(as.logical(d$metro_same_stn), FALSE) else FALSE,
                 non_reachable = is.na(d$best_total_m),
                 transit_wins  = ifelse(is.na(t_best), NA, t_best < car))
    }
    comp_df <- .pertrip(df_analysis %>% filter(dest_type %in% comp3$anchor)) %>%
      transmute(design = tag, anchor = dest_type, same_station,
                nr_raw = non_reachable, transit_wins) %>%
      # Option A: drop "too close" trips (NA & same-station = walk-preferred, outside
      # the transit estimand); remaining NA trips are GENUINELY non-reachable.
      filter(!(nr_raw & same_station)) %>%
      mutate(non_reachable = nr_raw & !same_station) %>%
      select(design, anchor, non_reachable, transit_wins)
    den <- r14_denominator_bounds(comp_df)
    tip_tab <- bind_rows(lapply(sort(unique(comp_df$anchor)), function(a) {
      s  <- comp_df %>% filter(anchor == a)
      tp <- r14_tipping_point(n_compet  = sum(s$transit_wins & !s$non_reachable, na.rm = TRUE),
                              n_reach   = sum(!s$non_reachable),
                              n_nonreach= sum(s$non_reachable))
      data.frame(anchor = a, conditional_pct = 100*tp$conditional,
                 composite_pct = 100*tp$composite, manski_upper_pct = 100*tp$manski_upper,
                 non_reachable_pct = 100*tp$n_nonreach/(tp$n_reach+tp$n_nonreach),
                 tipping_delta_pct = 100*tp$tipping_delta, note = tp$interpretation,
                 stringsAsFactors = FALSE)
    }))
    msg("competitiveness @%d km/h — conditional (reachable-only) vs composite (non-reachable=fail) vs Manski upper:", S_REF)
    print(as.data.frame(tip_tab), digits = 3, row.names = FALSE)
  } else {
    msg("travel-time constants/helpers not in scope; competitiveness + tipping skipped.")
  }

  # ---- 5. persist deliverables (rds always; docx if officer present) ----------
  res <- list(network_distance_3method = comp3, breakeven_3method = be_tab,
              accounting = acct, denominator_bounds = den,
              competitiveness_tipping = tip_tab,
              imputed_shares = shares, M = M, tag = tag)
  rds_path <- file.path(out_dir, sprintf("R14_imputation_sensitivity_%s.rds", out_suffix))
  saveRDS(res, rds_path); msg("wrote %s", rds_path)
  if (requireNamespace("flextable", quietly=TRUE) && requireNamespace("officer", quietly=TRUE)) {
    suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
    .wx <- function(df, path, ttl) if (!is.null(df) && nrow(df)) {
      ft <- flextable(as.data.frame(df)) %>% theme_booktabs() %>% autofit() %>%
        add_header_lines(values = ttl)
      save_as_docx(ft, path = path)
      msg("wrote %s", path)
    }
    .wx(comp3,  file.path(out_dir, sprintf("R14_network_distance_3method_%s.docx", out_suffix)),
        sprintf("Network distance by imputation method (%s): complete-case / ratio / PMM-MI", tag))
    .wx(be_tab, file.path(out_dir, sprintf("R14_breakeven_3method_%s.docx", out_suffix)),
        sprintf("Break-even speed by imputation method (%s)", tag))
    .wx(acct,   file.path(out_dir, sprintf("R14_accounting_%s.docx", out_suffix)),
        sprintf("Imputation & reachability accounting (%s)", tag))
    .wx(den,    file.path(out_dir, sprintf("R14_denominator_bounds_%s.docx", out_suffix)),
        sprintf("Transit competitiveness: reachable-only vs all-trips (%s)", tag))
    .wx(tip_tab, file.path(out_dir, sprintf("R14_competitiveness_tipping_%s.docx", out_suffix)),
        sprintf("Competitiveness estimands + Manski bounds + tipping point (%s)", tag))
  }
  invisible(res)
}

invisible(NULL)
