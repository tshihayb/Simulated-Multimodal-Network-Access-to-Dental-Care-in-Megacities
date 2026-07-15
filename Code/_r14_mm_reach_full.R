# =============================================================================
#  _r14_mm_reach_full.R  --  FULL correction for spurious multimodal
#  non-reachability (R2-3f). Constrained-graph re-route.
# -----------------------------------------------------------------------------
#  WHY a NEW approach (vs _r14_mm_reach_fix.R, the SAMPLE diagnostic):
#  The certified multimodal router `.impl_l2r` takes the SINGLE shortest path on
#  g_multimodal and REJECTS it (NA = "non-reachable through transit") whenever it
#  violates the transfer rules (forbidden first/last transfer, or two consecutive
#  transfers) WITHOUT searching for the shortest VALID path. The diagnostic fix
#  used igraph::k_shortest_paths to walk paths until a valid one appears, but that
#  is ~1 s/call on the full graph, so it could only ESTIMATE a recovery rate from
#  a per-anchor SAMPLE (and random was skipped).
#
#  THE FULL FIX (this file): build a transfer-STATE node-split graph in which the
#  transfer rules are STRUCTURALLY impossible to violate, so a SINGLE Dijkstra
#  (igraph::shortest_paths) returns the minimum-weight VALID path directly -- ~ms
#  per call. That is fast enough to re-route EVERY non-reachable trip across ALL
#  8 anchors (nearest/median/farthest/random x priv/pub), BOTH weightings, and
#  regenerate a corrected Table 2.
#
#  EXACTNESS: the certified router selects by edge `weight` and reports a MASKED
#  `real_distance` (first/last metro_metro_transfer edges are free). k_shortest
#  walks paths in increasing weight order and returns the first valid one -> the
#  minimum-weight VALID path. The node-split graph, minimising the SAME weight
#  over ONLY valid paths, returns the SAME minimum-weight valid path (proof: any
#  valid path lighter than the one found would have been enumerated earlier and
#  rejected -> contradiction). On REACHABLE trips the single shortest path is
#  already valid, so the node-split result == `.impl_l2r` exactly (validation
#  gate). We only ever USE the node-split router on trips the certified router
#  rejected (NA); reachable trips keep their certified values untouched.
#
#  This file defines FUNCTIONS ONLY (no side effects on source). Certified
#  routing/objects are never modified; the driver writes NEW files only and runs
#  behind the RUN_MM_FULL gate, inside the pipeline (graphs + lookups live).
# =============================================================================
suppressWarnings(suppressMessages({ library(igraph) }))

# ---- transfer-rule vocabulary (verbatim from .impl_l2r, Analysis L6834-6841) --
.R14_FORBIDDEN_TR <- c("bus_bus_exact", "metro_bus", "metro_bus_manual",
                       "bus_bus_proximity", "bus_bus_standard")
.R14_MM_TR        <- "metro_metro_transfer"
.R14_ALL_TR       <- c(.R14_MM_TR, .R14_FORBIDDEN_TR)

# TRUE if an edge-type sequence satisfies the certified transfer rules.
.r14_path_valid <- function(e_types) {
  n <- length(e_types)
  if (n == 0) return(FALSE)
  if (e_types[1] %in% .R14_FORBIDDEN_TR) return(FALSE)            # forbidden FIRST transfer
  if (e_types[n] %in% .R14_FORBIDDEN_TR) return(FALSE)            # forbidden LAST transfer
  is_tr <- e_types %in% .R14_ALL_TR
  if (n > 1 && any(is_tr & c(FALSE, is_tr[-n]))) return(FALSE)    # CONSECUTIVE transfers
  TRUE
}

# Masked real distance of an edge path in the ORIGINAL graph g (mirrors
# .impl_l2r / .r14_masked_dist: drop first/last metro_metro_transfer cost).
.r14_masked_dist <- function(ep, graph) {
  if (length(ep) == 0) return(NA_real_)
  e_types <- E(graph)$edge_type[ep]
  e_dists <- as.numeric(E(graph)$real_distance[ep])
  n <- length(ep); cost_mask <- rep(TRUE, n)
  if (e_types[1] == .R14_MM_TR) cost_mask[1] <- FALSE
  if (n > 1 && e_types[n] == .R14_MM_TR) cost_mask[n] <- FALSE
  sum(e_dists[cost_mask], na.rm = TRUE)
}

# Path-details on a KNOWN-VALID original edge sequence -- byte-faithful copy of
# the metric block of `.impl_pd` (Analysis L6918-6965), skipping the (redundant)
# re-routing + validation because `ep` is already the shortest VALID path.
.r14_pd_from_edges <- function(edge_seq, graph) {
  res <- list(n_transfers=0, n_bus_transfers=0, n_metro_transfers=0, n_mode_switches=0,
              n_bus_routes=0, n_metro_lines=0, n_stops=0, has_brt=FALSE,
              dist_metro_m=0, dist_brt_m=0, dist_bus_std_m=0, dist_walk_transfer_m=0, seg_str_metro="")
  if (length(edge_seq) == 0) return(res)
  e_types  <- E(graph)$edge_type[edge_seq]
  e_dists  <- suppressWarnings(as.numeric(E(graph)$real_distance[edge_seq]))
  e_routes <- E(graph)$route_code[edge_seq]

  count_mask <- rep(TRUE, length(e_types))
  if (e_types[1] == .R14_MM_TR) count_mask[1] <- FALSE
  if (length(e_types) > 1 && e_types[length(e_types)] == .R14_MM_TR) count_mask[length(e_types)] <- FALSE

  res$n_metro_transfers <- sum(e_types == .R14_MM_TR & count_mask)
  res$n_bus_transfers   <- sum(e_types %in% c("bus_bus_exact", "bus_bus_proximity", "bus_bus_standard"))
  res$n_mode_switches   <- sum(e_types %in% c("metro_bus", "metro_bus_manual"))
  res$n_transfers       <- res$n_metro_transfers + res$n_bus_transfers + res$n_mode_switches

  keys <- E(graph)$unique_route_key[edge_seq]
  res$n_bus_routes  <- dplyr::n_distinct(keys[e_types == "bus_route" & !is.na(keys)])
  res$n_metro_lines <- dplyr::n_distinct(keys[e_types == "metro_line" & !is.na(keys)])
  if (e_types[1] == .R14_MM_TR && res$n_metro_lines > 1) res$n_metro_lines <- res$n_metro_lines - 1
  if (length(e_types) > 1 && e_types[length(e_types)] == .R14_MM_TR && res$n_metro_lines > 1) res$n_metro_lines <- res$n_metro_lines - 1

  res$n_stops <- sum(e_types %in% c("metro_line", "bus_route"))

  mask_metro <- e_types == "metro_line"
  mask_bus   <- e_types == "bus_route"
  mask_walk  <- (e_types %in% .R14_ALL_TR) & count_mask
  safe_routes <- as.character(e_routes); mask_brt_route <- safe_routes %in% c("11", "12", "13")
  mask_brt_leg <- mask_bus & mask_brt_route; mask_std_leg <- mask_bus & !mask_brt_route

  res$dist_metro_m         <- sum(e_dists[mask_metro],   na.rm=TRUE)
  res$dist_brt_m           <- sum(e_dists[mask_brt_leg], na.rm=TRUE)
  res$dist_bus_std_m       <- sum(e_dists[mask_std_leg], na.rm=TRUE)
  res$dist_walk_transfer_m <- sum(e_dists[mask_walk],    na.rm=TRUE)
  if (any(mask_metro)) {
    metro_segments <- e_dists[mask_metro]; metro_segments <- metro_segments[!is.na(metro_segments)]
    if (length(metro_segments) > 0) res$seg_str_metro <- paste(round(metro_segments, 1), collapse = ";")
  }
  res$has_brt <- any(mask_brt_leg)
  res
}

# =============================================================================
#  .r14_build_valid_graph(g) -- transfer-STATE node-split of g_multimodal.
#  Each original node x is split into 4 state-copies encoding "what edge did I
#  arrive on", so the transfer rules become structural:
#     x@2  = START state (no edge taken yet)
#     x@0  = arrived via a TRAVEL edge (metro_line / bus_route)
#     x@1m = arrived via a metro_metro_transfer (allowed as a terminal edge)
#     x@1f = arrived via a FORBIDDEN transfer   (NOT allowed as a terminal edge)
#  Edges (original u--e-->w, weight ew):
#     travel e : {u@2,u@0,u@1m,u@1f} -> w@0        (reset transfer flag)
#     mm e     : {u@2,u@0}           -> w@1m        (not from @1m/@1f = consecutive)
#     forb e   : {u@0}               -> w@1f        (not from @2 = forbidden FIRST,
#                                                    not from @1m/@1f = consecutive)
#  A query routes from s@2 to the cheaper of {e@0, e@1m} (NOT e@1f -> forbidden
#  LAST). Every valid path is representable and every representable path is valid.
#  orig_eid on each split edge maps back to the original edge id in g.
# =============================================================================
.r14_build_valid_graph <- function(g) {
  el <- igraph::as_edgelist(g, names = TRUE)          # E x 2, edge-id order
  et <- as.character(E(g)$edge_type)
  ew <- as.numeric(E(g)$weight)
  u <- el[, 1]; w <- el[, 2]

  is_mm   <- et == .R14_MM_TR
  is_forb <- et %in% .R14_FORBIDDEN_TR
  is_trav <- !(is_mm | is_forb)                       # metro_line, bus_route
  if (any(is_trav & (et %in% .R14_ALL_TR)))
    stop(".r14_build_valid_graph: edge classification overlap (unexpected edge_type).")

  ti <- which(is_trav); mi <- which(is_mm); fi <- which(is_forb)

  # travel: from all 4 states -> w@0
  st <- c("@2", "@0", "@1m", "@1f")
  df_trav <- data.frame(
    from   = paste0(rep(u[ti], times = 4L), rep(st, each = length(ti))),
    to     = paste0(rep(w[ti], times = 4L), "@0"),
    weight = rep(ew[ti], times = 4L),
    orig_eid = rep(ti, times = 4L), stringsAsFactors = FALSE)
  # metro_metro: from @2 and @0 -> w@1m
  df_mm <- data.frame(
    from   = paste0(rep(u[mi], times = 2L), rep(c("@2", "@0"), each = length(mi))),
    to     = paste0(rep(w[mi], times = 2L), "@1m"),
    weight = rep(ew[mi], times = 2L),
    orig_eid = rep(mi, times = 2L), stringsAsFactors = FALSE)
  # forbidden: from @0 -> w@1f
  df_fb <- data.frame(
    from   = paste0(u[fi], "@0"),
    to     = paste0(w[fi], "@1f"),
    weight = ew[fi],
    orig_eid = fi, stringsAsFactors = FALSE)

  edf <- rbind(df_trav, df_mm, df_fb)
  # declare ALL 4V state-nodes up front so queries never error on an edgeless state
  vdf <- data.frame(name = as.vector(t(outer(V(g)$name, st, paste0))), stringsAsFactors = FALSE)
  gv <- igraph::graph_from_data_frame(edf, directed = TRUE, vertices = vdf)
  igraph::E(gv)$weight   <- edf$weight
  igraph::E(gv)$orig_eid <- edf$orig_eid
  gv
}

# =============================================================================
#  .r14_route_source(s_name, e_names, g, gv) -- shortest VALID multimodal path
#  from ONE source access node s to MANY target access nodes (single Dijkstra).
#  Returns a named list keyed by e_name: list(dist = masked real distance or NA,
#  epath = original edge-id sequence or NULL).
# =============================================================================
.r14_route_source <- function(s_name, e_names, g, gv) {
  e_names <- unique(as.character(e_names))
  out <- vector("list", length(e_names)); names(out) <- e_names
  src <- paste0(s_name, "@2")
  vn <- V(gv)$name
  if (!(src %in% vn)) {                                # no non-forbidden first edge -> none valid
    for (e in e_names) out[[e]] <- list(dist = NA_real_, epath = NULL)
    return(out)
  }
  tgt0 <- paste0(e_names, "@0"); tgt1 <- paste0(e_names, "@1m")
  targets <- c(tgt0, tgt1)
  ok <- targets %in% vn                                # some terminal states may be edgeless
  sp_ep <- vector("list", length(targets))
  if (any(ok)) {
    sp <- suppressWarnings(igraph::shortest_paths(
      gv, from = src, to = targets[ok],
      weights = igraph::E(gv)$weight, output = "epath"))
    sp_ep[ok] <- sp$epath
  }
  ewv <- igraph::E(gv)$weight; eov <- igraph::E(gv)$orig_eid
  n <- length(e_names)
  for (i in seq_len(n)) {
    ep0 <- sp_ep[[i]]; ep1 <- sp_ep[[i + n]]
    wt0 <- if (!is.null(ep0) && length(ep0)) sum(ewv[ep0]) else Inf
    wt1 <- if (!is.null(ep1) && length(ep1)) sum(ewv[ep1]) else Inf
    if (!is.finite(wt0) && !is.finite(wt1)) { out[[i]] <- list(dist = NA_real_, epath = NULL); next }
    ep <- if (wt0 <= wt1) ep0 else ep1
    orig <- eov[ep]
    out[[i]] <- list(dist = .r14_masked_dist(orig, g), epath = orig)
  }
  out
}

# Single-pair convenience (mainly for tests / the validation gate).
.r14_l2_valid <- function(s_name, e_name, g, gv) {
  if (is.na(s_name) || is.na(e_name)) return(NA_real_)
  if (!s_name %in% V(g)$name || !e_name %in% V(g)$name) return(NA_real_)
  if (s_name == e_name) return(0)
  .r14_route_source(s_name, e_name, g, gv)[[1]]$dist
}

# =============================================================================
#  r14_mm_reach_full(tag) -- DRIVER for the full Table-2 correction.
#  Re-routes EVERY multimodal-non-reachable trip (all 8 anchors incl. random,
#  both weightings) with the shortest-VALID-path node-split router, PATCHES the
#  augmented results_df's multimodal fields for the recovered trips (reachable
#  trips are left byte-identical), rebuilds a CORRECTED Table 2 (ALL8 + NEW4),
#  and writes a per-anchor before/after non-reachability summary.
#  Reproduction-safe: reads in-scope objects, writes NEW files only.
#  Runs behind the RUN_MM_FULL gate, inside the pipeline (graphs + lookups live).
# =============================================================================
r14_mm_reach_full <- function(tag = "unweighted", include_random = TRUE,
                              out_dir = NULL, rdf = NULL, write_tables = TRUE,
                              emit_combos = TRUE, emit_results = TRUE) {
  ge <- globalenv()
  msg <- function(...) message(sprintf("[MM-FULL %s] %s", tag, sprintf(...)))
  suppressWarnings(suppressMessages({ library(dplyr); library(igraph) }))
  gv_get <- function(nm) if (exists(nm, envir = ge)) get(nm, envir = ge) else NULL

  if (is.null(out_dir)) out_dir <- if (!is.null(gv_get("base_dir"))) gv_get("base_dir") else getwd()

  # ---- resolve the in-scope objects the correction needs -------------------
  need <- c("g_multimodal", ".rp_metro_node", ".rp_bus_node", ".cl_metro_node", ".cl_bus_node",
            ".rp_bus_rt", ".cl_bus_rt", ".ct_dest_geo", "get_L1_from_lookup", "ratio_lookup",
            "L1_metro_lookup", "L1_bus_lookup", "get_combo_priority", "select_best_by_pct_tolerance",
            "check_on_route_via_distances", "TOL_PCT",
            ".l3m_geo", ".l3m_net", ".l3b_geo", ".l3b_net",
            "ratio_L3_metro_priv", "ratio_L3_metro_pub", "ratio_L3_bus_priv", "ratio_L3_bus_pub")
  miss <- need[!vapply(need, exists, logical(1), envir = ge)]
  if (length(miss)) { msg("missing in-scope objects (%s); run inside the pipeline. skip.", paste(miss, collapse=",")); return(invisible(NULL)) }
  g   <- gv_get("g_multimodal")
  rpM <- gv_get(".rp_metro_node"); rpB <- gv_get(".rp_bus_node")
  clM <- gv_get(".cl_metro_node"); clB <- gv_get(".cl_bus_node")
  rpRT <- gv_get(".rp_bus_rt");    clRT <- gv_get(".cl_bus_rt")
  ctDG <- gv_get(".ct_dest_geo")
  get_L1 <- gv_get("get_L1_from_lookup"); ratio_lookup <- gv_get("ratio_lookup")
  L1m <- gv_get("L1_metro_lookup"); L1b <- gv_get("L1_bus_lookup")
  gcp <- gv_get("get_combo_priority"); sbt <- gv_get("select_best_by_pct_tolerance")
  cor_route <- gv_get("check_on_route_via_distances"); TOLP <- gv_get("TOL_PCT")
  l3m_geo<-gv_get(".l3m_geo"); l3m_net<-gv_get(".l3m_net"); l3b_geo<-gv_get(".l3b_geo"); l3b_net<-gv_get(".l3b_net")
  rL3mp<-gv_get("ratio_L3_metro_priv"); rL3mpub<-gv_get("ratio_L3_metro_pub")
  rL3bp<-gv_get("ratio_L3_bus_priv");   rL3bpub<-gv_get("ratio_L3_bus_pub")

  # L3 by clinic (mirrors get_L3_from_lookup / .L3_from_clinic exactly)
  L3_clinic <- function(clinic_id, transit_type, is_private) {
    if (transit_type == "metro") {
      if (!(clinic_id %in% names(l3m_geo))) return(NA_real_)
      net <- l3m_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      return(l3m_geo[[clinic_id]] * (if (is_private) rL3mp else rL3mpub) * 1000)
    } else {
      if (!(clinic_id %in% names(l3b_geo))) return(NA_real_)
      net <- l3b_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      return(l3b_geo[[clinic_id]] * (if (is_private) rL3bp else rL3bpub) * 1000)
    }
  }

  if (is.null(rdf)) rdf <- gv_get("results_df")
  if (is.null(rdf)) { msg("results_df not found; skip."); return(invisible(NULL)) }
  rdf$rp_id <- as.character(rdf$rp_id)

  # ---- attach dkey (dest_id_geo) to every results_df row -------------------
  orig4 <- c("nearest_priv", "median_priv", "nearest_pub", "median_pub")
  rdf$dkey <- NA_character_
  is_o <- rdf$dest_type %in% orig4
  rdf$dkey[is_o] <- unname(ctDG[ paste0(rdf$rp_id[is_o], "||", rdf$dest_type[is_o]) ])

  N <- if (!is.null(gv_get("N_RANDOM_DRAWS"))) as.integer(gv_get("N_RANDOM_DRAWS")) else 10L
  t1 <- file.path(out_dir, sprintf("table1_new_anchors_N%d.rds", N))
  asg <- if (file.exists(t1)) readRDS(t1)$assignments[[if (tag=="weighted") "Population-weighted" else "Uniform in populated districts only"]] else NULL
  new_present <- FALSE
  if (!is.null(asg)) {
    wl_new <- bind_rows(
      if (!is.null(asg$far)) asg$far %>% transmute(rp_id=as.character(id), dest_type, dkey=dest_id_geo) else NULL,
      if (include_random && !is.null(asg$rnd)) asg$rnd %>% transmute(rp_id=as.character(id), dest_type, dkey=dest_id_geo) else NULL)
    for (dt in unique(wl_new$dest_type)) {
      idx <- which(rdf$dest_type == dt)
      wl_dt <- wl_new %>% filter(dest_type == dt)
      if (length(idx) != nrow(wl_dt)) stop(sprintf("[MM-FULL] row-count mismatch for %s: results_df=%d worklist=%d", dt, length(idx), nrow(wl_dt)))
      if (!all(rdf$rp_id[idx] == as.character(wl_dt$rp_id)))
        stop(sprintf("[MM-FULL] positional misalignment for %s (rp_id order differs) -- cannot map draws safely.", dt))
      rdf$dkey[idx] <- wl_dt$dkey
      new_present <- TRUE
    }
  }

  # ---- identify the non-reachable trips to re-route ------------------------
  patch_anchors <- c(orig4, "farthest_priv", "farthest_pub")
  if (include_random) patch_anchors <- c(patch_anchors, "random_priv", "random_pub")
  nr <- which(is.na(rdf$multi_total_m) & rdf$mm_same_acc == FALSE &
              rdf$dest_type %in% patch_anchors & !is.na(rdf$dkey))
  msg("results_df %d rows; %d multimodal-non-reachable trips to re-route (anchors: %s)%s.",
      nrow(rdf), length(nr), paste(patch_anchors, collapse=","),
      if (!include_random) " [random EXCLUDED]" else " [random INCLUDED]")
  if (!length(nr)) { msg("no non-reachable trips; nothing to correct."); return(invisible(NULL)) }

  s_metro <- unname(rpM[rdf$rp_id[nr]]); s_bus <- unname(rpB[rdf$rp_id[nr]])
  e_metro <- unname(clM[rdf$dkey[nr]]);  e_bus <- unname(clB[rdf$dkey[nr]])

  # ---- collect unique (s,e) combos to route (certified skip rules) ---------
  inv_mb <- (e_bus == s_bus); inv_bm <- (e_metro == s_metro)
  mk_pair <- function(s, e, extra) data.frame(s=s, e=e,
      skip = is.na(s) | is.na(e) | (s==e) | (extra %in% TRUE), stringsAsFactors=FALSE)
  pairs <- bind_rows(
    mk_pair(s_metro, e_metro, FALSE),
    mk_pair(s_metro, e_bus,   inv_mb),
    mk_pair(s_bus,   e_metro, inv_bm),
    mk_pair(s_bus,   e_bus,   FALSE))
  uniq <- pairs %>% filter(!skip) %>% distinct(s, e)
  msg("building node-split valid graph + routing %d unique (s,e) pairs across %d sources ...",
      nrow(uniq), dplyr::n_distinct(uniq$s))
  t0 <- Sys.time()
  gv <- .r14_build_valid_graph(g)
  L2map <- new.env(hash=TRUE, parent=emptyenv()); EPmap <- new.env(hash=TRUE, parent=emptyenv())
  srcs <- unique(uniq$s); nsrc <- length(srcs)
  for (si in seq_len(nsrc)) {
    s <- srcs[si]
    if (si %% 200 == 0) msg("  routed %d / %d sources ...", si, nsrc)
    es <- uniq$e[uniq$s == s]
    rr <- .r14_route_source(s, es, g, gv)
    for (e in names(rr)) { k <- paste0(s, "||", e); L2map[[k]] <- rr[[e]]$dist; EPmap[[k]] <- rr[[e]]$epath }
  }
  msg("routing done (%.1f min).", as.numeric(Sys.time()-t0, units="mins"))

  # ---- per-trip recompute of the multimodal fields (pure R; routing cached) --
  getL2 <- function(s, e) { if (is.na(s)||is.na(e)) return(NA_real_); v <- L2map[[paste0(s,"||",e)]]; if (is.null(v)) NA_real_ else v }
  getEP <- function(s, e) EPmap[[paste0(s,"||",e)]]
  PDcache <- new.env(hash=TRUE, parent=emptyenv())   # path-details per (s,e); same regardless of trip
  getPD <- function(s, e) { k <- paste0(s,"||",e); v <- PDcache[[k]]; if (!is.null(v)) return(v)
    r <- .r14_pd_from_edges(getEP(s, e), g); PDcache[[k]] <- r; r }
  L1cache <- new.env(hash=TRUE, parent=emptyenv())
  L1of <- function(u, mode) { k <- paste0(mode,"|",u); v <- L1cache[[k]]; if (!is.null(v)) return(v); r <- get_L1(u, mode, L1m, L1b, ratio_lookup); L1cache[[k]] <- r; r }

  recov <- 0L
  patch <- vector("list", length(nr))
  combos_patch <- vector("list", length(nr))   # recovered trips' valid multimodal candidates in R13_combos schema
  for (ii in seq_along(nr)) {
    ri <- nr[ii]
    u  <- rdf$rp_id[ri]; dt <- rdf$dest_type[ri]; dk <- rdf$dkey[ri]
    road <- rdf$road_dist_m[ri]; tot_mo <- rdf$metro_only_total_m[ri]
    is_priv <- grepl("priv", dt); clinic_id <- gsub("^(priv_|pub_)", "", dk)
    cid_combos <- if (dt %in% orig4) NA_character_ else clinic_id   # R13_combos convention: NA for base anchors, clinic id for new
    L1_metro <- L1of(u, "metro"); L1_bus <- L1of(u, "bus")
    L3_metro <- L3_clinic(clinic_id, "metro", is_priv); L3_bus <- L3_clinic(clinic_id, "bus", is_priv)
    sm <- s_metro[ii]; sb <- s_bus[ii]; em <- e_metro[ii]; eb <- e_bus[ii]
    rp_rt <- unname(rpRT[u]); cl_rt <- unname(clRT[dk])
    imb <- (eb == sb); ibm <- (em == sm)
    combos <- list(
      list(type="Metro-Metro", l1=L1_metro, l3=L3_metro, l1_mode="metro", l3_mode="metro", s=sm, e=em, skip=FALSE),
      list(type="Metro-Bus",   l1=L1_metro, l3=L3_bus,   l1_mode="metro", l3_mode="bus",   s=sm, e=eb, skip=isTRUE(imb)),
      list(type="Bus-Metro",   l1=L1_bus,   l3=L3_metro, l1_mode="bus",   l3_mode="metro", s=sb, e=em, skip=isTRUE(ibm)),
      list(type="Bus-Bus",     l1=L1_bus,   l3=L3_bus,   l1_mode="bus",   l3_mode="bus",   s=sb, e=eb, skip=FALSE))
    crows <- list(); ccombos <- list()
    for (cmb in combos) {
      if (cmb$skip) next
      if (is.na(cmb$s) || is.na(cmb$e) || cmb$s == cmb$e) next
      l2 <- getL2(cmb$s, cmb$e); if (is.na(l2)) next
      prio_c <- gcp(l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, l1_rt=rp_rt, l3_rt=cl_rt)
      crows[[length(crows)+1]] <- data.frame(type=cmb$type, l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode,
        s=cmb$s, e=cmb$e, l1=cmb$l1, l2=l2, l3=cmb$l3, total=cmb$l1+l2+cmb$l3,
        prio=prio_c, stringsAsFactors=FALSE)
      # ---- emit an R13_combos multimodal candidate row (schema-faithful) ----
      pdc <- getPD(cmb$s, cmb$e)
      ccombos[[length(ccombos)+1]] <- data.frame(
        rp_id=u, dest_type=dt, road_dist_m=road, branch="multimodal", type=cmb$type,
        l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, l1=cmb$l1, l2=l2, l3=cmb$l3, prio=prio_c,
        l1_rt=rp_rt, l3_rt=cl_rt,
        d_metro=pdc$dist_metro_m, d_brt=pdc$dist_brt_m, d_std=pdc$dist_bus_std_m, d_walk=pdc$dist_walk_transfer_m,
        n_metro_tr=pdc$n_metro_transfers, n_bus_tr=pdc$n_bus_transfers, n_mode_sw=pdc$n_mode_switches,
        n_stops=pdc$n_stops, seg_str=ifelse(is.null(pdc$seg_str_metro),"",pdc$seg_str_metro),
        clinic_id=cid_combos, stringsAsFactors=FALSE)
    }
    combos_patch[[ii]] <- if (length(ccombos)) bind_rows(ccombos) else NULL
    combos_df <- bind_rows(crows); best_row <- sbt(combos_df, tol_pct = TOLP)
    min_tot <- NA_real_; best_prio <- NA_integer_; bc <- NULL
    if (!is.null(best_row) && nrow(best_row) == 1) {
      min_tot <- best_row$total[1]; best_prio <- best_row$prio[1]
      bc <- as.list(best_row[1, ])
    }
    p <- list(.rid = ri,
      multi_total_m = min_tot, multi_path_type = if (is.null(bc)) NA_character_ else bc$type,
      mm_dist_metro_m = 0, mm_dist_brt_m = 0, mm_dist_std_m = 0, mm_dist_walk_m = 0, mm_metro_segments = "",
      multi_L1_mode = if (is.null(bc)) NA_character_ else bc$l1_mode,
      multi_L1_m = if (is.null(bc)) NA_real_ else bc$l1, multi_L2_m = if (is.null(bc)) NA_real_ else bc$l2,
      multi_L3_m = if (is.null(bc)) NA_real_ else bc$l3,
      mm_bus_tr = NA_real_, mm_metro_tr = NA_real_, mm_mode_sw = NA_real_, mm_bus_rt = NA_real_,
      mm_metro_ln = NA_real_, mm_tot_stops = NA_real_, multi_has_brt = FALSE,
      mm_same_acc = FALSE, mm_closer = NA, mm_on_route = NA)
    if (!is.null(bc)) {
      det <- getPD(bc$s, bc$e)
      p$mm_dist_metro_m<-det$dist_metro_m; p$mm_dist_brt_m<-det$dist_brt_m; p$mm_dist_std_m<-det$dist_bus_std_m
      p$mm_dist_walk_m<-det$dist_walk_transfer_m; p$mm_metro_segments<-det$seg_str_metro
      p$mm_bus_tr<-det$n_bus_transfers; p$mm_metro_tr<-det$n_metro_transfers; p$mm_mode_sw<-det$n_mode_switches
      p$mm_bus_rt<-det$n_bus_routes; p$mm_metro_ln<-det$n_metro_lines
      p$mm_tot_stops<-if (is.na(det$n_stops)) NA_real_ else max(0, det$n_stops - 1)
      p$multi_has_brt<-det$has_brt; p$mm_closer<-(bc$l1 < road); p$mm_on_route<-cor_route(bc$l1, bc$l3, road)
      recov <- recov + 1L
    }
    if (is.na(tot_mo) && is.na(min_tot)) { p$best_mode <- NA_character_; p$best_total_m <- NA_real_ }
    else if (is.na(min_tot))             { p$best_mode <- "Metro-only"; p$best_total_m <- tot_mo }
    else if (is.na(tot_mo))              { p$best_mode <- "Multimodal"; p$best_total_m <- min_tot }
    else {
      fdf <- bind_rows(data.frame(mode="Metro-only", total=tot_mo, prio=10, stringsAsFactors=FALSE),
                       data.frame(mode="Multimodal", total=min_tot, prio=best_prio, stringsAsFactors=FALSE))
      bf <- sbt(fdf, tol_pct = TOLP); p$best_mode <- bf$mode[1]; p$best_total_m <- bf$total[1]
    }
    patch[[ii]] <- p
  }

  # ---- write patched fields back into results_df ---------------------------
  pdf <- bind_rows(lapply(patch, function(p) as.data.frame(p, stringsAsFactors=FALSE)))
  rdf_corr <- rdf
  mm_cols <- setdiff(names(pdf), ".rid")
  for (cc in mm_cols) rdf_corr[[cc]][pdf$.rid] <- pdf[[cc]]
  msg("recovered %d / %d non-reachable trips (%.1f%%); %d remain genuinely non-reachable.",
      recov, length(nr), 100*recov/length(nr), length(nr) - recov)

  # ---- emit the full corrected results_df (feeds R14 orchestrate / cheap R13) --
  if (emit_results) {
    rf <- file.path(out_dir, sprintf("sample_test_results_corrected_%s.rds", tag))
    saveRDS(rdf_corr, rf); msg("wrote %s", basename(rf))
  }

  # ---- emit corrected R13_combos: existing combos + recovered multimodal candidates --
  # (the shared root for Figs 2-4, R14 chain break-even, and the R13 sweep tornado)
  if (emit_combos) {
    cf <- file.path(out_dir, sprintf("R13_combos_%s.rds", tag))
    if (file.exists(cf)) {
      base_combos <- readRDS(cf)
      add <- bind_rows(Filter(Negate(is.null), combos_patch))
      if (nrow(add)) {
        miss_cols <- setdiff(names(base_combos), names(add))
        for (mc in miss_cols) add[[mc]] <- NA           # align schema (defensive)
        add <- add[, names(base_combos), drop = FALSE]
        corr_combos <- bind_rows(base_combos, add)
        of <- file.path(out_dir, sprintf("R13_combos_%s_CORRECTED.rds", tag))
        saveRDS(corr_combos, of)
        msg("wrote %s: %d base + %d recovered multimodal candidate rows (%d recovered trips).",
            basename(of), nrow(base_combos), nrow(add),
            dplyr::n_distinct(add$rp_id, add$dest_type, add$clinic_id))
      } else msg("no recovered multimodal candidates to inject into combos.")
    } else msg("R13_combos_%s.rds not found; corrected combos skipped (run the enhanced R1-3 capture first).", tag)
  }

  # ---- per-anchor before/after non-reachability summary --------------------
  ord <- c("nearest_priv","nearest_pub","median_priv","median_pub",
           "farthest_priv","farthest_pub","random_priv","random_pub")
  summ <- lapply(ord, function(a) {
    io <- which(rdf$dest_type == a); if (!length(io)) return(NULL)
    # non-reach = EXACT Table-2 row-24 formula (is.na(multi) & !same_acc, na.rm as in npct);
    # too_close = same-access "too-close" trips (row 23), NOT re-routable (s==e, no transit leg).
    o_nr <- sum(is.na(rdf$multi_total_m[io]) & rdf$mm_same_acc[io] == FALSE, na.rm = TRUE)
    c_nr <- sum(is.na(rdf_corr$multi_total_m[io]) & rdf_corr$mm_same_acc[io] == FALSE, na.rm = TRUE)
    too_close <- sum(rdf$mm_same_acc[io] == TRUE, na.rm = TRUE)
    data.frame(anchor=a, n=length(io), too_close=too_close,
               orig_nonreach=o_nr, orig_nonreach_pct=round(100*o_nr/length(io),2),
               corr_nonreach=c_nr, corr_nonreach_pct=round(100*c_nr/length(io),2),
               recovered=o_nr-c_nr, recovered_pct=if (o_nr>0) round(100*(o_nr-c_nr)/o_nr,2) else NA_real_,
               stringsAsFactors=FALSE)
  })
  summ <- bind_rows(summ)
  msg("per-anchor multimodal non-reachability: certified (before) vs constrained-valid re-route (after).")
  msg("  before non-reach total = %s (matches Table-2 'Non-reachable through transit'); too-close (not re-routable) = %s.",
      format(sum(summ$orig_nonreach), big.mark=","), format(sum(summ$too_close), big.mark=","))
  print(as.data.frame(summ), row.names = FALSE)

  out <- list(summary = summ, tag = tag, include_random = include_random,
              n_nonreach = length(nr), n_recovered = recov,
              rdf_corr_mm = rdf_corr[, c("rp_id","dest_type","dkey","multi_total_m","best_mode","best_total_m")])
  saveRDS(out, file.path(out_dir, sprintf("R14_mmreach_full_%s.rds", tag)))
  msg("wrote R14_mmreach_full_%s.rds", tag)

  # ---- before/after summary docx -------------------------------------------
  if (requireNamespace("flextable", quietly=TRUE) && requireNamespace("officer", quietly=TRUE)) {
    suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
    ft <- flextable(summ) %>% theme_booktabs() %>% autofit() %>%
      set_header_labels(anchor="Anchor", n="Trips", too_close="Too-close (same access, not re-routable)",
        orig_nonreach="Non-reach (before)", orig_nonreach_pct="Before %",
        corr_nonreach="Non-reach (after)", corr_nonreach_pct="After %",
        recovered="Recovered", recovered_pct="Recovered %") %>%
      add_header_lines(sprintf("Multimodal reachability, FULL correction: certified single-shortest vs shortest-VALID-path re-route (%s)", tag)) %>%
      add_footer_lines(paste0(
        "Before = certified .impl_l2r, which rejects the single shortest multimodal path when it violates the transfer rules (forbidden first/last transfer or two consecutive transfers); the Before column equals the Table-2 'Non-reachable through transit' row. ",
        "After = the SAME nearest access nodes routed on a transfer-state node-split graph that admits only rule-valid paths, so a single shortest-path query returns the shortest VALID path. ",
        "Too-close = trips whose nearest access node for origin and facility coincide (s = e, no transit leg): Table-2 'Access node = access node', NOT re-routable and NOT counted as non-reachable. ",
        "Reachable trips are unchanged; only rejected trips are re-routed. Random anchors ", if (include_random) "ARE" else "are NOT", " included."))
    save_as_docx(ft, path = file.path(out_dir, sprintf("R14_mmreach_full_%s.docx", tag)))
    msg("wrote R14_mmreach_full_%s.docx", tag)
  }

  # ---- corrected Table 2 (ALL8 + NEW4), reusing the in-scope builders ------
  gcs <- gv_get("get_column_stats"); vars <- gv_get("vars"); groups <- gv_get("groups"); bft <- gv_get(".build_acc_ft")
  if (write_tables && new_present && !is.null(gcs) && !is.null(bft) &&
      all(c("farthest_priv","farthest_pub","random_priv","random_pub") %in% rdf_corr$dest_type)) {
    df_all <- data.frame(Group=groups, Variable=vars,
      Priv_Nearest=gcs(rdf_corr,"nearest_priv"), Pub_Nearest=gcs(rdf_corr,"nearest_pub"),
      Priv_Median=gcs(rdf_corr,"median_priv"),   Pub_Median=gcs(rdf_corr,"median_pub"),
      Priv_Farthest=gcs(rdf_corr,"farthest_priv"),Pub_Farthest=gcs(rdf_corr,"farthest_pub"),
      Priv_Random=gcs(rdf_corr,"random_priv"),   Pub_Random=gcs(rdf_corr,"random_pub"),
      stringsAsFactors=FALSE)
    ft_all <- bft(df_all, rep(c("Private","Public"),4),
      c("","Nearest Facility","Median-distance Facility","Farthest Facility","Random Facility"), c(1,2,2,2,2))
    save_as_docx(ft_all, path = sprintf("Accessibility_metrics_for_random_points_%s_ALL8_CORRECTED.docx", tag))
    msg("wrote Accessibility_metrics_for_random_points_%s_ALL8_CORRECTED.docx", tag)
    df_new <- data.frame(Group=groups, Variable=vars,
      Priv_Farthest=gcs(rdf_corr,"farthest_priv"),Pub_Farthest=gcs(rdf_corr,"farthest_pub"),
      Priv_Random=gcs(rdf_corr,"random_priv"),   Pub_Random=gcs(rdf_corr,"random_pub"),
      stringsAsFactors=FALSE)
    ft_new <- bft(df_new, rep(c("Private","Public"),2),
      c("","Farthest Facility","Random Facility"), c(1,2,2))
    save_as_docx(ft_new, path = sprintf("Accessibility_metrics_for_random_points_%s_NEW4_CORRECTED.docx", tag))
    msg("wrote Accessibility_metrics_for_random_points_%s_NEW4_CORRECTED.docx", tag)
  } else if (write_tables) {
    msg("corrected Table 2 skipped (new anchors absent or table builders not in scope).")
  }

  invisible(out)
}

invisible(NULL)
