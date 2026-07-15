# =============================================================================
#  _r14_mm_reach_fix.R  --  OPT-IN fix for spurious multimodal non-reachability.
# -----------------------------------------------------------------------------
#  DIAGNOSIS: the certified multimodal L2 router `.impl_l2r` takes the SINGLE
#  shortest path on g_multimodal and REJECTS it (returns NA -> "non-reachable
#  through transit") when that path violates the transfer rules:
#     - first or last edge is a forbidden transfer (bus_bus_*, metro_bus*), or
#     - two transfer edges are consecutive.
#  It never searches for the shortest VALID path, so a trip is logged
#  non-reachable even when a slightly longer, rule-satisfying path exists
#  (which is exactly why the metro-only graph, forced to a pure-metro path,
#  connects the same nodes). This is the "second-best could not be chosen" issue.
#
#  FIX: `.l2r_kbest` enumerates the K shortest paths and returns the first that
#  SATISFIES the same transfer rules (masked distance, identical to .impl_l2r).
#  This file defines FUNCTIONS ONLY (no side effects on source); the certified
#  routing is untouched and only runs the fix behind the RUN_MM_REACH gate.
# =============================================================================
suppressWarnings(suppressMessages({ library(igraph) }))

# The forbidden first/last transfer types (verbatim from .impl_l2r, Analysis L6834-6835)
.R14_FORBIDDEN_TR <- c("bus_bus_exact", "metro_bus", "metro_bus_manual",
                       "bus_bus_proximity", "bus_bus_standard")

# TRUE if an edge-type sequence satisfies the certified transfer rules.
.r14_path_valid <- function(e_types) {
  n <- length(e_types)
  if (n == 0) return(FALSE)
  if (e_types[1] %in% .R14_FORBIDDEN_TR) return(FALSE)            # forbidden FIRST transfer
  if (e_types[n] %in% .R14_FORBIDDEN_TR) return(FALSE)            # forbidden LAST transfer
  all_tr <- c("metro_metro_transfer", .R14_FORBIDDEN_TR)
  is_tr  <- e_types %in% all_tr
  if (n > 1 && any(is_tr & c(FALSE, is_tr[-n]))) return(FALSE)    # CONSECUTIVE transfers
  TRUE
}

# Masked real distance of an edge path (mirrors .impl_l2r: drop first/last metro_metro_transfer cost).
.r14_masked_dist <- function(ep, graph) {
  e_types <- E(graph)$edge_type[ep]
  e_dists <- as.numeric(E(graph)$real_distance[ep])
  n <- length(ep); cost_mask <- rep(TRUE, n)
  if (e_types[1] == "metro_metro_transfer") cost_mask[1] <- FALSE
  if (n > 1 && e_types[n] == "metro_metro_transfer") cost_mask[n] <- FALSE
  sum(e_dists[cost_mask], na.rm = TRUE)
}

# Shortest VALID multimodal path (THE FIX), SAME start/end access nodes. Walks the
# shortest paths in increasing order and returns the FIRST that satisfies the
# transfer rules (masked distance, identical to .impl_l2r). The search is ADAPTIVE:
# it starts at K and keeps doubling (up to K_max) until a valid path is found OR all
# simple paths between the two nodes are exhausted (then NA = genuinely none valid).
.l2r_kbest <- function(from_node, to_node, graph, K = 20L, K_max = 200L) {
  if (is.na(from_node) || is.na(to_node) || is.null(graph)) return(NA_real_)
  if (!from_node %in% V(graph)$name || !to_node %in% V(graph)$name) return(NA_real_)
  if (from_node == to_node) return(0)
  k <- max(1L, as.integer(K)); K_max <- max(k, as.integer(K_max))
  repeat {
    kp <- tryCatch(
      suppressWarnings(igraph::k_shortest_paths(graph, from = from_node, to = to_node, k = k,
                               weights = E(graph)$weight, mode = "out")),
      error = function(e) NULL)
    if (is.null(kp)) return(NA_real_)
    eps <- kp$epaths; np <- length(eps)
    if (np == 0) return(NA_real_)                                  # no path at all
    for (ep in eps) {
      if (length(ep) == 0) next
      if (.r14_path_valid(E(graph)$edge_type[ep])) return(.r14_masked_dist(ep, graph))
    }
    if (np < k) return(NA_real_)        # fewer paths returned than asked => all enumerated, none valid
    if (k >= K_max) return(NA_real_)    # safety cap reached
    k <- min(k * 2L, K_max)             # none valid yet: search deeper
  }
}

# =============================================================================
#  r14_mm_reach_fix(tag) — DRIVER. Re-routes the currently multimodal-non-reachable
#  trips with the shortest-valid-path fix and reports recovery per anchor.
#  Reproduction-safe: reads in-scope routing objects, writes NEW files only
#  (R14_mmreach_<tag>.{rds,docx}); does NOT touch the certified results.
#  Runs behind the RUN_MM_REACH gate, inside the pipeline (graphs + lookups live).
# =============================================================================
r14_mm_reach_fix <- function(tag = "unweighted", K = 20L, out_dir = NULL,
                             include_random = TRUE) {
  ge <- globalenv(); msg <- function(...) message(sprintf("[MM-REACH %s] %s", tag, sprintf(...)))
  suppressWarnings(suppressMessages({ library(dplyr); library(igraph) }))
  if (is.null(out_dir)) out_dir <- if (exists("base_dir", envir = ge)) get("base_dir", envir = ge) else getwd()
  need <- c("g_multimodal","get_L2_distance_real",".rp_metro_node",".rp_bus_node",".cl_metro_node",".cl_bus_node")
  miss <- need[!vapply(need, exists, logical(1), envir = ge)]
  if (length(miss)) { msg("missing in-scope objects (%s); run inside the pipeline. skip.", paste(miss, collapse=",")); return(invisible(NULL)) }
  g <- get("g_multimodal", ge); getL2 <- get("get_L2_distance_real", ge)
  rpM<-get(".rp_metro_node",ge); rpB<-get(".rp_bus_node",ge); clM<-get(".cl_metro_node",ge); clB<-get(".cl_bus_node",ge)

  # ---- worklist (rp_id, dest_type, dkey) over all anchors ----
  wl <- list()
  if (exists(".ct_dest_geo", envir = ge)) {                       # originals: nearest/median priv+pub
    ctd <- get(".ct_dest_geo", envir = ge); tks <- names(ctd)
    wl$orig <- data.frame(rp_id = sub("\\|\\|.*$","",tks), dest_type = sub("^.*\\|\\|","",tks),
                          dkey = unname(unlist(ctd)), stringsAsFactors = FALSE)
  }
  N <- if (exists("N_RANDOM_DRAWS", envir = ge)) as.integer(get("N_RANDOM_DRAWS", envir = ge)) else 10L
  t1 <- file.path(out_dir, sprintf("table1_new_anchors_N%d.rds", N))
  asg <- if (file.exists(t1)) readRDS(t1)$assignments[[if(tag=="weighted")"Population-weighted" else "Uniform in populated districts only"]] else NULL
  if (!is.null(asg)) {
    if (!is.null(asg$far)) wl$far <- asg$far %>% transmute(rp_id=as.character(id), dest_type, dkey=dest_id_geo)
    if (include_random && !is.null(asg$rnd)) wl$rnd <- asg$rnd %>% transmute(rp_id=as.character(id), dest_type, dkey=dest_id_geo)
  }
  W <- bind_rows(wl)
  if (!nrow(W)) { msg("empty worklist; skip."); return(invisible(NULL)) }

  # ---- access nodes (vectorized) + same-access flag ----
  W$s_metro <- unname(rpM[W$rp_id]); W$s_bus <- unname(rpB[W$rp_id])
  W$e_metro <- unname(clM[W$dkey]);  W$e_bus  <- unname(clB[W$dkey])
  W$same_acc <- (!is.na(W$s_metro) & W$s_metro==W$e_metro) & (!is.na(W$s_bus) & W$s_bus==W$e_bus)

  # ---- 4 combos per trip (s,e) with the certified skip rules ----
  inv_mb <- (W$e_bus == W$s_bus); inv_bm <- (W$e_metro == W$s_metro)
  mk <- function(s,e,type,skip_extra) data.frame(row=seq_len(nrow(W)), type=type, s=s, e=e,
                  skip = is.na(s) | is.na(e) | (s==e) | skip_extra, stringsAsFactors=FALSE)
  CL <- bind_rows(
    mk(W$s_metro, W$e_metro, "Metro-Metro", FALSE),
    mk(W$s_metro, W$e_bus,   "Metro-Bus",   inv_mb),
    mk(W$s_bus,   W$e_metro, "Bus-Metro",   inv_bm),
    mk(W$s_bus,   W$e_bus,   "Bus-Bus",     FALSE))
  CLk <- CL[!CL$skip, , drop = FALSE]

  # ---- certified reachability per UNIQUE (s,e) (cached -> fast, ALL trips) ----
  uniq <- unique(CLk[, c("s","e")])
  msg("worklist %d trips; %d combo-queries; %d unique (s,e) pairs; certified reachability (cached) ...", nrow(W), nrow(CLk), nrow(uniq))
  uniq$orig <- vapply(seq_len(nrow(uniq)), function(i) getL2(uniq$s[i], uniq$e[i], g), numeric(1))
  CLk <- CLk %>% left_join(uniq[, c("s","e","orig")], by = c("s","e")) %>% mutate(orig_ok = !is.na(orig))
  agg <- CLk %>% group_by(row) %>% summarise(orig_reach = any(orig_ok), .groups = "drop")
  W$orig_reach <- FALSE; W$orig_reach[agg$row] <- agg$orig_reach
  W$nonreach <- (!W$orig_reach) & (!W$same_acc)          # EXACT, matches Table 2
  W$.row <- seq_len(nrow(W))

  # ---- shortest-valid re-route on a per-anchor SAMPLE of the non-reach trips ----
  # (k_shortest_paths is ~1s/call on the full graph, so we estimate the recovery RATE
  #  from a sample rather than re-route every non-reach trip. Orig non-reach is exact.)
  SAMP <- suppressWarnings(as.integer(Sys.getenv("MM_REACH_SAMPLE"))); if (is.na(SAMP)) SAMP <- 250L
  Kx   <- suppressWarnings(as.integer(Sys.getenv("MM_REACH_KMAX")));   if (is.na(Kx))   Kx   <- 40L
  set.seed(2024L)
  samp <- W %>% filter(nonreach) %>% group_by(dest_type) %>% slice_sample(n = SAMP) %>% ungroup()
  msg("non-reach trips: %d total; re-routing a per-anchor sample of %d (cap %d/anchor, K=%d..%d) ...",
      sum(W$nonreach), nrow(samp), SAMP, K, Kx)
  samp_combos <- CLk %>% filter(row %in% samp$.row & is.na(orig)) %>% distinct(row, s, e)
  fixp <- samp_combos %>% distinct(s, e); fixp$fix <- NA_real_
  if (nrow(fixp)) for (j in seq_len(nrow(fixp))) {
    if (j %% 50 == 0) msg("  shortest-valid %d / %d unique pairs ...", j, nrow(fixp))
    fixp$fix[j] <- .l2r_kbest(fixp$s[j], fixp$e[j], g, K = K, K_max = Kx)
  }
  rec_rows <- samp_combos %>% left_join(fixp, by = c("s","e")) %>%
    group_by(row) %>% summarise(recovered = any(!is.na(fix)), .groups = "drop")
  samp <- samp %>% left_join(rec_rows, by = c(".row" = "row")) %>% mutate(recovered = coalesce(recovered, FALSE))

  # ---- per anchor: EXACT original non-reach + SAMPLE-estimated recovery ----
  ord <- c("nearest_priv","nearest_pub","median_priv","median_pub","farthest_priv","farthest_pub","random_priv","random_pub")
  exact <- W %>% mutate(dest_type = factor(dest_type, levels = ord)) %>% filter(!is.na(dest_type)) %>%
    group_by(dest_type) %>% summarise(n = n(), orig_nonreach_pct = 100*mean(nonreach), .groups = "drop")
  ssum <- samp %>% mutate(dest_type = factor(dest_type, levels = ord)) %>% filter(!is.na(dest_type)) %>%
    group_by(dest_type) %>% summarise(n_sampled = n(), recovered_n = sum(recovered),
                                      recovered_rate_pct = 100*mean(recovered), .groups = "drop")
  res <- exact %>% left_join(ssum, by = "dest_type") %>%
    mutate(est_fixed_nonreach_pct = orig_nonreach_pct * (1 - coalesce(recovered_rate_pct, 0)/100))
  msg("multimodal non-reachability: EXACT original vs SAMPLE-estimated recovery (K=%d..%d, %d/anchor):", K, Kx, SAMP)
  print(as.data.frame(res %>% mutate(across(where(is.numeric), ~round(.x,2)))), row.names = FALSE)

  out <- list(per_anchor = res, tag = tag, K = K, n_worklist = nrow(W))
  saveRDS(out, file.path(out_dir, sprintf("R14_mmreach_%s.rds", tag))); msg("wrote R14_mmreach_%s.rds", tag)
  if (requireNamespace("flextable", quietly=TRUE) && requireNamespace("officer", quietly=TRUE)) {
    suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
    ft <- flextable(as.data.frame(res %>% mutate(across(where(is.numeric), ~round(.x,2))))) %>% theme_booktabs() %>% autofit() %>%
      set_header_labels(dest_type="Anchor", n="Trips", orig_nonreach_pct="Original non-reach %",
                        n_sampled="Sample n", recovered_n="Recovered (sample)", recovered_rate_pct="Recovery rate %",
                        est_fixed_nonreach_pct="Est. fixed non-reach %") %>%
      add_header_lines(sprintf("Multimodal reachability: certified single-shortest vs shortest-VALID-path fix (%s)", tag)) %>%
      add_footer_lines(paste0("Original non-reach %% (EXACT, all trips) = certified .impl_l2r, which rejects the single shortest path when it violates the transfer rules. ",
        "Recovery rate is estimated from a per-anchor random sample re-routed with the shortest VALID path (k_shortest_paths, same nearest access nodes). ",
        "Est. fixed non-reach %% = original x (1 - recovery rate). 'Recovered' trips had a valid multimodal path the certified router missed."))
    save_as_docx(ft, path = file.path(out_dir, sprintf("R14_mmreach_%s.docx", tag))); msg("wrote R14_mmreach_%s.docx", tag)
  }
  invisible(out)
}

invisible(NULL)
