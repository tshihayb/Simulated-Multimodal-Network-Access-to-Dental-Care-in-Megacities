# =============================================================================
#  _r14_imputation.R  --  R1-4 / R2-3f imputation-sensitivity engine
# -----------------------------------------------------------------------------
#  Pure, environment-agnostic helpers for the R1-4 revision item:
#    "ratio-based imputation may bias estimates ... impact not quantified"
#    + R2-3f "competitiveness computed on reachable trips only".
#
#  This file defines FUNCTIONS ONLY (no side effects on source()).  It is sourced
#  by the gated R1-4 block in `Analysis clean actual road distance.R` (which holds
#  the pipeline's in-scope travel-time helpers) and by the synthetic test
#  `_test_r14.R`.  Nothing here touches certified outputs.
#
#  Two missingness regimes (see memory cdoe-r14-imputation-plan):
#    Track 1 (MAR, imputable road legs): point->facility DIRECT net (car baseline)
#            and L1 (point->stop).  Compared under 3 methods:
#            COMPLETE-CASE, RATIO (current primary), PMM-MI (mice).
#    Track 2 (MNAR, non-reachable transit chain): not imputed; handled by
#            denominator transparency + bounds.
# =============================================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

# ---- small numeric coercion mirroring the main script's as_num ---------------
.r14_as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# -----------------------------------------------------------------------------
# r14_build_leg_table()
#   Assemble a trip-level table for ONE imputable leg, ready for the 3 methods.
#   Arguments are vectors of equal length, one element per trip:
#     net_km    : observed network distance (NA where the road route failed)
#     geo_km    : straight-line distance (always observed -> predictor)
#     origin_x  : projected easting of the random point   (predictor)
#     origin_y  : projected northing of the random point  (predictor)
#     ownership : "private" / "public"                    (predictor, factor)
#     anchor    : dest_type, e.g. "nearest_priv"          (stratum)
#     region    : official region of the origin point     (predictor, factor)
#   Returns a tibble with a logical `is_missing` flag.
# -----------------------------------------------------------------------------
r14_build_leg_table <- function(net_km, geo_km, origin_x, origin_y,
                                ownership, anchor, region, leg = NA_character_) {
  n <- length(net_km)
  stopifnot(length(geo_km) == n, length(origin_x) == n, length(origin_y) == n,
            length(ownership) == n, length(anchor) == n, length(region) == n)
  net_km <- .r14_as_num(net_km)
  net_km[is.infinite(net_km)] <- NA_real_
  tibble::tibble(
    leg        = leg,
    anchor     = as.character(anchor),
    ownership  = as.character(ownership),
    region     = as.character(region),
    geo_km     = .r14_as_num(geo_km),
    origin_x   = .r14_as_num(origin_x),
    origin_y   = .r14_as_num(origin_y),
    net_km     = net_km,
    is_missing = is.na(net_km)
  )
}

# -----------------------------------------------------------------------------
# r14_ratio_fill()
#   RATIO method (current primary): net = geo * ratio, ratio = ratio-of-means per
#   anchor (matching the pipeline's pooled per-anchor ratio), fallback 1.3.
#   Returns the input table with a completed `net_ratio` column.
# -----------------------------------------------------------------------------
r14_ratio_fill <- function(tbl, fallback = 1.3) {
  rr <- tbl %>%
    group_by(anchor) %>%
    summarise(ratio = mean(net_km, na.rm = TRUE) / mean(geo_km, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(ratio = ifelse(is.na(ratio) | is.infinite(ratio), fallback, ratio))
  tbl %>%
    left_join(rr, by = "anchor") %>%
    mutate(net_ratio = ifelse(is_missing, geo_km * ratio, net_km)) %>%
    select(-ratio)
}

# -----------------------------------------------------------------------------
# r14_pmm_fill()
#   PMM multiple imputation via mice, run SEPARATELY PER ANCHOR (so the anchor
#   stratum conditions the donor pool without adding anchor as a predictor --
#   parallel to the per-anchor ratio method and to the locked 5-predictor list
#   geo_km + origin_x + origin_y + ownership + region).
#     donors = 5  (Morris/White/Royston 2014; mice default)
#     m       = supplied by caller (adaptive: max(20, ceil(max imputed-share%)),
#               bumped to 40 if any anchor share > ~30%).
#   Returns a list:
#     $completed : list of length m; each a tibble copy of `tbl` with net_km
#                  filled (observed kept, missing replaced by draw m).
#     $m, $donors, $ok (FALSE if mice unavailable / nothing to impute)
#   `log_net = TRUE` models log(net) (PMM is transform-robust; keeps fills > 0).
# -----------------------------------------------------------------------------
r14_pmm_fill <- function(tbl, m = 20L, donors = 5L, seed = 2024L, log_net = TRUE) {
  out <- list(completed = NULL, m = as.integer(m), donors = as.integer(donors), ok = FALSE)
  if (!requireNamespace("mice", quietly = TRUE)) {
    warning("[R1-4] package 'mice' not installed; PMM-MI skipped.")
    return(out)
  }
  if (!any(tbl$is_missing)) {            # nothing to impute -> M identical copies
    out$completed <- replicate(m, tbl, simplify = FALSE)
    out$ok <- TRUE
    return(out)
  }
  tbl$.r14_ord <- seq_len(nrow(tbl))   # preserve input row order through the per-anchor split
  completed_per_anchor <- list()
  anchors <- unique(tbl$anchor)
  for (a in anchors) {
    ta <- tbl %>% filter(anchor == a)
    idx <- which(ta$is_missing)
    if (length(idx) == 0L) {             # this anchor fully observed
      completed_per_anchor[[a]] <- replicate(m, ta, simplify = FALSE)
      next
    }
    # mice input frame: response (possibly logged) + 5 predictors.
    # Standardise the numeric predictors (UTM eastings/northings are ~1e6 while
    # geo_km is ~10 -- that scale disparity makes pmm's internal regression
    # computationally singular).  PMM matches on PREDICTED values, so any linear
    # rescale of predictors is innocuous for the imputations.
    resp <- ta$net_km
    if (log_net) resp <- log(pmax(resp, 1e-6))
    .z <- function(v) { s <- stats::sd(v, na.rm = TRUE); if (!is.finite(s) || s == 0) s <- 1; (v - mean(v, na.rm = TRUE)) / s }
    mf <- data.frame(
      net       = resp,
      geo_km    = .z(ta$geo_km),
      origin_x  = .z(ta$origin_x),
      origin_y  = .z(ta$origin_y),
      ownership = factor(ta$ownership),
      region    = factor(ta$region)
    )
    # drop predictors that are constant within this anchor (mice chokes on them)
    keep_pred <- vapply(c("geo_km","origin_x","origin_y","ownership","region"),
                        function(v) length(unique(stats::na.omit(mf[[v]]))) > 1L, logical(1))
    pred_vars <- names(keep_pred)[keep_pred]
    meth <- setNames(rep("", ncol(mf)), names(mf)); meth["net"] <- "pmm"
    pm <- matrix(0L, ncol(mf), ncol(mf), dimnames = list(names(mf), names(mf)))
    pm["net", pred_vars] <- 1L           # only `net` is predicted, by the kept preds
    mi <- tryCatch(
      mice::mice(mf, m = m, method = meth, predictorMatrix = pm,
                 donors = donors, printFlag = FALSE, seed = seed),
      error = function(e) { warning("[R1-4] mice failed for anchor ", a, ": ",
                                    conditionMessage(e)); NULL })
    if (is.null(mi)) {                    # fall back to ratio fill for this anchor
      rf <- r14_ratio_fill(ta)
      completed_per_anchor[[a]] <- lapply(seq_len(m), function(j) {
        ta2 <- ta; ta2$net_km <- rf$net_ratio; ta2 })
      next
    }
    completed_per_anchor[[a]] <- lapply(seq_len(m), function(j) {
      filled <- mice::complete(mi, j)$net
      if (log_net) filled <- exp(filled)
      ta2 <- ta; ta2$net_km[idx] <- filled[idx]; ta2
    })
  }
  # stitch anchors back together for each imputation j, RESTORING input row order
  # (the per-anchor split reorders rows; callers align $completed positionally).
  out$completed <- lapply(seq_len(m), function(j) {
    st <- bind_rows(lapply(anchors, function(a) completed_per_anchor[[a]][[j]]))
    if (".r14_ord" %in% names(st)) { st <- st[order(st$.r14_ord), , drop = FALSE]; st$.r14_ord <- NULL }
    st
  })
  out$ok <- TRUE
  out
}

# -----------------------------------------------------------------------------
# r14_pool_rubin()
#   Rubin's rules for a scalar estimand (e.g. an anchor's mean network distance).
#     q : length-M vector of point estimates (one per imputation)
#     u : length-M vector of their sampling variances (e.g. s^2 / n)
#   Returns qbar, total variance T = Ubar + (1 + 1/M) B, SE, and components.
# -----------------------------------------------------------------------------
r14_pool_rubin <- function(q, u) {
  q <- .r14_as_num(q); u <- .r14_as_num(u)
  M <- length(q)
  qbar <- mean(q, na.rm = TRUE)
  Ubar <- mean(u, na.rm = TRUE)
  B    <- if (M > 1L) stats::var(q, na.rm = TRUE) else 0
  Tot  <- Ubar + (1 + 1/M) * B
  list(estimate = qbar, total_var = Tot, se = sqrt(Tot),
       within = Ubar, between = B, m = M)
}

# -----------------------------------------------------------------------------
# r14_mean_by_anchor()
#   Mean of a value column per anchor, with the sampling variance of the mean
#   (s^2/n) attached -> ready for Rubin pooling across imputations.
# -----------------------------------------------------------------------------
r14_mean_by_anchor <- function(tbl, value_col) {
  tbl %>%
    group_by(anchor) %>%
    summarise(
      n    = sum(!is.na(.data[[value_col]])),
      mean = mean(.data[[value_col]], na.rm = TRUE),
      var  = stats::var(.data[[value_col]], na.rm = TRUE) / pmax(sum(!is.na(.data[[value_col]])), 1),
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# r14_network_distance_3method()
#   Deliverable (A), network-distance portion: mean point->facility network
#   distance per anchor under COMPLETE-CASE, RATIO, and PMM-MI (pooled).
#     tbl     : leg table from r14_build_leg_table() for the DIRECT car baseline
#     pmm     : result of r14_pmm_fill(tbl, ...)  (may be $ok = FALSE)
#   Returns one row per anchor with the three method estimates (+ PMM SE/FMI-ish
#   between/within split).
# -----------------------------------------------------------------------------
r14_network_distance_3method <- function(tbl, pmm = NULL) {
  cc <- tbl %>% filter(!is_missing) %>%
    group_by(anchor) %>% summarise(complete_case = mean(net_km), .groups = "drop")
  rf <- r14_ratio_fill(tbl) %>%
    group_by(anchor) %>% summarise(ratio = mean(net_ratio), .groups = "drop")
  res <- cc %>% full_join(rf, by = "anchor")
  if (!is.null(pmm) && isTRUE(pmm$ok)) {
    per_m <- lapply(pmm$completed, function(d) r14_mean_by_anchor(d, "net_km"))
    anchors <- sort(unique(res$anchor))
    pooled <- lapply(anchors, function(a) {
      qs <- vapply(per_m, function(d) d$mean[d$anchor == a][1], numeric(1))
      us <- vapply(per_m, function(d) d$var [d$anchor == a][1], numeric(1))
      pr <- r14_pool_rubin(qs, us)
      tibble::tibble(anchor = a, pmm = pr$estimate, pmm_se = pr$se,
                     pmm_between = pr$between, pmm_within = pr$within)
    })
    res <- res %>% left_join(bind_rows(pooled), by = "anchor")
  } else {
    res <- res %>% mutate(pmm = NA_real_, pmm_se = NA_real_,
                          pmm_between = NA_real_, pmm_within = NA_real_)
  }
  res %>% arrange(anchor)
}

# -----------------------------------------------------------------------------
# r14_accounting_table()  -- Deliverable (C)
#   Per anchor x design: share of trips that needed each kind of fill, plus the
#   reachability accounting.  All inputs are logical/counts already computed by
#   the caller (which knows the pipeline's leg semantics); this just tabulates.
#     df : one row per trip with columns
#          anchor, design,
#          imp_L1 (L1 needed ratio fill), imp_direct (direct car baseline filled),
#          imp_L3 (L3 filled; ~0), same_station (metro chain collapsed to same
#          stop -- NOT non-reachable), non_reachable (best_total_m == NA: truly
#          unreachable by any transit).
# -----------------------------------------------------------------------------
r14_accounting_table <- function(df) {
  pct <- function(x) 100 * mean(x, na.rm = TRUE)
  df %>%
    group_by(design, anchor) %>%
    summarise(
      n               = n(),
      pct_L1_imputed     = pct(imp_L1),
      pct_direct_imputed = pct(imp_direct),
      pct_L3_imputed     = if ("imp_L3" %in% names(df)) pct(imp_L3) else 0,
      pct_same_station   = if ("same_station" %in% names(df)) pct(same_station) else NA_real_,
      pct_too_close      = if ("too_close" %in% names(df)) pct(too_close) else NA_real_,
      pct_non_reachable  = pct(non_reachable),   # Option A: GENUINE (excl. same-station)
      .groups = "drop"
    ) %>%
    arrange(design, anchor)
}

# -----------------------------------------------------------------------------
# r14_denominator_bounds()  -- R2-3f
#   Transit competitiveness under TWO denominators:
#     reachable-only (optimistic upper bound, current) and
#     all-trips with non-reachable counted as transit-loses (conservative lower).
#     df : one row per trip with columns
#          anchor, design, transit_wins (logical; transit faster than car for
#          a REACHABLE trip -- NA when non-reachable), non_reachable (logical).
# -----------------------------------------------------------------------------
r14_denominator_bounds <- function(df) {
  df %>%
    group_by(design, anchor) %>%
    summarise(
      n_total          = n(),
      n_reachable      = sum(!non_reachable),
      n_non_reachable  = sum(non_reachable),
      # upper bound: among reachable trips only
      compet_reachable_pct = 100 * mean(transit_wins[!non_reachable], na.rm = TRUE),
      # lower bound: non-reachable trips counted as transit losing
      compet_alltrips_pct  = 100 * sum(transit_wins & !non_reachable, na.rm = TRUE) / n(),
      .groups = "drop"
    ) %>%
    arrange(design, anchor)
}

# -----------------------------------------------------------------------------
# r14_tipping_point()  -- MNAR / truncation-by-death sensitivity (R2-3f)
#   Transit competitiveness is a binary outcome (transit faster than car) that is
#   UNDEFINED for non-reachable trips (no path -> "truncation by death"). Rather
#   than impute it, we (a) report the partial-identification (Manski) region and
#   (b) compute the TIPPING POINT: what fraction delta of non-reachable trips
#   would have to be competitive for the composite competitiveness to reach a
#   decision threshold.
#     n_compet : # reachable trips where transit beats car
#     n_reach  : # reachable trips (transit path exists)
#     n_nonreach: # truly non-reachable trips (best_total_m == NA)
#     threshold: decision threshold for the competitive SHARE (default 0.5)
#   Returns:
#     conditional   = n_compet / n_reach            (while-reachable estimand)
#     composite      = n_compet / (n_reach+n_nonreach)  (non-reachable=fail; PRIMARY
#                      composite estimand = Manski LOWER bound)
#     manski_upper   = (n_compet+n_nonreach)/N      (non-reachable=win; worst case)
#     tipping_delta  = delta s.t. composite reaches `threshold`; <0 => already past
#                      (robust regardless), >1 => unreachable even if all compete.
#   Refs: Manski 1990/2003 (bounds); Liublinska & Rubin 2014 Stat Med (tipping
#   point, binary outcome); ICH E9(R1) 2019 (estimands); Frangakis & Rubin 2002.
# -----------------------------------------------------------------------------
r14_tipping_point <- function(n_compet, n_reach, n_nonreach, threshold = 0.5) {
  N <- n_reach + n_nonreach
  cond <- if (n_reach > 0) n_compet / n_reach else NA_real_
  comp_lo <- if (N > 0) n_compet / N else NA_real_
  comp_hi <- if (N > 0) (n_compet + n_nonreach) / N else NA_real_
  # composite(delta) = (n_compet + delta*n_nonreach)/N == threshold  ->  delta*
  tip <- if (n_nonreach > 0) (threshold * N - n_compet) / n_nonreach else NA_real_
  tip_status <- if (is.na(tip)) "no non-reachable trips" else
                if (tip < 0)  "robust: composite already meets threshold even if 0% of non-reachable compete" else
                if (tip > 1)  "threshold unreachable even if 100% of non-reachable compete" else
                sprintf("%.1f%% of non-reachable trips would need to be competitive to reach %.0f%%",
                        100 * tip, 100 * threshold)
  list(conditional = cond, composite = comp_lo, manski_lower = comp_lo,
       manski_upper = comp_hi, tipping_delta = tip, threshold = threshold,
       n_compet = n_compet, n_reach = n_reach, n_nonreach = n_nonreach,
       interpretation = tip_status)
}

# -----------------------------------------------------------------------------
# r14_adaptive_m()
#   M = max(M_floor, ceil(max per-anchor imputed-share %)); bump to M_high if any
#   anchor's imputed share exceeds share_bump (%).  (White/Royston/Wood 2011;
#   Graham/Olchowski/Gilreath 2007.)
# -----------------------------------------------------------------------------
r14_adaptive_m <- function(imputed_share_pct, M_floor = 20L, M_high = 40L,
                           share_bump = 30) {
  mx <- suppressWarnings(max(imputed_share_pct, na.rm = TRUE))
  if (!is.finite(mx)) mx <- 0
  M <- max(M_floor, ceiling(mx))
  if (mx > share_bump) M <- max(M, M_high)
  as.integer(M)
}

invisible(NULL)
