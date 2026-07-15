# =============================================================================
#  _r13_enh_se.R  --  Monte-Carlo SD/SE over the 10 random facility draws, for
#  the ENHANCED-run variables. Each random anchor draws N facilities/point; this
#  computes the anchor-level metric (break-even speed vs walk-init transit, and
#  competitiveness@40) SEPARATELY for each draw, then reports the SD/SE across the
#  N draw-level estimates (the Monte-Carlo precision of the random-anchor result).
#  Sourced ALONGSIDE _r13_enhanced.R + _r13_orchestrate.R (uses .r13_reselect /
#  .r13_enh_dsum / .r13_enh_compet / r13_breakeven and the metro-physics globals).
#  NOT sourced by the pipeline hooks -> safe to edit while a run is in flight.
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

# draw_map: data.frame(rp_id, dest_type, clinic_id, draw) recovered from the
# Table-1 assignments ($assignments[[key]]$rnd: id, dest_type, dest_id_geo, draw).
r13_build_draw_map <- function(t1_cache, asg_key) {
  a <- readRDS(t1_cache)$assignments[[asg_key]]
  if (is.null(a) || is.null(a$rnd)) return(NULL)
  a$rnd %>% transmute(rp_id = as.character(id), dest_type,
                      clinic_id = gsub("^(priv_|pub_)", "", dest_id_geo),
                      draw = as.integer(draw))
}

r13_enh_se_random <- function(combos, draw_map, P, speeds, amap,
                              rand_anchors = c("random_priv","random_pub")) {
  if (is.null(draw_map)) return(NULL)
  rc <- combos %>% filter(dest_type %in% rand_anchors) %>%
        left_join(draw_map, by = c("rp_id","dest_type","clinic_id"))
  perdraw <- list()
  for (dt in rand_anchors) {
    sub <- rc %>% filter(dest_type == dt, !is.na(draw))
    if (!nrow(sub)) next
    am <- amap %>% filter(dest_type == dt)
    if (!nrow(am)) next
    for (dw in sort(unique(sub$draw))) {
      cb  <- sub %>% filter(draw == dw)
      sel <- .r13_reselect(cb, P$metro_xfer_m, P$tol_pct, P$prio_scheme)
      sel$metro_pre <- vapply(sel$seg_str, process_metro_string, numeric(1))
      dsum <- .r13_enh_dsum(sel, P, speeds, am)
      be_w <- r13_breakeven(dsum, am$Type[1], am$Target[1], "Best", "Walk-initiated")
      be_c <- r13_breakeven(dsum, am$Type[1], am$Target[1], "Best", "Car-initiated")
      cp   <- .r13_enh_compet(sel, P)
      cpv  <- cp$compet40_pct[cp$anchor == dt]
      perdraw[[length(perdraw)+1]] <- data.frame(
        dest_type = dt, draw = dw, be_best_walk = be_w, be_best_car = be_c,
        compet40_pct = if (length(cpv)) cpv else NA_real_, stringsAsFactors = FALSE)
    }
  }
  pd <- bind_rows(perdraw)
  if (!nrow(pd)) return(NULL)
  sefun <- function(v) { v <- v[is.finite(v)]; if (length(v) >= 2) sd(v)/sqrt(length(v)) else NA_real_ }
  summ <- pd %>% group_by(dest_type) %>% summarise(
    n_draws      = dplyr::n(),
    be_mean      = mean(be_best_walk, na.rm=TRUE),
    be_sd        = sd(be_best_walk,   na.rm=TRUE),
    be_se        = sefun(be_best_walk),
    be_car_mean  = mean(be_best_car, na.rm=TRUE),
    be_car_se    = sefun(be_best_car),
    compet_mean  = mean(compet40_pct, na.rm=TRUE),
    compet_sd    = sd(compet40_pct,   na.rm=TRUE),
    compet_se    = sefun(compet40_pct),
    .groups = "drop")
  list(per_draw = pd, summary = summ)
}
invisible(NULL)
