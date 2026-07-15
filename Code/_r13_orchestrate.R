# =============================================================================
#  _r13_orchestrate.R  --  R1-3 modelling-assumption SENSITIVITY (cheap pass)
# -----------------------------------------------------------------------------
#  Sourced by the RUN_R13-gated hooks in block 17b of Section 18 (unweighted) and
#  Section 19 (weighted). Runs in the global env -> sees df_analysis + the
#  pipeline helpers (split_legs, calc_brt_speed, calc_agg_metro_speed, as_num)
#  and the baseline constants. PURELY ADDITIVE: writes R13_*; certified outputs
#  untouched. Default OFF (RUN_R13 unset).
#
#  Sweeps (one-factor-at-a-time around the certified baseline), all as CHEAP
#  post-processing on the cached df_analysis (NO re-routing — chain distances and
#  transfer COUNTS are fixed; only the time aggregation changes):
#    1. walk speed            seq(2,6,1)         (baseline 4)
#    2. multimodal transfer   x{0.5,1,1.5,2}     multiplier on ACTUAL transfer
#       walk                                      walk (same-stop = 0 stays 0)
#    3. wait / peak-off-peak  mean-wait-per-boarding scenarios (denied-boarding)
#    4. dwell penalty         {0.25,0.5,1.0} min/stop
#    5. parking-search        +{0,7.5,15} min on direct-car + car-initiated L1
#       (park-and-ride); walk-initiated unaffected -> raises break-even
#  Reports break-even speed (car vs WALK-initiated transit) + competitiveness@40
#  per anchor for each swept value, summarised as a tornado (swing vs baseline).
#  The EXACT metro line-change (50->200) + per-type transfer split + priority/
#  TOL_PCT need the enhanced routing run (separate) and are NOT in this pass.
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

# baseline mean wait per boarding = max/2 (mean of U(0,max)); denied-boarding adds
# `denied * headway`.  Scenarios expressed directly as mean-wait-per-boarding.
R13_WAIT_SCENARIOS <- list(
  off_peak     = c(metro = 10/2,            bus = 25/2),                 # long headway, no denial
  midpoint     = c(metro = 7.5/2,           bus = 20/2),                 # current headline
  peak_mild    = c(metro = 5/2 + 0.25*5,    bus = 15/2 + 0.25*15),       # short headway + mild crowding
  peak_severe  = c(metro = 5/2 + 1.0*5,     bus = 15/2 + 1.0*15)         # short headway + severe crowding
)

# -----------------------------------------------------------------------------
# r13_tt_param() — parameterised copy of .tt_anchor_summaries. At the baseline
#   P (walk=4, metro/bus wait means 3.75/10, dwell=0.5, transfer_mult=1,
#   parking=0) it reproduces .tt_anchor_summaries EXACTLY. Globals split_legs /
#   calc_brt_speed / calc_agg_metro_speed / as_num resolve from the calling env.
# -----------------------------------------------------------------------------
r13_tt_param <- function(dat, type, tgt, P, speeds_vec) {
  if (is.null(P$brt_gap)) P$brt_gap <- 0.5; if (is.null(P$brt_cap)) P$brt_cap <- 80
  tmf <- function(d, s) (d / s) * 60
  brt_spd <- function(s) pmin(s + P$brt_gap * (P$brt_cap - s), P$brt_cap)  # parameterised BRT speed (R1-3 brt_gap sweep)
  sub_car   <- dat %>% filter(!is.na(road_dist_km))
  sub_metro <- dat %>% filter(!is.na(chain_metro_km))
  sub_multi <- dat %>% filter(!is.na(chain_multi_km))
  N_car <- nrow(sub_car); N_metro <- nrow(sub_metro); N_multi <- nrow(sub_multi)
  if (N_metro > 0) {
    metro_stop_pen <- P$dwell * sub_metro$metro_dwell
    ml <- split_legs(sub_metro$chain_metro_km, sub_metro$pct_metro_l1, sub_metro$pct_metro_l2)
    metro_l2_walk <- pmin(pmax(sub_metro$metro_transfer_walk_km, 0, na.rm=TRUE), pmax(ml$l2, 0, na.rm=TRUE), na.rm=TRUE)
    metro_l2_ride <- pmax(ml$l2 - metro_l2_walk, 0, na.rm=TRUE)
    metro_wait_avg <- (pmax(0L, as.integer(sub_metro$metro_only_transfers)) + 1L) * P$metro_wait_mean
    metro_spd_agg  <- calc_agg_metro_speed(metro_l2_ride)
  }
  if (N_multi > 0) {
    multi_stop_pen <- P$dwell * sub_multi$multi_dwell
    xl <- split_legs(sub_multi$chain_multi_km, sub_multi$pct_multi_l1, sub_multi$pct_multi_l2)
    pt <- sub_multi$path_type
    n_mtr <- coalesce(as_num(sub_multi$mm_metro_tr),0); n_btr <- coalesce(as_num(sub_multi$mm_bus_tr),0); n_msw <- coalesce(as_num(sub_multi$mm_mode_sw),0)
    n_mb <- ifelse(pt=="Metro-Metro", 1+n_mtr, ifelse(pt=="Metro-Bus", 1+n_mtr, ifelse(pt=="Bus-Metro", n_msw+n_mtr, 0)))
    n_bb <- ifelse(pt=="Bus-Bus", 1+n_btr, ifelse(pt=="Metro-Bus", n_msw+n_btr, ifelse(pt=="Bus-Metro", 1+n_btr, 0)))
    n_mb[is.na(pt)] <- 0; n_bb[is.na(pt)] <- 0
    multi_wait_avg  <- n_mb*P$metro_wait_mean + n_bb*P$bus_wait_mean
    multi_xfer_walk <- sub_multi$multi_transfer_walk_km * P$transfer_mult   # actual walk (0 stays 0)
  }
  rows <- list()
  for (s in speeds_vec) {
    if (N_car > 0) {
      car_t <- tmf(sub_car$road_dist_km, s) + P$parking
      rows[[length(rows)+1]] <- data.frame(Type=type,Target=tgt,Speed=s,Mode_family="Car-only (direct to clinic)",Initiation="Car-initiated",Time_mean=mean(car_t,na.rm=TRUE),stringsAsFactors=FALSE)
    }
    if (N_metro > 0) {
      base_m <- tmf(metro_l2_ride, metro_spd_agg) + tmf(metro_l2_walk, P$walk) + tmf(ml$l3, P$walk) + metro_stop_pen + metro_wait_avg
      rows[[length(rows)+1]] <- data.frame(Type=type,Target=tgt,Speed=s,Mode_family="Metro-only",Initiation="Car-initiated", Time_mean=mean(tmf(ml$l1, s)     + base_m + P$parking, na.rm=TRUE),stringsAsFactors=FALSE)
      rows[[length(rows)+1]] <- data.frame(Type=type,Target=tgt,Speed=s,Mode_family="Metro-only",Initiation="Walk-initiated",Time_mean=mean(tmf(ml$l1, P$walk) + base_m,           na.rm=TRUE),stringsAsFactors=FALSE)
    }
    if (N_multi > 0) {
      l2_time <- (sub_multi$dist_brt_km/brt_spd(s))*60 + (sub_multi$dist_std_km/s)*60 + sub_multi$metro_time_precise_min + (multi_xfer_walk/P$walk)*60
      base_mm <- l2_time + tmf(xl$l3, P$walk) + multi_stop_pen + multi_wait_avg
      rows[[length(rows)+1]] <- data.frame(Type=type,Target=tgt,Speed=s,Mode_family="Multimodal",Initiation="Car-initiated", Time_mean=mean(tmf(xl$l1, s)     + base_mm + P$parking, na.rm=TRUE),stringsAsFactors=FALSE)
      rows[[length(rows)+1]] <- data.frame(Type=type,Target=tgt,Speed=s,Mode_family="Multimodal",Initiation="Walk-initiated",Time_mean=mean(tmf(xl$l1, P$walk) + base_mm,           na.rm=TRUE),stringsAsFactors=FALSE)
    }
  }
  bind_rows(rows)
}

# break-even speed: car (with parking) crosses transit; `ini` picks the transit
# initiation (Walk-initiated = the policy "drive vs take transit" comparison).
r13_breakeven <- function(dsum, type, tgt, mf, ini) {
  car <- dsum %>% filter(Type==type, Target==tgt, Mode_family=="Car-only (direct to clinic)", Initiation=="Car-initiated") %>% arrange(Speed)
  tr  <- dsum %>% filter(Type==type, Target==tgt, Mode_family==mf, Initiation==ini) %>% arrange(Speed)
  if (nrow(car) < 2 || nrow(tr) < 2) return(NA_real_)
  d <- car$Time_mean - tr$Time_mean
  if (all(d > 0, na.rm=TRUE) || all(d < 0, na.rm=TRUE)) return(NA_real_)
  tryCatch(stats::approx(x = d, y = car$Speed, xout = 0, ties = mean)$y, error = function(e) NA_real_)
}

# finite-only summaries: anchors whose break-even never crosses within the swept
# speed range (e.g. NEAREST anchors, where driving beats walk-initiated transit at
# every speed) yield all-NA break-even. Plain min/max(na.rm=TRUE) over an all-NA
# group returns Inf/-Inf, which then poisons the dimension-level sum() to -Inf. These
# helpers return NA for empty/degenerate groups so the tornado stays finite.
.fin_min   <- function(v) { v <- v[is.finite(v)]; if (length(v))      min(v)         else NA_real_ }
.fin_max   <- function(v) { v <- v[is.finite(v)]; if (length(v))      max(v)         else NA_real_ }
.fin_swing <- function(v) { v <- v[is.finite(v)]; if (length(v) >= 2) max(v)-min(v)  else NA_real_ }

# r13_tornado(res) — per (dimension x anchor) break-even swing across swept values,
# robust to anchors with no finite break-even. Reused by the cache-regeneration path.
r13_tornado <- function(res) {
  res %>% group_by(dimension, anchor) %>%
    summarise(be_metro_min=.fin_min(be_metro_walk), be_metro_max=.fin_max(be_metro_walk),
              be_metro_swing=.fin_swing(be_metro_walk),
              compet_min=.fin_min(compet40_pct), compet_max=.fin_max(compet40_pct),
              n_be_finite=sum(is.finite(be_metro_walk)), .groups="drop") %>%
    arrange(anchor, desc(be_metro_swing))
}

# -----------------------------------------------------------------------------
# r13_pertrip_compet() — per-trip competitiveness@s_ref under params: best
#   WALK-initiated transit time < car drive time (car parks). Option-A reachable
#   set (genuine non-reachable excluded; same-station "too close" excluded).
# -----------------------------------------------------------------------------
r13_pertrip_compet <- function(dat, P, s_ref = 40) {
  if (is.null(P$brt_gap)) P$brt_gap <- 0.5; if (is.null(P$brt_cap)) P$brt_cap <- 80
  tmf <- function(d, s) (d / s) * 60
  ml <- split_legs(dat$chain_metro_km, dat$pct_metro_l1, dat$pct_metro_l2)
  xl <- split_legs(dat$chain_multi_km, dat$pct_multi_l1, dat$pct_multi_l2)
  t_metro <- ifelse(!is.na(dat$chain_metro_km),
    tmf(pmax(ml$l1,0), P$walk) + tmf(pmax(metro_ride <- pmax(ml$l2 - pmin(pmax(dat$metro_transfer_walk_km,0),pmax(ml$l2,0)),0),0), s_ref) +
    tmf(pmin(pmax(dat$metro_transfer_walk_km,0),pmax(ml$l2,0)), P$walk) + tmf(pmax(ml$l3,0), P$walk) +
    (pmax(0L, as.integer(dat$metro_only_transfers))+1L)*P$metro_wait_mean, NA_real_)
  t_multi <- ifelse(!is.na(dat$chain_multi_km),
    tmf(pmax(xl$l1,0), P$walk) +
    ((dat$dist_brt_km/pmin(s_ref + P$brt_gap*(P$brt_cap - s_ref), P$brt_cap))*60 + (dat$dist_std_km/s_ref)*60 + dat$metro_time_precise_min +
     (dat$multi_transfer_walk_km*P$transfer_mult/P$walk)*60) +
    tmf(pmax(xl$l3,0), P$walk) + P$dwell*dat$multi_dwell, NA_real_)
  t_best <- pmin(t_metro, t_multi, na.rm = TRUE); t_best[is.infinite(t_best)] <- NA_real_
  car <- tmf(dat$road_dist_km, s_ref) + P$parking
  same_station <- if ("metro_same_stn" %in% names(dat)) coalesce(as.logical(dat$metro_same_stn), FALSE) else FALSE
  data.frame(anchor = dat$dest_type, same_station = same_station,
             non_reachable = is.na(dat$best_total_m) & !same_station,
             too_close = is.na(dat$best_total_m) & same_station,
             transit_wins = ifelse(is.na(t_best), NA, t_best < car))
}

# -----------------------------------------------------------------------------
# r13_orchestrate(tag) — run the OFAT sweep, write R13_* outputs.
# -----------------------------------------------------------------------------
r13_orchestrate <- function(tag, out_suffix = tag, out_dir = NULL) {
  ge <- globalenv(); msg <- function(...) message(sprintf("[R1-3 %s] %s", tag, sprintf(...)))
  if (!exists("df_analysis", envir=ge)) { msg("df_analysis missing; skipping."); return(invisible(NULL)) }
  df_analysis <- get("df_analysis", envir=ge)
  if (is.null(out_dir)) out_dir <- if (exists("base_dir",envir=ge)) get("base_dir",envir=ge) else getwd()
  spd <- if (exists("speeds",envir=ge)) get("speeds",envir=ge) else seq(5,80,1)
  g <- function(n,d) if (exists(n,envir=ge)) get(n,envir=ge) else d
  BASE <- list(walk=g("speed_walk",4), metro_wait_mean=g("metro_wait_max",7.5)/2,
               bus_wait_mean=g("bus_wait_max",20)/2, dwell=g("stop_penalty_min",0.5),
               transfer_mult=1, parking=0,
               brt_gap=g("brt_gap_factor",0.5), brt_cap=g("brt_speed_cap",80))
  anchors <- tibble::tribble(~dest_type,~Type,~Target,
    "nearest_priv","Private","Nearest","median_priv","Private","Specific","farthest_priv","Private","Farthest","random_priv","Private","Random",
    "nearest_pub","Public","Nearest","median_pub","Public","Specific","farthest_pub","Public","Farthest","random_pub","Public","Random")
  anchors <- anchors %>% filter(dest_type %in% df_analysis$dest_type)

  # ---- baseline validation: r13_tt_param at BASE must reproduce .tt_anchor_summaries ----
  if (exists(".tt_anchor_summaries", envir=ge) && nrow(anchors)) {
    a1 <- anchors[1,]
    ref <- get(".tt_anchor_summaries",envir=ge)(df_analysis %>% filter(dest_type==a1$dest_type), a1$Type, a1$Target)
    new <- r13_tt_param(df_analysis %>% filter(dest_type==a1$dest_type), a1$Type, a1$Target, BASE, spd)
    cmp <- ref %>% transmute(Speed,Mode_family=as.character(Mode_family),Initiation=as.character(Initiation),ref=Time_mean) %>%
      inner_join(new %>% transmute(Speed,Mode_family=as.character(Mode_family),Initiation=as.character(Initiation),new=Time_mean),
                 by=c("Speed","Mode_family","Initiation"))
    msg("[validate] r13_tt_param vs .tt_anchor_summaries max|delta| = %.3e min (must be ~0)", max(abs(cmp$new-cmp$ref),na.rm=TRUE))
  }

  # ---- OFAT sweeps -> break-even (vs walk-initiated transit) + competitiveness@40 ----
  sweeps <- list(
    walk         = lapply(c(2,3,4,5,6),       function(v){ p<-BASE; p$walk<-v;          list(p=p,lab=sprintf("walk=%g",v)) }),
    transfer_mult= lapply(c(0.5,1,1.5,2),     function(v){ p<-BASE; p$transfer_mult<-v; list(p=p,lab=sprintf("xfer x%g",v)) }),
    wait         = lapply(names(R13_WAIT_SCENARIOS), function(nm){ p<-BASE; p$metro_wait_mean<-R13_WAIT_SCENARIOS[[nm]]["metro"]; p$bus_wait_mean<-R13_WAIT_SCENARIOS[[nm]]["bus"]; list(p=p,lab=paste0("wait:",nm)) }),
    dwell        = lapply(c(0.25,0.5,1.0),    function(v){ p<-BASE; p$dwell<-v;          list(p=p,lab=sprintf("dwell=%g",v)) }),
    parking      = lapply(c(0,7.5,15),        function(v){ p<-BASE; p$parking<-v;        list(p=p,lab=sprintf("park=%g",v)) }),
    brt_gap      = lapply(c(0,0.25,0.5,0.75,1),function(v){ p<-BASE; p$brt_gap<-v;        list(p=p,lab=sprintf("brt_gap=%g",v)) }))

  out <- list()
  for (dim in names(sweeps)) {
    for (cell in sweeps[[dim]]) {
      P <- cell$p
      dsum <- bind_rows(lapply(seq_len(nrow(anchors)), function(i)
        r13_tt_param(df_analysis %>% filter(dest_type==anchors$dest_type[i]), anchors$Type[i], anchors$Target[i], P, spd)))
      comp <- r13_pertrip_compet(df_analysis %>% filter(dest_type %in% anchors$dest_type), P) %>%
        filter(!too_close) %>% group_by(anchor) %>%
        summarise(compet40_pct = 100*mean(transit_wins[!non_reachable], na.rm=TRUE), .groups="drop")
      be <- anchors %>% rowwise() %>% mutate(
        be_metro_walk = r13_breakeven(dsum, Type, Target, "Metro-only","Walk-initiated"),
        be_multi_walk = r13_breakeven(dsum, Type, Target, "Multimodal","Walk-initiated"),
        be_metro_car  = r13_breakeven(dsum, Type, Target, "Metro-only","Car-initiated"),
        be_multi_car  = r13_breakeven(dsum, Type, Target, "Multimodal","Car-initiated")) %>% ungroup()
      out[[length(out)+1]] <- be %>% left_join(comp, by=c("dest_type"="anchor")) %>%
        transmute(dimension=dim, scenario=cell$lab, anchor=dest_type,
                  be_metro_walk, be_multi_walk, be_metro_car, be_multi_car, compet40_pct)
    }
  }
  res <- bind_rows(out)
  base_lab <- c(walk="walk=4", transfer_mult="xfer x1", wait="wait:midpoint", dwell="dwell=0.5", parking="park=0")
  # tornado: per dimension x anchor, swing of break-even (metro) across the swept values vs baseline
  tornado <- r13_tornado(res)
  msg("sweep complete: %d (dimension x scenario x anchor) rows", nrow(res))
  msg("tornado (break-even metro swing by dimension, summed over anchors):")
  print(as.data.frame(tornado %>% group_by(dimension) %>% summarise(total_be_swing=sum(be_metro_swing,na.rm=TRUE)) %>% arrange(desc(total_be_swing))), row.names=FALSE)

  saveRDS(list(sweep=res, tornado=tornado, base=BASE, wait_scenarios=R13_WAIT_SCENARIOS),
          file.path(out_dir, sprintf("R13_sensitivity_%s.rds", out_suffix)))
  msg("wrote R13_sensitivity_%s.rds", out_suffix)
  if (requireNamespace("flextable",quietly=TRUE) && requireNamespace("officer",quietly=TRUE)) {
    suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
    rnd <- function(d) d %>% mutate(across(where(is.numeric), ~round(.x,2)))
    save_as_docx(flextable(as.data.frame(rnd(res))) %>% theme_booktabs() %>% autofit() %>%
      add_header_lines(sprintf("R1-3 assumption sweeps (%s): break-even (vs walk-initiated transit) + competitiveness@40km/h", tag)),
      path=file.path(out_dir, sprintf("R13_sensitivity_sweep_%s.docx", out_suffix)))
    save_as_docx(flextable(as.data.frame(rnd(tornado))) %>% theme_booktabs() %>% autofit() %>%
      add_header_lines(sprintf("R1-3 tornado (%s): break-even swing by assumption", tag)),
      path=file.path(out_dir, sprintf("R13_tornado_%s.docx", out_suffix)))
    msg("wrote R13_sensitivity_sweep_%s.docx + R13_tornado_%s.docx", out_suffix, out_suffix)
  }
  invisible(res)
}
invisible(NULL)
