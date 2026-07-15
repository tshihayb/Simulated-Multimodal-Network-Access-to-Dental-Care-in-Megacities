# =============================================================================
#  _r13_bounds.R  --  COMBINED-assumption bounds on Fig 2/3/4 across traffic speed
#  5-80, CAR-INITIATED (manuscript Fig 3/4 basis), per anchor, per weighting.
#  Three layers per curve: BASELINE + OUTER min/max bracket (over 32 selection
#  variants x 4 time-corners: {fast,slow transit} x {parking 0,15}) + INNER 5-95%
#  Monte-Carlo (K random joint draws over every sweep range).
#  Reuses .r13_reselect + .r13_path_time (from _r13_enhanced.R) + the metro-physics
#  globals (process_metro_string, calc_agg_metro_speed). Works off R13_combos_<tag>.rds
#  only (no df_analysis). Sourced by the RUN_R13_BOUNDS hook; default OFF.
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2) }))

.r13b_theme <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
        legend.position = "bottom", plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        strip.background = element_blank(), plot.caption = element_text(hjust = 0, size = 8.5))

# evaluate one fully-specified param set P over the speed grid -> per anchor x speed:
# mean car-initiated transit time, mean direct-car time, % faster, mean time saved (hrs).
.r13b_eval <- function(getsel, P, speeds) {
  sel <- getsel(P$metro_xfer_m, P$tol_pct, P$prio_scheme)
  out <- vector("list", length(speeds))
  for (j in seq_along(speeds)) {
    s   <- speeds[j]
    tt  <- .r13_path_time(sel, P, s, "car")               # car-initiated transit
    car <- (sel$road_dist_m/1000)/s*60 + P$parking
    reach <- !is.na(sel$best_total_m) & !is.na(tt)
    win <- reach & (tt < car)
    out[[j]] <- data.frame(anchor = sel$dest_type, tt = tt, car = car, reach = reach,
                           win = win, saved = ifelse(win, car - tt, NA_real_)) %>%
      dplyr::group_by(anchor) %>%
      dplyr::summarise(speed = s,
        tt_mean  = mean(tt[reach], na.rm = TRUE),
        car_mean = mean(car, na.rm = TRUE),
        pct      = 100 * mean(win[reach], na.rm = TRUE),
        saved    = mean(saved[win], na.rm = TRUE) / 60, .groups = "drop")
  }
  dplyr::bind_rows(out)
}

r13_bounds <- function(tag, out_dir = NULL, K = 200, speeds = seq(5, 80, 5), seed = 123) {
  ge <- globalenv(); msg <- function(...) message(sprintf("[R1-3-BOUNDS %s] %s", tag, sprintf(...)))
  if (is.null(out_dir)) out_dir <- if (exists("base_dir", envir = ge)) get("base_dir", envir = ge) else getwd()
  cf <- file.path(out_dir, sprintf("R13_combos_%s.rds", tag))
  if (!file.exists(cf)) { msg("combos %s missing; skip.", basename(cf)); return(invisible(NULL)) }
  combos <- readRDS(cf); if (!"clinic_id" %in% names(combos)) combos$clinic_id <- NA_character_
  combos$metro_pre <- vapply(combos$seg_str, process_metro_string, numeric(1))
  g <- function(n, d) if (exists(n, envir = ge)) get(n, envir = ge) else d
  BASE <- list(walk = g("speed_walk", 4), metro_wait_mean = g("metro_wait_max", 7.5)/2,
               bus_wait_mean = g("bus_wait_max", 20)/2, dwell = g("stop_penalty_min", 0.5),
               parking = 0, pen_metro_tr = 0, pen_bus_tr = 0, pen_mode_sw = 0, transfer_mult = 1,
               brt_gap = g("brt_gap_factor", 0.5), brt_cap = g("brt_speed_cap", 80),
               metro_xfer_m = 50, tol_pct = g("TOL_PCT", 0.10), prio_scheme = "default")

  selcache <- new.env()
  getsel <- function(mx, tl, pr) { k <- paste(mx, tl, pr); v <- get0(k, selcache)
    if (is.null(v)) { v <- .r13_reselect(combos, mx, tl, pr); assign(k, v, selcache) }; v }

  msg("baseline + outer bracket (32 selections x 4 time-corners) + %d-draw MC over speeds %s ...",
      K, paste(range(speeds), collapse="-"))
  base_df <- .r13b_eval(getsel, BASE, speeds)

  # ---- OUTER min/max bracket ----
  fast <- list(walk=6, mw=3.75, bw=10,   dwell=0.25, tm=0.5, brt=1, pm=0, pb=0, ps=0)
  slow <- list(walk=2, mw=7.5,  bw=22.5, dwell=1.0,  tm=2,   brt=0, pm=2, pb=4, ps=6)
  sels <- expand.grid(mx=c(50,100,150,200), tl=c(.05,.10,.20,.30), pr=c("default","total_only"), stringsAsFactors=FALSE)
  outer_list <- list()
  for (si in seq_len(nrow(sels))) for (tc in list(fast, slow)) for (pk in c(0, 15)) {
    P <- modifyList(BASE, list(walk=tc$walk, metro_wait_mean=tc$mw, bus_wait_mean=tc$bw, dwell=tc$dwell,
            transfer_mult=tc$tm, brt_gap=tc$brt, pen_metro_tr=tc$pm, pen_bus_tr=tc$pb, pen_mode_sw=tc$ps,
            parking=pk, metro_xfer_m=sels$mx[si], tol_pct=sels$tl[si], prio_scheme=sels$pr[si]))
    outer_list[[length(outer_list)+1]] <- .r13b_eval(getsel, P, speeds)
  }
  outer_df <- dplyr::bind_rows(outer_list) %>% dplyr::group_by(anchor, speed) %>%
    dplyr::summarise(dplyr::across(c(tt_mean,car_mean,pct,saved),
      list(lo=~min(.x,na.rm=TRUE), hi=~max(.x,na.rm=TRUE)), .names="{.col}_{.fn}"), .groups="drop")

  # ---- INNER 5-95% Monte-Carlo ----
  set.seed(seed)
  MC <- list(walk=c(2,3,4,5,6), dwell=c(.25,.5,1), tm=c(.5,1,1.5,2), brt=c(0,.25,.5,.75,1), pk=c(0,7.5,15),
             wait=list(c(5,12.5),c(3.75,10),c(3.75,11.25),c(7.5,22.5)), pen=list(c(0,0,0),c(1,2,3),c(2,4,6)),
             mx=c(50,100,150,200), tl=c(.05,.10,.20,.30), pr=c("default","total_only"))
  mc_list <- vector("list", K)
  for (k in seq_len(K)) {
    w <- MC$wait[[sample.int(length(MC$wait),1)]]; pn <- MC$pen[[sample.int(length(MC$pen),1)]]
    P <- modifyList(BASE, list(walk=sample(MC$walk,1), dwell=sample(MC$dwell,1), transfer_mult=sample(MC$tm,1),
            brt_gap=sample(MC$brt,1), parking=sample(MC$pk,1), metro_wait_mean=w[1], bus_wait_mean=w[2],
            pen_metro_tr=pn[1], pen_bus_tr=pn[2], pen_mode_sw=pn[3],
            metro_xfer_m=sample(MC$mx,1), tol_pct=sample(MC$tl,1), prio_scheme=sample(MC$pr,1)))
    mc_list[[k]] <- .r13b_eval(getsel, P, speeds)
  }
  mc_df <- dplyr::bind_rows(mc_list) %>% dplyr::group_by(anchor, speed) %>%
    dplyr::summarise(dplyr::across(c(tt_mean,car_mean,pct,saved),
      list(lo=~stats::quantile(.x,.05,na.rm=TRUE), hi=~stats::quantile(.x,.95,na.rm=TRUE)), .names="{.col}_{.fn}"), .groups="drop")

  saveRDS(list(baseline=base_df, outer=outer_df, inner=mc_df, speeds=speeds, K=K),
          file.path(out_dir, sprintf("R13_bounds_%s.rds", tag)))
  msg("wrote R13_bounds_%s.rds", out_suffix <- tag)

  # ---- 3 figures (nested ribbons) ----
  anchor_lv <- c("nearest_priv","median_priv","farthest_priv","random_priv","nearest_pub","median_pub","farthest_pub","random_pub")
  anchor_lab<- c("Nearest (priv)","Median (priv)","Farthest (priv)","Random (priv)","Nearest (pub)","Median (pub)","Farthest (pub)","Random (pub)")
  fac <- function(a) factor(a, levels=anchor_lv, labels=anchor_lab)
  if (!(requireNamespace("ggplot2",quietly=TRUE))) return(invisible(NULL))
  mk_fig <- function(qty, ylab, ttl, fname, show_car=FALSE) {
    d <- base_df %>% transmute(anchor, speed, base=.data[[qty]], carbase=car_mean) %>%
      left_join(outer_df %>% transmute(anchor, speed, lo_o=.data[[paste0(qty,"_lo")]], hi_o=.data[[paste0(qty,"_hi")]]), by=c("anchor","speed")) %>%
      left_join(mc_df    %>% transmute(anchor, speed, lo_m=.data[[paste0(qty,"_lo")]], hi_m=.data[[paste0(qty,"_hi")]]), by=c("anchor","speed"))
    d$anchor <- fac(d$anchor)
    p <- ggplot(d, aes(speed)) +
      geom_ribbon(aes(ymin=lo_o, ymax=hi_o), fill="#0072B2", alpha=0.16) +
      geom_ribbon(aes(ymin=lo_m, ymax=hi_m), fill="#0072B2", alpha=0.36) +
      geom_line(aes(y=base), color="black", linewidth=0.7)
    if (show_car) p <- p + geom_line(aes(y=carbase), color="#D55E00", linewidth=0.6, linetype="longdash")
    p <- p + facet_wrap(~anchor, ncol=4, scales="free_y") +
      labs(x="Average traffic speed for car / standard bus (km/h)", y=ylab,
           title=ttl, subtitle=paste0("Car-initiated transit, ", tag,
             ". Black line = baseline; dark band = 5-95% Monte-Carlo; light band = min/max over all assumption extremes",
             if (show_car) "; dashed orange = direct-car baseline." else "."),
           caption="Combined uncertainty across: walking speed, peak/off-peak waits, dwell, BRT speed, transfer penalties, metro line-change, mode-choice tolerance & priority.") +
      .r13b_theme
    ggsave(file.path(out_dir, fname), p, width=14, height=7.5, dpi=300, bg="white", compression="lzw")
    msg("wrote %s", fname)
  }
  mk_fig("tt_mean", "Mean travel time (min)", sprintf("Fig 2 bounds: mean transit travel time vs traffic speed (%s)", tag),
         sprintf("Fig_R13_bounds_traveltime_%s.tiff", tag), show_car=TRUE)
  mk_fig("pct", "Transit trips faster than driving (%)", sprintf("Fig 3 bounds: %% of transit trips faster than driving (%s)", tag),
         sprintf("Fig_R13_bounds_pctfaster_%s.tiff", tag))
  mk_fig("saved", "Mean time saved vs driving (hours)", sprintf("Fig 4 bounds: mean time saved by transit (%s)", tag),
         sprintf("Fig_R13_bounds_timesaved_%s.tiff", tag))
  invisible(NULL)
}
invisible(NULL)
