# =============================================================================
#  _r13_permode.R  --  PER-MODE (Metro-only / Multimodal) x PER-INITIATION
#  (Car / Walk) engine driven from the captured combos, for the upgraded Figs 2/3/4.
#  Produces, per (anchor, Mode_family, Initiation, speed): mean travel time, %faster
#  than direct car, mean time saved; and the 50%-crossing break-even (= the
#  manuscript Fig-3 definition: speed where 50% of trips become slower than driving).
#  Adds combined-assumption bands (baseline + 5-95% Monte-Carlo + min/max bracket).
#  Self-contained: defines its own copies of the metro/BRT physics so it runs
#  standalone (no pipeline re-run). Reads R13_combos_<tag>.rds only.
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

# ---- physics (copied verbatim from the main script L7750-7761) ----
.PM <- new.env()
.PM$speed_metro_max <- 80; .PM$metro_accel_rate <- 1.2
.PM$metro_max_speed_ms <- .PM$speed_metro_max*1000/3600
.PM$metro_time_to_max_speed <- .PM$metro_max_speed_ms/.PM$metro_accel_rate
.PM$metro_dist_to_max_speed_m <- 0.5*.PM$metro_accel_rate*.PM$metro_time_to_max_speed^2
.PM$metro_dist_to_max_speed_km <- .PM$metro_dist_to_max_speed_m/1000
.PM$metro_min_segment_for_cruise <- 2*.PM$metro_dist_to_max_speed_km
.pm_seg_time <- function(seg_m){ if(is.na(seg_m)||seg_m<=0) return(0); seg_km<-seg_m/1000
  if(seg_km >= .PM$metro_min_segment_for_cruise){
    th <- 2*(.PM$metro_time_to_max_speed/3600) + (seg_km - 2*.PM$metro_dist_to_max_speed_km)/.PM$speed_metro_max
  } else { th <- (2*sqrt(2*((seg_km*1000)/2)/.PM$metro_accel_rate))/3600 }
  th*60 }
.pm_metrostr <- function(s){ if(is.na(s)||s=="") return(0); p<-as.numeric(unlist(strsplit(s,";"))); p<-p[!is.na(p)&p>0]
  if(!length(p)) return(0); sum(vapply(p, .pm_seg_time, numeric(1))) }
.pm_agg_metro_speed <- function(dist_km){            # VECTORISED (was a per-element for loop)
  d <- pmax(0.01, dist_km)
  cruise <- d >= .PM$metro_min_segment_for_cruise
  t_hr <- ifelse(cruise,
    (2*.PM$metro_time_to_max_speed/3600) + (d - 2*.PM$metro_dist_to_max_speed_km)/.PM$speed_metro_max,
    (2*sqrt(2*(d*1000/2)/.PM$metro_accel_rate))/3600)
  pmin(pmax(dist_km/t_hr, 20), 80) }
.pm_brt_speed <- function(s, gap, cap) pmin(s + gap*(cap - s), cap)

# ---- per-trip frame: metro-only candidate (mo_*) + best-multimodal candidate (mm_*) ----
# Multimodal re-selection done in base-R (order + !duplicated) — far faster than a
# dplyr group_by+slice over the ~240k (point,anchor,draw) groups, which is the band bottleneck.
.r13pm_frame <- function(combos, mx=50, tol=0.10, pr="default") {
  gk <- c("rp_id","dest_type","clinic_id")
  mo <- combos %>% filter(branch=="metro_only") %>%
    transmute(rp_id,dest_type,clinic_id, road_dist_m,
              mo_l1=l1, mo_l2=l2, mo_l3=l3, mo_nmt=n_metro_tr, mo_nst=n_stops)
  cb <- combos[combos$branch=="multimodal", , drop=FALSE]
  cb$total_adj <- cb$l1 + (cb$l2 + cb$n_metro_tr*(mx-50)) + cb$l3
  cb$prio_use  <- if(pr=="total_only") 0L else cb$prio
  cb <- cb[!is.na(cb$total_adj), , drop=FALSE]
  gid <- paste(cb$rp_id, cb$dest_type, cb$clinic_id, sep="\r")
  o1  <- order(gid, cb$total_adj); cb <- cb[o1,]; gid <- gid[o1]      # per-group min via first
  fi  <- !duplicated(gid); gmin <- setNames(cb$total_adj[fi], gid[fi])
  keep <- cb$total_adj <= gmin[gid]*(1+tol); cb <- cb[keep,]; gid <- gid[keep]
  o2  <- order(gid, cb$prio_use, cb$total_adj); cb <- cb[o2,]; gid <- gid[o2]
  win <- cb[!duplicated(gid), , drop=FALSE]
  # carry the precise metro-segment time from combos$metro_pre (precompute it ONCE on
  # combos before the band loops — recomputing it per frame is the dominant cost).
  mm_pre <- if ("metro_pre" %in% names(win)) win$metro_pre else vapply(ifelse(is.na(win$seg_str),"",win$seg_str), .pm_metrostr, numeric(1))
  mm <- data.frame(rp_id=win$rp_id, dest_type=win$dest_type, clinic_id=win$clinic_id, road_dist_m_mm=win$road_dist_m,
    mm_l1=win$l1, mm_l3=win$l3, mm_type=win$type, mm_dmet=win$d_metro, mm_dbrt=win$d_brt, mm_dstd=win$d_std,
    mm_dwalk=win$d_walk, mm_nmt=win$n_metro_tr, mm_nbt=win$n_bus_tr, mm_nms=win$n_mode_sw, mm_nst=win$n_stops,
    mm_pre=mm_pre, stringsAsFactors=FALSE)
  fr <- full_join(mo, mm, by=gk)
  fr$road_dist_m <- ifelse(is.na(fr$road_dist_m), fr$road_dist_m_mm, fr$road_dist_m); fr$road_dist_m_mm <- NULL
  fr
}

# ---- per-trip travel time for one mode + initiation ----
.r13pm_time <- function(fr, P, s, init, mode) {
  walk<-P$walk; car_acc <- function(km) km/s*60 + P$parking; walk_acc <- function(km) km/walk*60
  if (mode=="Metro-only") {
    l2<-fr$mo_l2/1000; carve<-pmin(fr$mo_nmt*P$metro_xfer_m/1000, l2); ride<-pmax(l2-carve,0)
    tacc <- if(init=="car") car_acc(fr$mo_l1/1000) else walk_acc(fr$mo_l1/1000)
    tacc + ride/.pm_agg_metro_speed(ride)*60 + carve/walk*60 + walk_acc(fr$mo_l3/1000) +
      (fr$mo_nmt+1)*P$metro_wait_mean + P$dwell*pmax(0,fr$mo_nst-1) + fr$mo_nmt*P$pen_metro_tr
  } else {
    bs<-.pm_brt_speed(s,P$brt_gap,P$brt_cap)
    wmm<-pmax(pmax(fr$mm_dwalk-fr$mm_nmt*50,0)*P$transfer_mult + fr$mm_nmt*P$metro_xfer_m,0)/1000
    n_mb<-ifelse(fr$mm_type %in% c("Metro-Metro","Metro-Bus"),1+fr$mm_nmt, ifelse(fr$mm_type=="Bus-Metro",fr$mm_nms+fr$mm_nmt,0))
    n_bb<-ifelse(fr$mm_type=="Bus-Bus",1+fr$mm_nbt, ifelse(fr$mm_type=="Metro-Bus",fr$mm_nms+fr$mm_nbt, ifelse(fr$mm_type=="Bus-Metro",1+fr$mm_nbt,0)))
    tacc <- if(init=="car") car_acc(fr$mm_l1/1000) else walk_acc(fr$mm_l1/1000)
    tacc + fr$mm_pre + (fr$mm_dbrt/1000)/bs*60 + (fr$mm_dstd/1000)/s*60 + wmm/walk*60 +
      n_mb*P$metro_wait_mean + n_bb*P$bus_wait_mean + P$dwell*pmax(0,fr$mm_nst-1) + walk_acc(fr$mm_l3/1000) +
      fr$mm_nmt*P$pen_metro_tr + fr$mm_nbt*P$pen_bus_tr + fr$mm_nms*P$pen_mode_sw
  }
}

# ---- evaluate one param set over the speed grid -> per (anchor, Mode_family, Initiation, speed) ----
# Aggregation via rowsum() (fast C) keyed by a precomputed anchor factor — avoids the
# per-speed dplyr::group_by over the full trip set (the bottleneck for the band loops).
.r13pm_eval <- function(fr, P, speeds) {
  af <- factor(fr$dest_type); ua <- levels(af)
  .agg <- function(num, den) { sn <- rowsum(num, af)[,1]; sd <- rowsum(den, af)[,1]; ifelse(sd > 0, sn/sd, NA_real_) }
  rows <- list()
  for (s in speeds) {
    car <- fr$road_dist_m/1000/s*60 + P$parking; car0 <- ifelse(is.na(car), 0, car); carok <- as.numeric(!is.na(car))
    rows[[length(rows)+1]] <- data.frame(anchor=ua, Mode_family="Car-only (direct to clinic)", Initiation="Car-initiated",
        speed=s, mean_time=.agg(car0, carok), pct=NA_real_, saved=NA_real_, stringsAsFactors=FALSE)
    for (mode in c("Metro-only","Multimodal")) for (init in c("Car-initiated","Walk-initiated")) {
      t <- .r13pm_time(fr, P, s, if (init=="Car-initiated") "car" else "walk", mode)
      reach <- as.numeric(!is.na(t) & !is.na(car))
      t0    <- ifelse(reach==1, t, 0)
      win   <- ifelse(reach==1 & t < car, 1, 0)
      svnum <- ifelse(win==1, car - t, 0)
      rows[[length(rows)+1]] <- data.frame(anchor=ua, Mode_family=mode, Initiation=init, speed=s,
        mean_time=.agg(t0, reach), pct=100*.agg(win, reach), saved=.agg(svnum, win)/60, stringsAsFactors=FALSE)
    }
  }
  bind_rows(rows)
}

# 50%-crossing break-even (manuscript Fig-3 def): speed where pct crosses 50, declining
.r13pm_be50 <- function(ev) {
  ev %>% filter(Mode_family!="Car-only (direct to clinic)") %>%
    group_by(anchor,Mode_family,Initiation) %>%
    summarise(be50 = if (sum(is.finite(pct))>=2 && max(pct,na.rm=TRUE)>=50)
                tryCatch(stats::approx(x=pct,y=speed,xout=50,ties=mean)$y, error=function(e) NA_real_) else NA_real_,
              .groups="drop")
}

# baseline P from the documented defaults (self-contained)
r13pm_BASE <- function() list(walk=4, metro_wait_mean=7.5/2, bus_wait_mean=20/2, dwell=0.5, parking=0,
  pen_metro_tr=0, pen_bus_tr=0, pen_mode_sw=0, transfer_mult=1, brt_gap=0.5, brt_cap=80,
  metro_xfer_m=50, tol_pct=0.10, prio_scheme="default")
invisible(NULL)
