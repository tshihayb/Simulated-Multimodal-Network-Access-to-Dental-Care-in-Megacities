# =============================================================================
#  _r13_tornado_signed_corrected.R -- SIGNED (diverging) version of the R1-3
#  assumption tornado on the MM-REACH-CORRECTED routing. Same swings as
#  _r13_tornado_recolor_corrected.R, but now DIRECTIONAL:
#    * one-factor (OFAT) rows: bar 0 -> (be50 at high end - be50 at low end),
#      so the side of zero shows whether raising the assumption raises(+) or
#      lowers(-) break-even speed.
#    * "Combined (joint)" row: has no single direction (all assumptions vary
#      together), so its bar spans the FULL break-even range across the 40 joint
#      draws RELATIVE TO THE BASELINE scenario -> a real two-sided down/up split.
#  Writes Fig_R13_sweep_impact_<tag>_colour_CORRECTED.tiff (overwrites the
#  magnitude version) + caches R13_sweep_impact_signed_<tag>_CORRECTED.rds.
#  Build guard: R13SIGN_BUILD (set 0 to source funcs only).
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2) }))
Sys.setenv(R13TORN_BUILD="0")                         # source the magnitude module for funcs only
source("Code/_r13_tornado_recolor_corrected.R")       # -> lab, type_lab, MODE_COL, base_theme, .fin_swing, engine (_r13_permode.R)

# ---- ORIGIN-BASED estimand (2026-07-12): align the tornado with Fig 3 (App Fig 18) / App Fig 16
# (App Fig 19). The former code pooled a set.seed(7) 12,000-PAIR subsample of the random target
# (covering only ~7,245/10,000 origins) via .r13pm_eval. The random target's unit of analysis is the
# ORIGIN: each origin's value = its mean over its Monte-Carlo draws, origins equal-weighted -- the same
# two-stage aggregation as .r13pm_eval_origin / App Fig 16. So we now use ALL random trips, per-draw
# expanded (a facility drawn k times counts k) with the 4 weighted phantom origins' nearest_priv
# multimodal masked -- both via .r13pm_expand_perdraw (extracted from the figure engine into a child
# env so it cannot clobber the tornado styling). nearest/median/farthest are 1 trip/origin => unchanged.
# .tor_eval_origin batches all speeds into ONE rowsum per metric (verified bit-exact vs
# .r13pm_eval_origin; the per-speed scalar form is ~120s/eval, this is far faster -- ~75 evals/tag).
.fe_env <- new.env(parent = globalenv())
local({ .owd<-getwd(); on.exit(setwd(.owd)); .ob<-Sys.getenv("R13FIG_BUILD"); Sys.setenv(R13FIG_BUILD="0")
  sys.source("Code/_r13_manuscript_figs_corrected.R", envir=.fe_env); Sys.setenv(R13FIG_BUILD=.ob) })
.r13pm_expand_perdraw <- get(".r13pm_expand_perdraw", .fe_env)

.tor_codes <- function(fr) { af<-factor(fr$dest_type)
  ok_f<-factor(paste(fr$dest_type, fr$rp_id, sep="\r"))
  list(ua=levels(af), oi=as.integer(ok_f), oaf=as.integer(factor(sub("\r.*$","",levels(ok_f)), levels=levels(af)))) }
.tor_aggm <- function(NUM, DEN, oi, oaf, na) {                 # two-stage: per-origin mean(num/den), then per-anchor mean
  SN<-rowsum(NUM,oi); SD<-rowsum(DEN,oi); MU<-SN/SD; MU[!is.finite(MU)]<-NA_real_
  OK<-matrix(as.numeric(is.finite(MU)),nrow(MU)); MU0<-MU; MU0[!is.finite(MU0)]<-0
  N2<-rowsum(MU0,oaf); D2<-rowsum(OK,oaf); R<-N2/D2; R[!is.finite(R)]<-NA_real_
  out<-matrix(NA_real_,na,ncol(R)); out[as.integer(rownames(R)),]<-R; out }
.tor_eval_origin <- function(fr, P, speeds, C) {               # mirrors .r13pm_eval_origin, all speeds vectorised
  ua<-C$ua; oi<-C$oi; oaf<-C$oaf; na<-length(ua); ns<-length(speeds)
  road_km<-fr$road_dist_m/1000; CAR<-sapply(speeds, function(s) road_km/s*60 + P$parking)
  carok<-as.numeric(!is.na(CAR[,1])); CAR0<-CAR; CAR0[is.na(CAR0)]<-0; CARok<-matrix(carok,length(carok),ns)
  car_mt<-.tor_aggm(CAR0,CARok,oi,oaf,na); rows<-list()
  for (j in seq_len(ns)) rows[[length(rows)+1]]<-data.frame(anchor=ua,Mode_family="Car-only (direct to clinic)",
      Initiation="Car-initiated",speed=speeds[j],mean_time=car_mt[,j],pct=NA_real_,saved=NA_real_,stringsAsFactors=FALSE)
  for (mode in c("Metro-only","Multimodal")) for (init in c("Car-initiated","Walk-initiated")) {
    TT<-sapply(speeds, function(s) .r13pm_time(fr,P,s,if(init=="Car-initiated")"car" else "walk",mode))
    RE<-(!is.na(TT))&(!is.na(CAR)); REn<-matrix(as.numeric(RE),nrow(RE)); T0<-TT; T0[!RE]<-0
    WIN<-matrix(as.numeric(RE & TT<CAR),nrow(TT)); SV<-(CAR-TT); SV[!(RE & TT<CAR)]<-0
    mt<-.tor_aggm(T0,REn,oi,oaf,na); pc<-100*.tor_aggm(WIN,REn,oi,oaf,na); sv<-.tor_aggm(SV,WIN,oi,oaf,na)/60
    for (j in seq_len(ns)) rows[[length(rows)+1]]<-data.frame(anchor=ua,Mode_family=mode,Initiation=init,
        speed=speeds[j],mean_time=mt[,j],pct=pc[,j],saved=sv[,j],stringsAsFactors=FALSE)
  }
  bind_rows(rows) }

# ---- signed sweep data (full recompute: OFAT endpoints + baseline + 40 joint draws) ----
tornado_data_signed_corr <- function(tag) {
  cf <- sprintf("Data/R13_combos_%s_CORRECTED.rds",tag)
  if (!file.exists(cf)) stop("corrected combos not found: ", cf)
  combos <- readRDS(cf); if(!"clinic_id"%in%names(combos)) combos$clinic_id<-NA_character_
  # NO subsample: the origin estimand uses ALL random trips (per-draw expanded below).
  combos$metro_pre <- vapply(ifelse(is.na(combos$seg_str),"",combos$seg_str), .pm_metrostr, numeric(1))
  BASE<-r13pm_BASE(); spd<-seq(5,80,2.5)
  # gf: build the per-(mx,tol,pr) frame, expand to per-draw + phantom-mask (origin estimand),
  # precompute origin group codes -- all cached per unique (mx,tol,pr).
  fc<-new.env(); gf<-function(mx,tl,pr){k<-paste(mx,tl,pr);v<-get0(k,fc)
    if(is.null(v)){fr<-.r13pm_expand_perdraw(.r13pm_frame(combos,mx,tl,pr),tag); v<-list(fr=fr,C=.tor_codes(fr)); assign(k,v,fc)};v}
  be_of<-function(P){g<-gf(P$metro_xfer_m,P$tol_pct,P$prio_scheme); .r13pm_be50(.tor_eval_origin(g$fr,P,spd,g$C))}
  W<-list(c(5,12.5),c(3.75,10),c(3.75,11.25),c(7.5,22.5))
  vals <- list(walk=c(2,3,4,5,6), wait=W[order(sapply(W,sum))], dwell=c(.25,.5,1), parking=c(0,7.5,15),   # each ordered low->high
               transfer_mult=c(.5,1,1.5,2), brt_gap=c(0,.25,.5,.75,1),
               metro_xfer=c(50,100,150,200), tol_pct=c(.05,.1,.2,.3), priority=c("default","total_only"))
  setp <- function(p, dim, v) { if(dim=="wait"){p$metro_wait_mean<-v[1];p$bus_wait_mean<-v[2]} else if(dim=="metro_xfer")p$metro_xfer_m<-v else if(dim=="priority")p$prio_scheme<-v else p[[dim]]<-v; p }
  key <- c("anchor","Mode_family","Initiation")
  base_be <- be_of(BASE) %>% select(all_of(key), base=be50)
  # OFAT: evaluate every sweep point, idx 1=low ... n=high
  ofat <- bind_rows(lapply(names(vals), function(dn){
    vv<-vals[[dn]]; n<-length(vv)
    bind_rows(lapply(seq_len(n), function(i) be_of(setp(BASE, dn, if(dn=="wait") vv[[i]] else vv[i])) %>% mutate(idx=i))) %>% mutate(dimension=dn)
  }))
  ofat_s <- ofat %>% group_by(dimension, anchor, Mode_family, Initiation) %>%
    summarise(lo=be50[idx==min(idx)][1], hi=be50[idx==max(idx)][1],
              mn=suppressWarnings(min(be50,na.rm=TRUE)), mx=suppressWarnings(max(be50,na.rm=TRUE)),
              swing=.fin_swing(be50), .groups="drop") %>%
    mutate(mn=ifelse(is.finite(mn),mn,NA_real_), mx=ifelse(is.finite(mx),mx,NA_real_), signed=hi-lo, kind="ofat")
  # Combined: joint random draws over ALL dims -> min/max range
  set.seed(11); Kc<-40
  jointP <- lapply(seq_len(Kc), function(i){p<-BASE; for(dn in names(vals)){vv<-vals[[dn]]; sel<-if(dn=="wait") vv[[sample.int(length(vv),1)]] else vv[sample.int(length(vv),1)]; p<-setp(p,dn,sel)}; p})
  comb <- bind_rows(lapply(jointP, be_of)) %>% group_by(anchor, Mode_family, Initiation) %>%
    summarise(mn=suppressWarnings(min(be50,na.rm=TRUE)), mx=suppressWarnings(max(be50,na.rm=TRUE)), swing=.fin_swing(be50), .groups="drop") %>%
    mutate(mn=ifelse(is.finite(mn),mn,NA_real_), mx=ifelse(is.finite(mx),mx,NA_real_),
           dimension="combined", lo=NA_real_, hi=NA_real_, signed=NA_real_, kind="joint")
  res <- bind_rows(ofat_s, comb) %>% left_join(base_be, by=key) %>%
    mutate(xmin = ifelse(kind=="ofat", pmin(0, signed), mn - base),          # OFAT: 0->signed (direction); joint: range vs baseline
           xmax = ifelse(kind=="ofat", pmax(0, signed), mx - base),
           nonmono = kind=="ofat" & is.finite(swing) & swing>0.5 & abs(abs(signed)-swing)>0.5,  # endpoints miss the range
           type = sub("_(priv|pub)$","",anchor),
           Ownership = ifelse(grepl("priv",anchor),"Private","Public"),
           weighting = if(tag=="weighted")"Population-weighted" else "Unweighted")
  res
}

# ---- signed (diverging) plot; manual mode-dodge via geom_rect so joint bars can straddle 0 ----
mk_tornado_signed <- function(sw, tag) {
  # Restore direction where a sweep ENDPOINT had no break-even (signed=NA) but the range (swing) is finite
  # (mainly Walk-initiated walking-speed, whose slow end never crosses 50%). Impute the dimension's
  # CONSISTENT non-zero sign x swing (sweeps are monotone). Mixed-sign dims (mode-choice) stay blank.
  ds  <- sw %>% filter(kind=="ofat", is.finite(signed), signed!=0) %>% group_by(dimension) %>%
    summarise(d=ifelse(all(signed>0),1,ifelse(all(signed<0),-1,NA_real_)), .groups="drop")
  dsv <- setNames(ds$d, ds$dimension)
  sw <- sw %>% mutate(
    signed_use = ifelse(is.finite(signed), signed, ifelse(is.finite(swing), as.numeric(dsv[dimension])*swing, NA_real_)),
    xmin = ifelse(kind=="ofat", pmin(0, signed_use), xmin),
    xmax = ifelse(kind=="ofat", pmax(0, signed_use), xmax))
  ord <- sw %>% group_by(dimension) %>% summarise(m=mean(swing,na.rm=TRUE),.groups="drop") %>% arrange(m)
  lv  <- unname(lab[ord$dimension])
  sw <- sw %>% mutate(dim_lab=factor(unname(lab[dimension]), levels=lv),
    type=factor(type,levels=names(type_lab),labels=type_lab),
    Ownership=factor(Ownership,levels=c("Private","Public")),
    Mode_family=factor(Mode_family,levels=c("Metro-only","Multimodal")),
    Initiation=factor(Initiation,levels=c("Car-initiated","Walk-initiated")),
    ynum=as.integer(dim_lab), off=ifelse(Mode_family=="Metro-only", 0.19, -0.19))
  h <- 0.17
  ggplot(sw) +
    facet_grid(Ownership + Initiation ~ type) +
    geom_vline(xintercept=0, color="grey40", linewidth=0.5) +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ynum+off-h, ymax=ynum+off+h, fill=Mode_family), na.rm=TRUE) +
    scale_fill_manual(values=MODE_COL, name="Travel mode",
                      labels=c("Metro-only"="Through metro-only","Multimodal"="Through metro and/or bus (multimodal)")) +
    scale_y_continuous(breaks=seq_along(lv), labels=lv, expand=expansion(add=0.6)) +
    scale_x_continuous(expand=expansion(mult=0.04)) +
    labs(x="Change in break-even speed (km/h)   (left = lowers, right = raises)", y=NULL,
         title="Impact of Modelling-assumption Sweeps on Break-even Speed",
         subtitle="Break-even = speed where 50% of trips become slower than driving.") +
    base_theme
}

if (Sys.getenv("R13SIGN_BUILD") != "0")
for (tag in c("unweighted","weighted")) {
  cf <- sprintf("Data/R13_combos_%s_CORRECTED.rds",tag)
  if (!file.exists(cf)) { cat("SKIP", tag, "- corrected combos not found\n"); next }
  cat("=== signed tornado:", tag, "===\n"); sw <- tornado_data_signed_corr(tag)
  saveRDS(sw, sprintf("Data/R13_sweep_impact_signed_%s_CORRECTED.rds", tag))
  ggsave(sprintf("Data/Fig_R13_sweep_impact_%s_colour_CORRECTED.tiff", tag), mk_tornado_signed(sw,tag),
         width=13, height=15, dpi=300, bg="white", compression="lzw")
  nm <- sw %>% filter(nonmono) %>% distinct(dimension, type, Ownership)
  cat("wrote signed", tag, "- non-monotone OFAT flags:", nrow(nm), "\n")
}
cat("DONE\n")
