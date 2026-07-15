# =============================================================================
#  _r13_manuscript_figs.R  --  rebuilt Figs 2/3/4 (mean travel time / % faster /
#  mean time saved), UNWEIGHTED + WEIGHTED, COLOUR + GREYSCALE, with:
#   - all 8 anchors (Nearest/Median/Farthest/Random x Private/Public)
#   - 5-95% Monte-Carlo simulation interval per mode x initiation (mode colour)
#   - 50%-crossing break-even (manuscript Fig-3 definition) on Fig 3
#   - "Transit Initiation" legend, no gridlines
#  Standalone from R13_combos_<tag>.rds via the _r13_permode.R engine (no pipeline run).
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2) }))
source("Code/_r13_permode.R")

plasma3 <- viridisLite::plasma(3, end = 0.9)
PAL <- list(
  colour = c("Car-only (direct to clinic)"="grey50", "Metro-only"=plasma3[1], "Multimodal"=plasma3[2]),
  grey   = c("Car-only (direct to clinic)"="grey62", "Metro-only"="grey20",    "Multimodal"="grey48"))
amap <- tibble::tribble(~anchor,~Type,~Target,
  "nearest_priv","Private","Nearest","median_priv","Private","Specific","farthest_priv","Private","Farthest","random_priv","Private","Random",
  "nearest_pub","Public","Nearest","median_pub","Public","Specific","farthest_pub","Public","Farthest","random_pub","Public","Random")
TARGET_LV <- c("Nearest","Specific","Farthest","Random")
tlab <- labeller(Target = c(Nearest="Nearest", Specific="Median-distance", Farthest="Farthest", Random="Random"))
base_theme <- theme_bw(base_size=14) + theme(
  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
  axis.line=element_blank(), axis.ticks=element_blank(), legend.position="bottom", legend.box="horizontal",
  plot.title=element_text(face="bold"), plot.subtitle=element_text(size=10), axis.title=element_text(face="bold"),
  legend.title=element_text(face="bold"), strip.text=element_text(face="bold"), strip.background=element_blank(),
  panel.spacing.x=unit(1,"lines"), panel.spacing.y=unit(1.4,"lines"), legend.key.width=unit(1.4,"cm"))

# ---- engine: baseline + 5-95% MC band per (anchor,Mode_family,Initiation,speed) ----
r13_fig_data <- function(tag, K=60, n_rand=12000, seed=7, spd=seq(5,80,2.5)) {
  combos <- readRDS(sprintf("Data/R13_combos_%s.rds", tag)); if(!"clinic_id"%in%names(combos)) combos$clinic_id<-NA_character_
  set.seed(seed)
  kr <- combos %>% filter(grepl("^random",dest_type)) %>% distinct(rp_id,dest_type,clinic_id) %>%
        group_by(dest_type) %>% slice_sample(n=n_rand) %>% ungroup()
  combos <- bind_rows(combos %>% filter(!grepl("^random",dest_type)),
                      combos %>% filter(grepl("^random",dest_type)) %>% semi_join(kr,by=c("rp_id","dest_type","clinic_id")))
  combos$metro_pre <- vapply(ifelse(is.na(combos$seg_str),"",combos$seg_str), .pm_metrostr, numeric(1))
  BASE <- r13pm_BASE()
  fcache<-new.env(); getfr<-function(mx,tl,pr){k<-paste(mx,tl,pr);v<-get0(k,fcache);if(is.null(v)){v<-.r13pm_frame(combos,mx,tl,pr);assign(k,v,fcache)};v}
  collect<-function(Pl) bind_rows(lapply(Pl,function(P) .r13pm_eval(getfr(P$metro_xfer_m,P$tol_pct,P$prio_scheme),P,spd)))
  base_ev <- .r13pm_eval(getfr(50,0.10,"default"), BASE, spd)
  MC<-list(walk=c(2,3,4,5,6),dwell=c(.25,.5,1),tm=c(.5,1,1.5,2),brt=c(0,.25,.5,.75,1),pk=c(0,7.5,15),
           wait=list(c(5,12.5),c(3.75,10),c(3.75,11.25),c(7.5,22.5)),pen=list(c(0,0,0),c(1,2,3),c(2,4,6)))
  mcP<-lapply(1:K,function(i){w<-MC$wait[[sample.int(4,1)]];pn<-MC$pen[[sample.int(3,1)]];modifyList(BASE,list(
    walk=sample(MC$walk,1),dwell=sample(MC$dwell,1),transfer_mult=sample(MC$tm,1),brt_gap=sample(MC$brt,1),parking=sample(MC$pk,1),
    metro_wait_mean=w[1],bus_wait_mean=w[2],pen_metro_tr=pn[1],pen_bus_tr=pn[2],pen_mode_sw=pn[3]))})
  mc <- collect(mcP) %>% group_by(anchor,Mode_family,Initiation,speed) %>%
    summarise(tt_lo=quantile(mean_time,.05,na.rm=T), tt_hi=quantile(mean_time,.95,na.rm=T),
              pct_lo=quantile(pct,.05,na.rm=T),     pct_hi=quantile(pct,.95,na.rm=T),
              sv_lo=quantile(saved,.05,na.rm=T),    sv_hi=quantile(saved,.95,na.rm=T), .groups="drop")
  fac <- function(x) x %>% mutate(Mode_family=factor(Mode_family,levels=c("Car-only (direct to clinic)","Metro-only","Multimodal")),
    Initiation=factor(Initiation,levels=c("Car-initiated","Walk-initiated")),
    Target=factor(Target,levels=TARGET_LV), Type=factor(Type,levels=c("Private","Public")))
  d  <- base_ev %>% left_join(mc,by=c("anchor","Mode_family","Initiation","speed")) %>% left_join(amap,by="anchor") %>% fac()
  be <- .r13pm_be50(base_ev) %>% left_join(amap,by="anchor") %>% fac()
  list(d=d, be=be)
}

.lt <- scale_linetype_manual(values=c("Car-initiated"="solid","Walk-initiated"="longdash"))
.gl <- guides(color=guide_legend(override.aes=list(linewidth=1.3)), linetype=guide_legend(override.aes=list(linewidth=1.3)))
WT <- function(tag) if (tag=="weighted") "Weighted " else ""

mk_fig2 <- function(fd, tag, grey=FALSE) {
  pal <- if(grey) PAL$grey else PAL$colour; band <- fd$d %>% filter(Mode_family!="Car-only (direct to clinic)")
  ggplot(fd$d, aes(speed, mean_time/60)) +
    geom_ribbon(data=band, aes(x=speed,ymin=tt_lo/60,ymax=tt_hi/60,fill=Mode_family,group=interaction(Mode_family,Initiation)),alpha=0.30,inherit.aes=FALSE,show.legend=FALSE) +
    geom_line(aes(color=Mode_family,linetype=Initiation),linewidth=1.0) +
    facet_grid(Type~Target,scales="free_y",labeller=tlab) + scale_color_manual(values=pal)+scale_fill_manual(values=pal)+.lt+
    scale_x_continuous(limits=c(5,80),breaks=seq(10,80,10)) +
    labs(title=sprintf("Mean Travel Time from %sRandom Point to Dental Facility by Target and Sector",WT(tag)),
         subtitle="Shaded band = 5-95% Monte-Carlo simulation interval across modelling assumptions (per mode & initiation).",
         x="Average speed for car/standard bus (km/h)", y="Mean travel time (hours)", color="Travel mode", linetype="Transit Initiation") +
    base_theme + .gl
}
mk_fig3 <- function(fd, tag, grey=FALSE) {
  pal <- if(grey) PAL$grey else PAL$colour
  dd <- fd$d %>% filter(Mode_family!="Car-only (direct to clinic)")
  bks <- fd$be %>% filter(Initiation=="Car-initiated", is.finite(be50))
  ggplot(dd, aes(speed, pct)) +
    geom_hline(yintercept=50, color="grey80", linetype="dotted") +
    geom_ribbon(aes(x=speed,ymin=pct_lo,ymax=pct_hi,fill=Mode_family,group=interaction(Mode_family,Initiation)),alpha=0.30,inherit.aes=FALSE,show.legend=FALSE) +
    geom_vline(data=bks, aes(xintercept=be50,color=Mode_family),linetype="dashed",linewidth=0.4,alpha=0.6,inherit.aes=FALSE,show.legend=FALSE) +
    geom_line(aes(color=Mode_family,linetype=Initiation),linewidth=1.0) +
    facet_grid(Type~Target,labeller=tlab) + scale_color_manual(values=pal)+scale_fill_manual(values=pal)+.lt+
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20))+scale_x_continuous(limits=c(5,80),breaks=seq(10,80,10)) +
    labs(title=sprintf("Percentage of %sPublic Transit Trips Faster than Direct Driving by Target and Sector",WT(tag)),
         subtitle="Dashed vertical = break-even (speed where 50% of car-initiated trips become slower than driving). Band = 5-95% MC interval.",
         x="Average speed for car/standard bus (km/h)", y="Percentage faster than car (%)", color="Travel mode", linetype="Transit Initiation") +
    base_theme + .gl
}
.smooth <- function(x,y){ ok<-is.finite(x)&is.finite(y); if(sum(ok)<5) return(y); fit<-tryCatch(stats::loess(y[ok]~x[ok],span=0.5),error=function(e)NULL); if(is.null(fit)) return(y); out<-rep(NA_real_,length(y)); out[ok]<-stats::predict(fit); out }
mk_fig4 <- function(fd, tag, grey=FALSE, variant="raw") {
  pal <- if(grey) PAL$grey else PAL$colour
  dd <- fd$d %>% filter(Mode_family!="Car-only (direct to clinic)")
  sub <- "Mean over trips where transit beats driving. Band = 5-95% MC interval (per mode & initiation)."
  if (variant=="capped") {
    dd <- dd %>% mutate(across(c(saved,sv_lo,sv_hi), ~ifelse(pct>=2, .x, NA_real_)))
    sub <- paste(sub, "Curves capped where <2% of trips qualify (conditional mean unstable).")
  } else if (variant=="smooth") {
    dd <- dd %>% group_by(anchor,Mode_family,Initiation) %>% arrange(speed) %>%
      mutate(saved=.smooth(speed,saved), sv_lo=.smooth(speed,sv_lo), sv_hi=.smooth(speed,sv_hi)) %>% ungroup()
    sub <- paste(sub, "Loess-smoothed (span 0.5).")
  }
  ggplot(dd, aes(speed, saved)) +
    geom_ribbon(aes(x=speed,ymin=sv_lo,ymax=sv_hi,fill=Mode_family,group=interaction(Mode_family,Initiation)),alpha=0.30,inherit.aes=FALSE,show.legend=FALSE) +
    geom_line(aes(color=Mode_family,linetype=Initiation),linewidth=1.0,na.rm=TRUE) +
    facet_grid(Type~Target,scales="free_y",labeller=tlab) + scale_color_manual(values=pal)+scale_fill_manual(values=pal)+.lt+
    scale_x_continuous(limits=c(5,80),breaks=seq(10,80,10)) +
    labs(title=sprintf("Mean Time Saved by %sTransit over Direct Driving by Target and Sector",WT(tag)),
         subtitle=sub, x="Average speed for car/standard bus (km/h)", y="Mean time saved (hours)", color="Travel mode", linetype="Transit Initiation") +
    base_theme + .gl
}

if (Sys.getenv("R13FIG_BUILD") != "0")
for (tag in c("unweighted","weighted")) {
  cat("=== building figures:", tag, "===\n"); fd <- r13_fig_data(tag)
  sfx <- if (tag=="weighted") "_weighted" else ""
  sv <- function(base, plt) { ggsave(file.path("Data",paste0(base,".tiff")), plt(FALSE), width=16,height=9,dpi=300,bg="white",compression="lzw")
    if (tag!="weighted") ggsave(file.path("Data",paste0(base,"_grey.tiff")), plt(TRUE), width=16,height=9,dpi=300,bg="white",compression="lzw"); cat("wrote",base,"\n") }
  sv(sprintf("Fig_mean_travel_time%s_ALL8_MCband",sfx),       function(g) mk_fig2(fd,tag,g))
  sv(sprintf("Fig_percent_faster_integrated%s_MCband",sfx),   function(g) mk_fig3(fd,tag,g))
  for (v in c("raw","capped","smooth"))                       # Fig 4: three variants for the user to choose
    sv(sprintf("Fig_mean_time_savings%s_MCband_%s",sfx,v),    (function(vv) function(g) mk_fig4(fd,tag,g,vv))(v))
}
cat("DONE\n")
