# =============================================================================
#  _r13_tornado_recolor_corrected.R  --  R1-3 assumption tornado on the
#  MM-REACH-CORRECTED routing (reads R13_combos_<tag>_CORRECTED.rds), with the
#  user's two changes vs _r13_tornado_recolor.R:
#    (1) Private / Public shown SEPARATELY (NOT averaged) -> ownership facets.
#    (2) a 10th "Combined" row = the JOINT impact of ALL sweep assumptions varied
#        together on break-even speed (envelope over a joint random sample), vs
#        the other rows which are one-factor-at-a-time (OFAT) swings.
#  Split by mode (Metro-only / Multimodal), 50%-crossing break-even, colour only.
#  Writes Fig_R13_sweep_impact_<tag>_colour_CORRECTED.tiff. Standalone via the engine.
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2) }))
source("Code/_r13_permode.R")
plasma3 <- viridisLite::plasma(3, end=0.9); MODE_COL <- c("Metro-only"=plasma3[1], "Multimodal"=plasma3[2])
.fin_swing <- function(v){ v<-v[is.finite(v)]; if(length(v)>=2) max(v)-min(v) else NA_real_ }
lab <- c(combined="Combined (all sweep assumptions, joint)",
         walk="Walking speed (2-6 km/h)", wait="Peak vs off-peak wait", dwell="Dwell penalty (0.25-1 min/stop)",
         parking="Parking-search penalty (0-15 min)", transfer_mult="Multimodal transfer-walk distance (halved to doubled)",
         brt_gap="Bus Rapid Transit speed (congestion escape)", metro_xfer="Metro line-change penalty (50-200 m)",
         tol_pct="Mode choice: shortest-path tolerance", priority="Mode choice: priority scoring")
type_lab <- c(nearest="Nearest", median="Median", farthest="Farthest", random="Random")

tornado_data_corr <- function(tag) {
  cf <- sprintf("Data/R13_combos_%s_CORRECTED.rds",tag)
  if (!file.exists(cf)) stop("corrected combos not found: ", cf)
  combos <- readRDS(cf); if(!"clinic_id"%in%names(combos)) combos$clinic_id<-NA_character_
  set.seed(7)
  kr <- combos %>% filter(grepl("^random",dest_type)) %>% distinct(rp_id,dest_type,clinic_id) %>% group_by(dest_type) %>% slice_sample(n=12000) %>% ungroup()
  combos <- bind_rows(combos %>% filter(!grepl("^random",dest_type)), combos %>% filter(grepl("^random",dest_type)) %>% semi_join(kr,by=c("rp_id","dest_type","clinic_id")))
  combos$metro_pre <- vapply(ifelse(is.na(combos$seg_str),"",combos$seg_str), .pm_metrostr, numeric(1))
  BASE<-r13pm_BASE(); spd<-seq(5,80,2.5)
  fc<-new.env(); gf<-function(mx,tl,pr){k<-paste(mx,tl,pr);v<-get0(k,fc);if(is.null(v)){v<-.r13pm_frame(combos,mx,tl,pr);assign(k,v,fc)};v}
  be_of<-function(P) .r13pm_be50(.r13pm_eval(gf(P$metro_xfer_m,P$tol_pct,P$prio_scheme),P,spd))
  W<-list(c(5,12.5),c(3.75,10),c(3.75,11.25),c(7.5,22.5))
  vals <- list(walk=c(2,3,4,5,6), wait=W, dwell=c(.25,.5,1), parking=c(0,7.5,15),
               transfer_mult=c(.5,1,1.5,2), brt_gap=c(0,.25,.5,.75,1),
               metro_xfer=c(50,100,150,200), tol_pct=c(.05,.1,.2,.3), priority=c("default","total_only"))
  setp <- function(p, dim, v) {
    if (dim=="wait") { p$metro_wait_mean<-v[1]; p$bus_wait_mean<-v[2] }
    else if (dim=="metro_xfer") p$metro_xfer_m<-v
    else if (dim=="priority")   p$prio_scheme<-v
    else p[[dim]] <- v
    p
  }
  # ---- OFAT sweeps (one dimension at a time), per-anchor (NO ownership averaging) ----
  sweeps <- lapply(names(vals), function(dn) lapply(vals[[dn]], function(v) setp(BASE, dn, v)))
  names(sweeps) <- names(vals)
  ofat <- bind_rows(lapply(names(sweeps), function(dn) bind_rows(lapply(sweeps[[dn]], be_of)) %>% mutate(dimension=dn)))
  ofat <- ofat %>% group_by(dimension, anchor, Mode_family, Initiation) %>%
    summarise(swing=.fin_swing(be50), .groups="drop")
  # ---- COMBINED: joint random sample over ALL sweep dims together -> envelope swing ----
  set.seed(11); Kc <- 40
  jointP <- lapply(seq_len(Kc), function(i) {
    p <- BASE
    for (dn in names(vals)) { vv <- vals[[dn]]; sel <- if (dn=="wait") vv[[sample.int(length(vv),1)]] else vv[sample.int(length(vv),1)]; p <- setp(p, dn, sel) }
    p
  })
  comb <- bind_rows(lapply(jointP, be_of)) %>% group_by(anchor, Mode_family, Initiation) %>%
    summarise(swing=.fin_swing(be50), .groups="drop") %>% mutate(dimension="combined")
  res <- bind_rows(ofat, comb)
  res %>% mutate(type = sub("_(priv|pub)$","",anchor),
                 Ownership = ifelse(grepl("priv",anchor),"Private","Public"),
                 weighting = if(tag=="weighted")"Population-weighted" else "Unweighted")
}

base_theme <- theme_bw(base_size=11) + theme(panel.grid.minor=element_blank(),
  panel.grid.major.x=element_line(color="grey88",linewidth=0.4), panel.grid.major.y=element_blank(), legend.position="bottom",  # no horizontal gridlines
  plot.title=element_text(face="bold", size=18), plot.subtitle=element_text(size=11),
  axis.title.x=element_text(face="bold", size=14), axis.text.x=element_text(size=12),
  legend.title=element_text(face="bold", size=13), legend.text=element_text(size=12),
  strip.text=element_text(face="bold", size=13), strip.background=element_blank(), plot.caption=element_text(hjust=0,size=8.5))

mk_tornado_corr <- function(sw, tag) {
  ord <- sw %>% group_by(dimension) %>% summarise(m=mean(swing,na.rm=TRUE),.groups="drop") %>% arrange(m)
  sw <- sw %>% mutate(dim_lab=factor(lab[dimension],levels=lab[ord$dimension]),
    type=factor(type,levels=names(type_lab),labels=type_lab),
    Ownership=factor(Ownership,levels=c("Private","Public")),
    Mode_family=factor(Mode_family,levels=c("Metro-only","Multimodal")),
    Initiation=factor(Initiation,levels=c("Car-initiated","Walk-initiated")))
  ggplot(sw, aes(swing, dim_lab, fill=Mode_family)) +
    facet_grid(Ownership + Initiation ~ type) +
    geom_col(position=position_dodge(width=0.7), width=0.62, na.rm=TRUE) +
    scale_fill_manual(values=MODE_COL, name="Travel mode",
                      labels=c("Metro-only"="Through metro-only", "Multimodal"="Through metro and/or bus (multimodal)")) +
    scale_x_continuous(expand=expansion(mult=c(0,0.05))) +
    labs(x="Swing in break-even speed (km/h)", y=NULL,
         title="Impact of Modelling-assumption Sweeps on Break-even Speed",
         subtitle="Break-even = speed where 50% of trips become slower than driving.") +
    base_theme
}

if (Sys.getenv("R13TORN_BUILD") != "0")
for (tag in c("unweighted","weighted")) {
  cf <- sprintf("Data/R13_combos_%s_CORRECTED.rds",tag)
  if (!file.exists(cf)) { cat("SKIP", tag, "- corrected combos not found\n"); next }
  cat("=== corrected tornado:", tag, "===\n"); sw <- tornado_data_corr(tag)
  ggsave(sprintf("Data/Fig_R13_sweep_impact_%s_colour_CORRECTED.tiff", tag), mk_tornado_corr(sw,tag),
         width=13, height=15, dpi=300, bg="white", compression="lzw")
  saveRDS(sw, sprintf("Data/R13_sweep_impact_%s_CORRECTED.rds", tag))
  cat("wrote Fig_R13_sweep_impact_", tag, "_colour_CORRECTED.tiff\n", sep="")
}
cat("DONE\n")
