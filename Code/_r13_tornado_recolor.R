# =============================================================================
#  _r13_tornado_recolor.R  --  R1-3 assumption tornado, RECOMPUTED on the
#  50%-crossing break-even (manuscript Fig-3 def), SPLIT BY MODE (Metro-only /
#  Multimodal, so BRT & transfer-walk show real bars), Figs 2-4 mode palette,
#  colour only (appendix). Per assumption: OFAT-sweep -> break-even swing per
#  (anchor type x mode x initiation), ownership averaged. Standalone via the engine.
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2) }))
source("Code/_r13_permode.R")
plasma3 <- viridisLite::plasma(3, end=0.9); MC <- c("Metro-only"=plasma3[1], "Multimodal"=plasma3[2])
amap <- tibble::tribble(~anchor,~Type,~Target,
  "nearest_priv","Private","Nearest","median_priv","Private","Specific","farthest_priv","Private","Farthest","random_priv","Private","Random",
  "nearest_pub","Public","Nearest","median_pub","Public","Specific","farthest_pub","Public","Farthest","random_pub","Public","Random")
.fin_swing <- function(v){ v<-v[is.finite(v)]; if(length(v)>=2) max(v)-min(v) else NA_real_ }
lab <- c(walk="Walking speed (2-6 km/h)", wait="Peak vs off-peak wait", dwell="Dwell penalty (0.25-1 min/stop)",
         parking="Parking-search penalty (0-15 min)", transfer_mult="Multimodal transfer-walk (x0.5-2)",
         brt_gap="BRT speed (congestion escape)", metro_xfer="Metro line-change penalty (50-200 m)",
         tol_pct="Mode choice: shortest-path tolerance", priority="Mode choice: priority scoring")
type_lab <- c(nearest="Nearest", median="Median", farthest="Farthest", random="Random")

tornado_data <- function(tag) {
  combos <- readRDS(sprintf("Data/R13_combos_%s.rds",tag)); if(!"clinic_id"%in%names(combos)) combos$clinic_id<-NA_character_
  set.seed(7)
  kr <- combos %>% filter(grepl("^random",dest_type)) %>% distinct(rp_id,dest_type,clinic_id) %>% group_by(dest_type) %>% slice_sample(n=12000) %>% ungroup()
  combos <- bind_rows(combos %>% filter(!grepl("^random",dest_type)), combos %>% filter(grepl("^random",dest_type)) %>% semi_join(kr,by=c("rp_id","dest_type","clinic_id")))
  combos$metro_pre <- vapply(ifelse(is.na(combos$seg_str),"",combos$seg_str), .pm_metrostr, numeric(1))
  BASE<-r13pm_BASE(); spd<-seq(5,80,2.5)
  fc<-new.env(); gf<-function(mx,tl,pr){k<-paste(mx,tl,pr);v<-get0(k,fc);if(is.null(v)){v<-.r13pm_frame(combos,mx,tl,pr);assign(k,v,fc)};v}
  be_of<-function(P) .r13pm_be50(.r13pm_eval(gf(P$metro_xfer_m,P$tol_pct,P$prio_scheme),P,spd))
  W<-list(c(5,12.5),c(3.75,10),c(3.75,11.25),c(7.5,22.5))
  sweeps<-list(
    walk=lapply(c(2,3,4,5,6),function(v){p<-BASE;p$walk<-v;p}),
    wait=lapply(W,function(w){p<-BASE;p$metro_wait_mean<-w[1];p$bus_wait_mean<-w[2];p}),
    dwell=lapply(c(.25,.5,1),function(v){p<-BASE;p$dwell<-v;p}),
    parking=lapply(c(0,7.5,15),function(v){p<-BASE;p$parking<-v;p}),
    transfer_mult=lapply(c(.5,1,1.5,2),function(v){p<-BASE;p$transfer_mult<-v;p}),
    brt_gap=lapply(c(0,.25,.5,.75,1),function(v){p<-BASE;p$brt_gap<-v;p}),
    metro_xfer=lapply(c(50,100,150,200),function(v){p<-BASE;p$metro_xfer_m<-v;p}),
    tol_pct=lapply(c(.05,.1,.2,.3),function(v){p<-BASE;p$tol_pct<-v;p}),
    priority=lapply(c("default","total_only"),function(v){p<-BASE;p$prio_scheme<-v;p}))
  res <- bind_rows(lapply(names(sweeps), function(dn) bind_rows(lapply(sweeps[[dn]], be_of)) %>% mutate(dimension=dn)))
  res$type <- sub("_(priv|pub)$","",res$anchor)
  res %>% group_by(dimension,anchor,type,Mode_family,Initiation) %>% summarise(swing=.fin_swing(be50),.groups="drop") %>%
    group_by(dimension,type,Mode_family,Initiation) %>% summarise(swing=mean(swing,na.rm=TRUE),.groups="drop") %>%
    mutate(weighting = if(tag=="weighted")"Population-weighted" else "Unweighted")
}

base_theme <- theme_bw(base_size=12) + theme(panel.grid.minor=element_blank(),
  panel.grid.major=element_line(color="grey88",linewidth=0.4), legend.position="bottom",
  plot.title=element_text(face="bold"), axis.title=element_text(face="bold"), legend.title=element_text(face="bold"),
  strip.text=element_text(face="bold"), strip.background=element_blank(), plot.caption=element_text(hjust=0,size=8.5))

mk_tornado <- function(sw, tag) {
  ord <- sw %>% group_by(dimension) %>% summarise(m=mean(swing,na.rm=TRUE),.groups="drop") %>% arrange(m)
  sw <- sw %>% mutate(dim_lab=factor(lab[dimension],levels=lab[ord$dimension]),
    type=factor(type,levels=names(type_lab),labels=type_lab),
    Mode_family=factor(Mode_family,levels=c("Metro-only","Multimodal")),
    Initiation=factor(Initiation,levels=c("Car-initiated","Walk-initiated")))
  ggplot(sw, aes(swing, dim_lab, fill=Mode_family)) +
    facet_grid(Initiation ~ type) +
    geom_col(position=position_dodge(width=0.7), width=0.62, na.rm=TRUE) +
    scale_fill_manual(values=MC, name="Travel mode") +
    scale_x_continuous(expand=expansion(mult=c(0,0.05))) +
    labs(x="Swing in break-even speed (km/h) across the swept range", y=NULL,
         title=sprintf("Impact of modelling-assumption sweeps on break-even speed (%s)", if(tag=="weighted")"Population-weighted" else "Unweighted"),
         subtitle="Break-even = speed where 50% of trips become slower than driving. Rows: transit access mode; columns: anchor; bars: transit mode.",
         caption="Ownership (priv/pub) averaged. Traffic speed (5-80) is the break-even axis, not a bar. NEAREST often has no break-even (blank).") +
    base_theme
}

if (Sys.getenv("R13TORN_BUILD") != "0")
for (tag in c("unweighted","weighted")) {
  cat("=== tornado:", tag, "===\n"); sw <- tornado_data(tag)
  ggsave(sprintf("Data/Fig_R13_sweep_impact_%s_colour.tiff", tag), mk_tornado(sw,tag), width=13, height=8, dpi=300, bg="white", compression="lzw")
  cat("wrote Fig_R13_sweep_impact_%s_colour.tiff\n", tag)
}
cat("DONE\n")
