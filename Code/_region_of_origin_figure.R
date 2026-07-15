setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({library(dplyr); library(ggplot2); library(patchwork)}))
SC <- "C:/Users/Tshih/AppData/Local/Temp/claude/C--Users-Tshih-OneDrive-Claude-code-projects-Simulation-of-transit-access-to-dental-facilities/e1c3ca52-4d12-4ccc-abd4-9d0074581d13/scratchpad/"  # PNG proof only
DD <- "Data/"   # persisted intermediates

direct <- readRDS(paste0(DD,"_region_direct_metrics.rds"))   # region,target,sector,n,n_dist,n_multi,distance,available,bestmulti
be     <- readRDS(paste0(DD,"_region_breakeven.rds"))        # region,target,sector,Initiation,breakeven (MULTIMODAL, both initiations)
be_car  <- be %>% filter(Initiation=="Car-initiated")  %>% transmute(region,target,sector, be_car=breakeven)
be_walk <- be %>% filter(Initiation=="Walk-initiated") %>% transmute(region,target,sector, be_walk=breakeven)
d <- direct %>% left_join(be_car,  by=c("region","target","sector")) %>%
                left_join(be_walk, by=c("region","target","sector"))

# region order by private nearest-facility distance (best->worst); weighted n per region (origins)
nord <- direct %>% filter(target=="Nearest", sector=="Private") %>% arrange(distance)
reglev <- as.character(nord$region)
nlab <- setNames(nord$n, as.character(nord$region))
d$region <- factor(d$region, levels=reglev)
d$target <- factor(d$target, levels=c("Nearest","Median","Farthest","Random"))
d$sector <- factor(d$sector, levels=c("Private","Public"))
xlabs <- setNames(sprintf("%s\n(%s)", reglev, format(nlab[reglev], big.mark=",")), reglev)
# best-mode row: contributing n (transit-available origins) differs by sector at the NEAREST target ->
# stack the two counts under each region; parentheses bracket the pair, private over public (see caption)
bmn <- transform(direct[direct$target=="Nearest", c("region","sector","n_multi")], region=as.character(region))
gpn <- function(r,s) bmn$n_multi[bmn$region==r & bmn$sector==s]
nearlab <- setNames(vapply(reglev, function(r) sprintf("%s\n(%s\n%s)", r,
             format(gpn(r,"Private"), big.mark=",", trim=TRUE),
             format(gpn(r,"Public"),  big.mark=",", trim=TRUE)), character(1)), reglev)
# region names only (used for the Nearest break-even facet, which is "no break-even")
xlabs_plain <- setNames(reglev, reglev)
# multimodal-REACHABLE effective count per region x target x sector (= availability x region n):
# the break-even's actual reach set. Shown stacked private/public under the break-even rows.
direct$n_reach <- round(direct$n * direct$available/100)
.reachlab <- function(tg){ sub <- direct[direct$target==tg,]; gp <- function(r,s) sub$n_reach[sub$region==r & sub$sector==s]
  setNames(vapply(reglev, function(r) sprintf("%s\n(%s\n%s)", r, format(gp(r,"Private"),big.mark=",",trim=TRUE),
    format(gp(r,"Public"),big.mark=",",trim=TRUE)), character(1)), reglev) }
reach_scales <- function() ggh4x::facetted_pos_scales(x=list(
  scale_x_discrete(labels=xlabs_plain),           # Nearest: no break-even -> region names only
  scale_x_discrete(labels=.reachlab("Median")),
  scale_x_discrete(labels=.reachlab("Farthest")),
  scale_x_discrete(labels=.reachlab("Random"))))

# plasma tones to match App Figs 18-19 (and main Figs 2-4): viridisLite::plasma(3, end=0.9)[1:2]
# Private = plasma[1] dark indigo (#0D0887), Public = plasma[2] magenta (#BF3984); Private stays the darker tone
COLS <- function(grey=FALSE) if(grey) c(Private="grey35", Public="grey68") else c(Private="#0D0887", Public="#BF3984")

mkrow <- function(metric, ylab, cols, freey=FALSE, ylim=NULL, strip=FALSE, showx=FALSE, legend=FALSE, be=FALSE, nearlab=NULL, xl=xlabs, xfacet=NULL){
  p <- ggplot(d, aes(region, .data[[metric]], fill=sector)) +
    geom_col(position=position_dodge2(width=0.9, preserve="single"), width=0.78, na.rm=TRUE)
  if(be) p <- p + geom_text(data=data.frame(target=factor("Nearest",levels=levels(d$target))),
      aes(x=3, y=11.5, label="no break-even"), inherit.aes=FALSE, size=2.5, fontface="italic", colour="grey45")
  sc <- if(freey && (!is.null(nearlab)||!is.null(xfacet))) "free" else if(freey) "free_y" else if(!is.null(nearlab)||!is.null(xfacet)) "free_x" else "fixed"
  p <- p + facet_wrap(~target, nrow=1, scales=sc) +
    scale_fill_manual(values=cols, name=NULL)
  if(!is.null(ylim)) p <- p + coord_cartesian(ylim=ylim)
  xsc <- if(!is.null(xfacet)) list(xfacet)
    else if(is.null(nearlab)) list(scale_x_discrete(labels=xl))
    else list(ggh4x::facetted_pos_scales(x=list(scale_x_discrete(labels=nearlab),
         scale_x_discrete(labels=xlabs), scale_x_discrete(labels=xlabs), scale_x_discrete(labels=xlabs))))
  p + labs(y=ylab, x=NULL) + xsc +
    guides(fill = if(legend) guide_legend(keywidth=unit(0.5,"cm"), keyheight=unit(0.35,"cm")) else "none") +
    theme_bw(base_size=11) +
    theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
          panel.border=element_rect(colour="grey80"),
          strip.background=element_blank(),
          strip.text=if(strip) element_text(face="bold", size=10.5) else element_blank(),
          axis.text.x=if(showx) element_text(size=8) else element_blank(),
          axis.ticks.x=if(showx) element_line(colour="grey70") else element_blank(),
          axis.text.y=element_text(size=7.5), axis.title.y=element_text(face="bold", size=8.5),
          legend.position=if(legend) "top" else "none", legend.margin=margin(0,0,0,0),
          legend.text=element_text(size=9), plot.margin=margin(7,7,7,5),
          panel.spacing=unit(0.45,"cm"))
}
build <- function(grey=FALSE){ cols <- COLS(grey)
  mkrow("distance","Distance to\nfacility (km)", cols, freey=TRUE, strip=TRUE, legend=TRUE, showx=TRUE) /
  mkrow("available","Multimodal\navailable (%)", cols, ylim=c(0,100), showx=TRUE) /
  mkrow("bestmulti","Best mode: multimodal\n(% of available origins)", cols, ylim=c(0,100), showx=TRUE, nearlab=nearlab) /
  mkrow("be_car","Multimodal break-even,\ncar-initiated (km/h)", cols, ylim=c(0,23), showx=TRUE, be=TRUE, nearlab=nearlab) /
  mkrow("be_walk","Multimodal break-even,\nwalk-initiated (km/h)", cols, ylim=c(0,15), showx=TRUE, be=TRUE, nearlab=nearlab) +
  plot_annotation(
    title="Spatial Accessibility to Dental Facilities by Region of Origin (Population-Weighted)",
    theme=theme(plot.title=element_text(face="bold", size=12)))
}
ggsave("Data/Fig_region_of_origin_weighted_CORRECTED.tiff",      build(FALSE), width=9.6, height=13, dpi=300, bg="white", compression="lzw")
ggsave(paste0(SC,"Fig_region_origin_weighted_mako.png"),         build(FALSE), width=9.6, height=13, dpi=150, bg="white")
ggsave("Data/Fig_region_of_origin_weighted_CORRECTED_grey.tiff", build(TRUE),  width=9.6, height=13, dpi=300, bg="white", compression="lzw")
cat("done; region order:", paste(reglev, collapse=", "), "\n")
