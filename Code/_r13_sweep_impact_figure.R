# Tornado of the R1-3 modelling-assumption sweeps, FACETED BY ANCHOR TYPE x
# INITIATION. Each bar = swing in break-even speed (km/h) across an assumption's
# swept range. Rows = Car-initiated (manuscript Fig 3/4 basis) / Walk-initiated;
# columns = Nearest / Median / Farthest / Random (ownership priv+pub averaged);
# dodged bars = Unweighted vs Population-weighted.
# Reads CHEAP R13_sensitivity_{tag}.rds (walk now; car after the post-run
# recompute) + ENHANCED R13_enh_sensitivity_{tag}.rds (when present). Re-run after
# the enhanced job + recompute to populate the car-initiated row and enhanced bars.
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2) }))

base_theme <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
        legend.position = "bottom", legend.box = "horizontal",
        plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        strip.background = element_blank(), legend.key.width = unit(1.2, "cm"))
pal2 <- viridisLite::plasma(2, end = 0.7)          # the two ownerships (priv/pub)

.fin_swing <- function(v){ v<-v[is.finite(v)]; if(length(v)>=2) max(v)-min(v) else NA_real_ }
wt_lab <- c(unweighted="Unweighted", weighted="Population-weighted")
lab <- c(walk="Walking speed (2-6 km/h)",
         wait="Peak vs off-peak wait",
         dwell="Dwell penalty (0.25-1 min/stop)",
         parking="Parking-search penalty (0-15 min)",
         transfer_mult="Multimodal transfer-walk (x0.5-2)",
         brt_gap="BRT speed (congestion escape, gap 0-1)",
         metro_xfer="Metro line-change penalty (50-200 m)",
         tol_pct="Mode choice: shortest-path tolerance",
         priority="Mode choice: priority scoring",
         transfer_penalty="Per-type transfer penalty")
type_lv <- c("nearest","median","farthest","random")
type_lab<- c("Nearest","Median","Farthest","Random")

collect <- function(tag) {
  rows <- list()
  for (src in c("R13_sensitivity","R13_enh_sensitivity")) {
    f <- sprintf("Data/%s_%s.rds", src, tag); if (!file.exists(f)) next
    sw <- readRDS(f)$sweep
    wcol <- if (src=="R13_sensitivity") "be_metro_walk" else "be_best_walk"
    ccol <- if (src=="R13_sensitivity") "be_metro_car"  else "be_best_car"
    for (ini in c("Car-initiated","Walk-initiated")) {
      col <- if (ini=="Walk-initiated") wcol else ccol
      if (!col %in% names(sw)) next
      rows[[paste(src,ini)]] <- transmute(sw, dimension, scenario, anchor,
                                           be = .data[[col]], initiation = ini)
    }
  }
  if (!length(rows)) return(NULL)
  bind_rows(rows) %>% mutate(weighting = wt_lab[[tag]])
}
df0 <- bind_rows(lapply(c("unweighted","weighted"), collect))
df0 <- df0 %>% mutate(type = sub("_(priv|pub)$","",anchor),
                      ownership = ifelse(grepl("priv$", anchor), "Private", "Public")) %>%
  filter(dimension %in% names(lab))

# swing across scenarios, kept SEPARATE by ownership (priv/pub)
sw <- df0 %>% group_by(dimension, type, ownership, weighting, initiation) %>%
        summarise(swing = .fin_swing(be), .groups="drop")

ord <- sw %>% group_by(dimension) %>% summarise(m=mean(swing,na.rm=TRUE), .groups="drop") %>% arrange(m)
sw <- sw %>% mutate(
  dim_lab    = factor(lab[dimension], levels = lab[ord$dimension]),
  type       = factor(type, levels=type_lv, labels=type_lab),
  ownership  = factor(ownership, levels=c("Private","Public")),
  initiation = factor(initiation, levels=c("Car-initiated","Walk-initiated")),
  weighting  = factor(weighting, levels=unname(wt_lab)))

inis <- sort(unique(as.character(sw$initiation)))
dims_have <- sort(unique(sw$dimension))
missing_enh <- setdiff(c("metro_xfer","tol_pct","priority","transfer_penalty"), dims_have)
cap <- paste0("Each bar = swing in break-even speed across that assumption's swept range; longer = more sensitive. ",
  "Private vs Public shown as separate bars. NEAREST has no break-even (driving always faster) -> blank column. ",
  "Traffic speed (5-80) is the break-even axis, not a bar. ",
  if (!"Car-initiated" %in% inis) "Car-initiated row + " else "",
  if (length(missing_enh)) paste0("enhanced sweeps (", paste(lab[missing_enh],collapse="; "), ") ") else "",
  if (!"Car-initiated" %in% inis || length(missing_enh)) "are being computed by the running job; re-run to populate." else "all shown.")

# ---- TWO figures: one per weighting (user choice) ----
mk_fig <- function(wt, grayscale=FALSE) {
  fillvals <- if (grayscale) c("grey35","grey70") else pal2
  ggplot(sw %>% filter(weighting==wt), aes(x=swing, y=dim_lab, fill=ownership)) +
    facet_grid(initiation ~ type, drop=FALSE) +
    geom_col(position=position_dodge(width=0.7), width=0.62, na.rm=TRUE) +
    scale_fill_manual(values=setNames(fillvals, c("Private","Public")), name="Facility ownership", drop=FALSE) +
    scale_x_continuous(expand=expansion(mult=c(0,0.05))) +
    labs(x="Swing in break-even speed (km/h) across the swept range", y=NULL,
         title=sprintf("Impact of modelling-assumption sweeps on break-even speed (%s)", wt),
         subtitle="Rows: transit access mode (Car-initiated = manuscript Fig 3/4 basis; Walk-initiated). Columns: anchor. Bars: private vs public.",
         caption=paste(strwrap(cap, width=125), collapse="\n")) +
    base_theme
}
tag_of <- c("Unweighted"="unweighted", "Population-weighted"="weighted")
for (wt in names(tag_of)) {
  tg <- tag_of[[wt]]
  ggsave(sprintf("Data/Fig_R13_sweep_impact_%s_colour.tiff", tg),    mk_fig(wt, FALSE), width=13, height=8, dpi=300, bg="white", compression="lzw")
  ggsave(sprintf("Data/Fig_R13_sweep_impact_%s_grayscale.tiff", tg), mk_fig(wt, TRUE),  width=13, height=8, dpi=300, bg="white", compression="lzw")
}
ggsave("Data/Fig_R13_sweep_impact_unweighted_preview.png", mk_fig("Unweighted", FALSE), width=13, height=8, dpi=120, bg="white")
cat("initiations:", paste(inis, collapse=", "), " | dims:", paste(dims_have, collapse=", "), "\n")
cat("wrote Data/Fig_R13_sweep_impact_{unweighted,weighted}_{colour,grayscale}.tiff\n")
