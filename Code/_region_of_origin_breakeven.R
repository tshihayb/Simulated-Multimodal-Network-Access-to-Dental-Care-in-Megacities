setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
Sys.setenv(R13FIG_BUILD="0")   # source the fig module for its evaluators WITHOUT triggering the figure build
suppressWarnings(suppressMessages({library(dplyr); library(ggplot2)}))
source("Code/_r13_permode.R")
source("Code/_r13_manuscript_figs_corrected.R")   # .r13pm_eval_origin + .r13pm_expand_perdraw
SC <- "C:/Users/Tshih/AppData/Local/Temp/claude/C--Users-Tshih-OneDrive-Claude-code-projects-Simulation-of-transit-access-to-dental-facilities/e1c3ca52-4d12-4ccc-abd4-9d0074581d13/scratchpad/"  # log only
DD <- "Data/"
o <- file(paste0(SC,"be_out.txt"),"w"); w <- function(...) writeLines(paste0(...), o)

rp_region <- readRDS(paste0(DD,"_region_rp_tag_weighted.rds"))          # id, region
combos <- readRDS("Data/R13_combos_weighted_CORRECTED.rds")
if(!"clinic_id" %in% names(combos)) combos$clinic_id <- NA_character_
combos$metro_pre <- vapply(ifelse(is.na(combos$seg_str),"",combos$seg_str), .pm_metrostr, numeric(1))
combos <- combos %>% filter(grepl("_(priv|pub)$", dest_type))

## CORRECTED origin estimand (identical to manuscript Figs 3/4):
##  (1) per-draw expand the random target so a facility drawn k times counts k, and mask multimodal
##      for origins that App Table 2 records as unavailable  (.r13pm_expand_perdraw);
##  (2) evaluate each region with .r13pm_eval_origin = per-origin mean over eligible draws, then equal
##      weight across origins.
## (The former version used the pooled .r13pm_eval over distinct pairs; that agrees with this to
##  <=0.03 km/h and only in the random column, but this states the SAME estimand as Fig 3.)
## The bundle join inside .r13pm_expand_perdraw needs all origins, so expand ONCE on the full frame,
## tag region by rp_id, then subset per region (region is an origin property, so an origin's draws
## are all in one region).
fr <- .r13pm_frame(combos, 50, 0.10, "default")
fr <- .r13pm_expand_perdraw(fr, "weighted")
fr$region <- rp_region$region[match(as.character(fr$rp_id), as.character(rp_region$id))]
w("expanded frame rows: ", nrow(fr), " | NA region: ", sum(is.na(fr$region)))

BASE <- r13pm_BASE(); spd <- seq(5,80,2.5)
regs <- c("North","East","Center","West","South")
out <- list()
for (r in regs) {
  frr <- fr %>% filter(region == r)
  ev  <- .r13pm_eval_origin(frr, BASE, spd)
  be  <- .r13pm_be50(ev)                      # anchor, Mode_family, Initiation, be50
  be$region <- r
  out[[r]] <- be
}
allbe <- bind_rows(out)
# multimodal break-even per target, BOTH initiations
tof <- function(dt) dplyr::recode(sub("_(priv|pub)$","",dt),
        nearest="Nearest", median="Median", farthest="Farthest", random="Random")
be_fig <- allbe %>%
  filter(Mode_family=="Multimodal") %>%
  mutate(target = tof(anchor),
         sector = ifelse(grepl("_pub$", anchor), "Public", "Private"),
         region = dplyr::recode(region, Center="Centre")) %>%
  select(region, target, sector, Initiation, breakeven = be50)
w("\n=== MULTIMODAL break-even (km/h) by region x target x initiation ===")
w(paste(capture.output(print(as.data.frame(be_fig), digits=4)), collapse="\n"))
w("\n=== NA (no break-even) count by target x initiation ===")
w(paste(capture.output(print(as.data.frame(dplyr::count(be_fig, target, Initiation, na=is.na(breakeven))))), collapse="\n"))
w("\n=== value range by initiation (non-NA) ===")
w(paste(capture.output(print(as.data.frame(be_fig %>% filter(is.finite(breakeven)) %>% group_by(Initiation) %>% summarise(min=min(breakeven), max=max(breakeven))))), collapse="\n"))
saveRDS(be_fig, paste0(DD,"_region_breakeven.rds"))
close(o); cat("done\n")
