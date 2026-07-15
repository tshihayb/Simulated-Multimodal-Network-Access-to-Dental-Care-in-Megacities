setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({library(sf); library(dplyr)}))
SC <- "C:/Users/Tshih/AppData/Local/Temp/claude/C--Users-Tshih-OneDrive-Claude-code-projects-Simulation-of-transit-access-to-dental-facilities/e1c3ca52-4d12-4ccc-abd4-9d0074581d13/scratchpad/"  # log only (ephemeral)
DD <- "Data/"   # persisted intermediates -> reproducible, no dependence on a session scratchpad
o <- file(paste0(SC,"metrics_out.txt"),"w"); w <- function(...) writeLines(paste0(...), o)

## 1) region-tag the weighted origins
pts  <- readRDS("Data/random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds")$pts   # sf 32638, cols: x, id
rmap <- readRDS("Data/_riyadh_merged_2_ORIGINAL.rds") %>% st_transform(32638) %>% select(new_region)
sf::sf_use_s2(FALSE)
tag <- suppressWarnings(st_join(pts, rmap, join = st_intersects, left = TRUE))
tag <- tag[!duplicated(tag$id), ]
rp_region <- tag %>% st_drop_geometry() %>% transmute(id, region = new_region)
w("region-tag: ", nrow(rp_region), " origins | NA region: ", sum(is.na(rp_region$region)))
w(paste(capture.output(print(table(rp_region$region, useNA="always"))), collapse="\n"))
saveRDS(rp_region, paste0(DD,"_region_rp_tag_weighted.rds"))

## 2) metrics from the corrected weighted results (PRIVATE sector)
res <- readRDS("Data/sample_test_results_corrected_weighted.rds")
w("\nbest_mode values: ", paste(unique(res$best_mode), collapse=", "))
w("dest_type values: ", paste(unique(res$dest_type), collapse=", "))
res <- res %>% filter(grepl("_(priv|pub)$", dest_type)) %>%
  left_join(rp_region, by = c("rp_id" = "id"))
# per-origin collapse (random target has 10 draws/origin)
# origin-any availability (== App Table 2): an origin is "available" if >=1 of its draws has a
# multimodal route. At the nearest/median/farthest targets there is 1 draw/origin, so this equals the
# fixed-target availability already shown; at the random target (10 draws/origin) it -> ~100% because
# essentially every origin can reach at least one of its 10 draws by multimodal transit.
per <- res %>% group_by(region, rp_id, dest_type) %>%
  summarise(dist_km = mean(road_dist_m, na.rm=TRUE)/1000,
            avail   = as.numeric(any(is.finite(multi_total_m))),
            multi   = mean(best_mode == "Multimodal", na.rm=TRUE), .groups="drop")
tof <- function(dt) dplyr::recode(sub("_(priv|pub)$","",dt),
        nearest="Nearest", median="Median", farthest="Farthest", random="Random")
summ <- per %>% filter(!is.na(region)) %>%
  mutate(target = tof(dest_type),
         sector = ifelse(grepl("_pub$", dest_type), "Public", "Private")) %>%
  group_by(region, target, sector) %>%
  summarise(n = n(),
            n_dist  = sum(is.finite(dist_km)),   # contributing (non-missing) n for the distance metric
            n_multi = sum(is.finite(multi)),      # contributing n for best-mode (transit-available origins)
            distance = mean(dist_km, na.rm=TRUE),
            available = 100*mean(avail, na.rm=TRUE),
            bestmulti = 100*mean(multi, na.rm=TRUE), .groups="drop")
summ$region <- dplyr::recode(summ$region, Center="Centre")
w("\n=== region x target summary (weighted, private) ===")
w(paste(capture.output(print(as.data.frame(summ), digits=4)), collapse="\n"))
saveRDS(summ, paste0(DD,"_region_direct_metrics.rds"))
close(o); cat("done\n")
