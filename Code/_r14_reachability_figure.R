# =============================================================================
#  _r14_reachability_figure.R  --  Convey UNREACHABILITY and its (null) effect on
#  the transit-competitiveness result (reviewer R2-3f).
#  Panel A: reachability decomposition per anchor (reachable / too-close / genuinely
#           non-reachable) -- the genuinely-non-reachable category is empty.
#  Panel B: transit competitiveness under the reachable-only vs all-trips denominator
#           -- they coincide exactly (no non-reachable trips -> point-identified).
#  Also writes a precise census table (docx). Reads cached rds only. Colour only.
#  TAG via env R14_FIG_TAG (unweighted | weighted). House style.
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2); library(tidyr); library(patchwork) }))

TAG <- { t <- Sys.getenv("R14_FIG_TAG"); if (nzchar(t)) t else "unweighted" }
design_lab <- if (TAG == "weighted") "population-weighted" else "unweighted"

s <- readRDS(sprintf("Data/R14_imputation_sensitivity_%s.rds", TAG))
tgt_lv <- c("Nearest","Median","Farthest","Random")
key <- function(df) df %>% mutate(
  sector = ifelse(grepl("priv", anchor), "Private facilities", "Public facilities"),
  target = factor(tools::toTitleCase(sub("_(priv|pub)$","",anchor)), levels = tgt_lv))

acct  <- key(s$accounting)
bound <- key(s$denominator_bounds)
tip   <- key(s$competitiveness_tipping)

# ---- Panel A data: 3-way reachability partition (% of all trips) -------------
cat_lv <- c("Reachable by transit", "Too close to need transit (walk)", "Genuinely non-reachable")
cat_col <- setNames(c("#2c7fb8", "#bdbdbd", "#e31a1c"), cat_lv)   # blue / grey / red(absent)
decomp <- acct %>% transmute(sector, target,
    `Reachable by transit`              = 100 - pct_too_close - pct_non_reachable,
    `Too close to need transit (walk)`  = pct_too_close,
    `Genuinely non-reachable`           = pct_non_reachable) %>%
  pivot_longer(-c(sector, target), names_to = "cat", values_to = "pct") %>%
  mutate(cat = factor(cat, levels = cat_lv))

pA <- ggplot(decomp, aes(x = pct, y = target, fill = cat)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.3) +
  facet_grid(sector ~ ., switch = "y") +
  scale_fill_manual(values = cat_col, name = NULL, drop = FALSE) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
  labs(subtitle = "A.  Where every point-to-facility trip falls", x = "Share of all trips", y = NULL) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.4),
        legend.position = "bottom", legend.box = "horizontal",
        plot.subtitle = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        strip.background = element_blank(), strip.placement = "outside")

# ---- Panel B data: competitiveness under the two denominators ---------------
cmp <- bound %>% transmute(sector, target,
          `Reachable trips only` = compet_reachable_pct,
          `All trips (non-reachable = loss)` = compet_alltrips_pct) %>%
  pivot_longer(-c(sector, target), names_to = "denom", values_to = "pct")
den_lv <- c("Reachable trips only", "All trips (non-reachable = loss)")
cmp$denom <- factor(cmp$denom, levels = den_lv)
lab_cmp <- bound %>% transmute(sector, target, pct = compet_reachable_pct,
                               lab = sprintf("%.3g%%", compet_reachable_pct))
xmax_b <- max(0.03, max(bound$compet_reachable_pct, na.rm = TRUE) * 1.6)

pB <- ggplot(cmp, aes(x = pct, y = target, colour = denom, shape = denom)) +
  geom_point(size = 3.0, position = position_dodge(width = 0.65)) +
  geom_text(data = lab_cmp, aes(x = pct, y = target, label = lab), inherit.aes = FALSE,
            hjust = -0.3, vjust = 0.5, size = 2.7, colour = "grey25") +
  facet_grid(sector ~ ., switch = "y") +
  scale_colour_manual(values = c("#2c7fb8", "#fc8d59"), name = "Competitiveness denominator", drop = FALSE) +
  scale_shape_manual(values = c(16, 17), name = "Competitiveness denominator", drop = FALSE) +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  scale_x_continuous(limits = c(0, xmax_b), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.04, 0.22))) +
  labs(subtitle = "B.  Transit faster than the car (both denominators coincide)",
       x = "Transit faster than the car (% of trips)", y = NULL) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.4),
        legend.position = "bottom", legend.box = "horizontal",
        plot.subtitle = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        strip.background = element_blank(), strip.placement = "outside",
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

fig <- (pA | pB) + plot_layout(widths = c(1.35, 1)) +
  plot_annotation(
    title = "Genuinely non-reachable trips are zero, so the reachable-only competitiveness denominator does not bias the result",
    subtitle = sprintf("Reachability accounting and its effect on transit competitiveness, by facility anchor (%s design). 'Too close' = facility within direct-walk range (transit not the relevant mode).", design_lab),
    theme = theme(plot.title = element_text(face = "bold", size = 13.5),
                  plot.subtitle = element_text(size = 10, colour = "grey25")))

ggsave(sprintf("Data/Fig_R14_reachability_%s.tiff", TAG), fig, width = 13.5, height = 6.6, dpi = 300, bg = "white", compression = "lzw")
ggsave(sprintf("Data/Fig_R14_reachability_%s_preview.png", TAG), fig, width = 13.5, height = 6.6, dpi = 110, bg = "white")
cat("wrote Fig_R14_reachability_", TAG, ".tiff + preview.png\n", sep = "")

# ---- precise census table (docx) -------------------------------------------
if (requireNamespace("flextable", quietly = TRUE) && requireNamespace("officer", quietly = TRUE)) {
  suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
  amap <- c(nearest_priv="Private / Nearest", median_priv="Private / Median", farthest_priv="Private / Farthest", random_priv="Private / Random",
            nearest_pub="Public / Nearest", median_pub="Public / Median", farthest_pub="Public / Farthest", random_pub="Public / Random")
  tab <- s$accounting %>%
    left_join(s$denominator_bounds %>% select(anchor, compet_reachable_pct, compet_alltrips_pct), by = "anchor") %>%
    left_join(s$competitiveness_tipping %>% select(anchor, manski_upper_pct), by = "anchor") %>%
    transmute(
      Anchor = amap[anchor],
      `Trips (n)` = formatC(n, format = "d", big.mark = ","),
      `Reachable by transit (%)` = round(100 - pct_too_close - pct_non_reachable, 2),
      `of which walk-only, same stop (%)` = round(pct_same_station, 2),
      `Too close to need transit (%)` = round(pct_too_close, 2),
      `Genuinely non-reachable (%)` = round(pct_non_reachable, 2),
      `Competitive, reachable only (%)` = round(compet_reachable_pct, 3),
      `Competitive, all trips (%)` = round(compet_alltrips_pct, 3),
      `Manski upper bound (%)` = round(manski_upper_pct, 3))
  ft <- flextable(as.data.frame(tab)) %>% theme_booktabs() %>% autofit() %>%
    add_header_lines(sprintf("Reachability census and transit-competitiveness bounds (%s design)", design_lab)) %>%
    add_footer_lines(paste0(
      "Competitiveness = share of trips where transit is faster than the car at the reference speed. ",
      "'Too close' trips (facility within direct-walk range) are excluded from the competitiveness denominator as transit is not the relevant mode; ",
      "'same stop' trips are reachable but need no ride (the nearest stop to the point is also the nearest stop to the facility). ",
      "With zero genuinely non-reachable trips, the reachable-only and all-trips denominators coincide exactly, the Manski partial-identification interval has zero width (competitiveness is point-identified), and the tipping-point fraction is undefined."))
  save_as_docx(ft, path = sprintf("Data/R14_reachability_census_%s.docx", TAG))
  cat("wrote R14_reachability_census_", TAG, ".docx\n", sep = "")
}
cat("DONE\n")
