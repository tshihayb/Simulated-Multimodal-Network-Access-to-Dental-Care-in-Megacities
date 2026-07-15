# =============================================================================
#  _random_draws_convergence.R  --  Monte-Carlo convergence + error figure for the
#  random-destination anchor (reviewer robustness exhibit, "why only 10 draws").
#  ONE figure, four panels: upper row = uniform (unweighted), lower row = population
#  weighted; left column = convergence of the estimate, right column = Monte Carlo
#  standard error vs number of draws. Origin (rp_id) is the unit; 10 exchangeable
#  draws/origin. Basis: Morris/White/Crowther 2019 (ADEMP) + Koehler/Brown/Haneuse 2009.
#  Reads Data/sample_test_results_corrected_<tag>.rds (road_dist_m). POP-SAFE.
#  Styling mirrors Code/_r13_manuscript_figs_corrected.R (base_theme, plasma3 palette).
#  RUN:  & <Rscript> "Code\_random_draws_convergence.R"
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2); library(patchwork); library(tidyr) }))

PREVIEW_DIR <- "C:/Users/Tshih/AppData/Local/Temp/claude/C--Users-Tshih-OneDrive-Claude-code-projects-Simulation-of-transit-access-to-dental-facilities/fefb1d38-2211-425c-8fea-55f3a4106ab1/scratchpad"

plasma3  <- viridisLite::plasma(3, end = 0.9)
PAL_COL  <- c(random_priv = unname(plasma3[1]), random_pub = unname(plasma3[2]))
PAL_GREY <- c(random_priv = "grey10",           random_pub = "grey52")
LAB      <- c(random_priv = "Random private-facility target", random_pub = "Random public-facility target")
DT_LV    <- c("random_priv", "random_pub")
WLV      <- c("Uniform (unweighted)", "Population-weighted")

base_theme <- theme_bw(base_size = 14) + theme(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
  axis.line = element_blank(), axis.ticks = element_blank(), legend.position = "bottom", legend.box = "horizontal",
  plot.title = element_text(face = "bold"), plot.subtitle = element_text(size = 10), axis.title = element_text(face = "bold"),
  legend.title = element_text(face = "bold"), strip.text = element_text(face = "bold"), strip.background = element_blank(),
  panel.spacing.x = unit(1, "lines"), panel.spacing.y = unit(1.4, "lines"), legend.key.width = unit(1.4, "cm"))
tick_theme <- theme(axis.ticks = element_line(colour = "grey45", linewidth = 0.3), axis.ticks.length = unit(2.6, "pt"))

prep_tag <- function(tag, wlabel, m_max = 10L, m_ext = 100L, seed = 7L) {
  d <- readRDS(sprintf("Data/sample_test_results_corrected_%s.rds", tag)) %>%
    filter(grepl("^random", dest_type)) %>%
    transmute(dest_type = factor(dest_type, levels = DT_LV), rp_id, km = road_dist_m / 1000) %>%
    filter(is.finite(km))
  po <- d %>% group_by(dest_type, rp_id) %>% summarise(mbar = mean(km), s2 = var(km), .groups = "drop")
  agg <- po %>% group_by(dest_type) %>%
    summarise(theta = mean(mbar), sbar2_W = mean(s2, na.rm = TRUE), var_mbar = var(mbar), Norig = n(), .groups = "drop") %>%
    mutate(sigma_W = sqrt(sbar2_W), sigma2_B = pmax(var_mbar - sbar2_W / m_max, 0),
           sigma_B = sqrt(sigma2_B), icc = sigma2_B / (sigma2_B + sbar2_W))
  set.seed(seed)
  runest <- d %>% group_by(dest_type, rp_id) %>% mutate(draw = sample.int(n())) %>%
    arrange(draw, .by_group = TRUE) %>% mutate(cummean = cumsum(km) / seq_len(n())) %>%
    group_by(dest_type, draw) %>% summarise(est = mean(cummean), .groups = "drop") %>%
    rename(m = draw) %>% left_join(agg, by = "dest_type") %>%
    mutate(mcse = sqrt(sbar2_W / (Norig * m)), lo = theta - 1.96 * mcse, hi = theta + 1.96 * mcse)
  mcse_curve <- agg %>% crossing(m = 1:m_ext) %>% mutate(mcse = sqrt(sbar2_W / (Norig * m)), pct = 100 * mcse / theta)
  lapply(list(runest = runest, mcse_curve = mcse_curve, agg = agg), function(x) mutate(x, weighting = factor(wlabel, levels = WLV)))
}

Pu <- prep_tag("unweighted", WLV[1]); Pw <- prep_tag("weighted", WLV[2])
runest <- bind_rows(Pu$runest, Pw$runest)
mcse   <- bind_rows(Pu$mcse_curve, Pw$mcse_curve)
agg    <- bind_rows(Pu$agg, Pw$agg)

cat("\n==== computed stats ====\n"); print(as.data.frame(agg %>% mutate(across(where(is.numeric), ~round(., 4)))))
for (i in seq_len(nrow(agg))) { a <- agg[i, ]
  for (mm in c(1,10,50,100)) cat(sprintf("  [%s | %s] m=%3d  MCSE=%.4f km (%.3f%%)\n",
      a$weighting, a$dest_type, mm, sqrt(a$sbar2_W/(a$Norig*mm)), 100*sqrt(a$sbar2_W/(a$Norig*mm))/a$theta)) }

mk_panelA <- function(grey = FALSE) {
  pal <- if (grey) PAL_GREY else PAL_COL
  ggplot(runest, aes(m, est, color = dest_type, fill = dest_type)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.16, colour = NA) +
    geom_hline(data = agg, aes(yintercept = theta, color = dest_type), linetype = "dashed", linewidth = 0.4, alpha = 0.55) +
    geom_line(linewidth = 1.0) + geom_point(size = 1.7) +
    scale_color_manual(values = pal, labels = LAB) + scale_fill_manual(values = pal, guide = "none") +
    scale_x_continuous(breaks = 1:10) +
    facet_grid(rows = vars(weighting), scales = "free_y", switch = "y") +
    labs(title = "Convergence of the Aggregate Estimate",
         subtitle = "Running estimate (vertical axis zoomed).",
         x = "Draws per origin (m)", y = "Estimated mean network distance (km)", color = NULL) +
    base_theme + tick_theme + theme(strip.placement = "outside")
}

mk_panelB <- function(grey = FALSE) {
  pal <- if (grey) PAL_GREY else PAL_COL
  mk  <- mcse %>% filter(m %in% c(10, 50, 100), dest_type == DT_LV[1])
  ann <- mk %>% mutate(lab = sprintf("m=%d: %.3f km (%.2f%%)", m, mcse, pct), hj = c(0, 0.5, 1)[match(m, c(10, 50, 100))])
  ggplot(mcse, aes(m, mcse, color = dest_type)) +
    geom_vline(xintercept = 10, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    geom_line(linewidth = 1.0) + geom_point(data = mk, size = 2.1) +
    geom_text(data = ann, aes(label = lab, hjust = hj), color = "grey25", size = 2.7, vjust = -0.9, show.legend = FALSE) +
    scale_color_manual(values = pal, labels = LAB, guide = "none") +
    scale_x_continuous(breaks = c(1, 10, 25, 50, 75, 100)) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.20))) +
    facet_grid(rows = vars(weighting), scales = "free_y") +
    labs(title = "Monte Carlo Error vs. Number of Draws",
         subtitle = "Standard error of the aggregated mean.",
         x = "Draws per origin (m)", y = "Monte Carlo standard error of the mean (km)", color = NULL) +
    base_theme + tick_theme + theme(strip.text.y = element_blank())
}

mk_fig <- function(grey = FALSE) {
  (mk_panelA(grey) | mk_panelB(grey)) + plot_layout(guides = "collect") +
    plot_annotation(title = "Monte Carlo Convergence of the Random-Destination Spatial Accessibility Estimate",
                    theme = theme(plot.title = element_text(face = "bold", size = 15))) &
    theme(legend.position = "bottom")
}

for (grey in c(FALSE, TRUE)) {
  fn <- sprintf("Data/Fig_random_draws_convergence_CORRECTED%s.tiff", if (grey) "_grey" else "")
  ggsave(fn, mk_fig(grey), width = 12, height = 9, dpi = 300, compression = "lzw")
  cat("wrote:", fn, "\n")
  if (!grey) try(magick::image_write(magick::image_resize(magick::image_read(fn), "1500x"),
                 file.path(PREVIEW_DIR, "preview_conv_combined.png"), format = "png"), silent = TRUE)
}
cat("ALL DONE\n")
