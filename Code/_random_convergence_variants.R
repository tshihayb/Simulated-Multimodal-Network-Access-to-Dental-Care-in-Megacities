# =============================================================================
#  _random_convergence_variants.R  --  TWO comparison variants of the random-target
#  Monte-Carlo convergence figure, extended to 3 metrics (road distance, metro-only
#  total chain, multimodal total best path) and the with- vs without-replacement
#  sampling comparison. Origin (rp_id) is the unit; per-origin mean over ELIGIBLE
#  draws, averaged over origins with >=1 eligible; MCSE = sqrt(sbar2_W/(N*pbar*m)).
#    Variant A (Option 1): 6 panels = metric(rows) x sample(cols), Monte Carlo error
#                          only; colour = target, solid/dashed = with/without repl.
#    Variant B (Option 2): 12 panels = (sample x metric) rows x {Convergence | MCSE}.
#  Writes Data/Fig_random_convergence_{optA_mcse,optB_full}{,_grey}.tiff.
#  Does NOT touch the deployed Appendix Figure 5 (Fig_random_draws_convergence_*).
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(tidyr); library(ggplot2); library(patchwork) }))

PREVIEW <- Sys.getenv("CONV_PREVIEW_DIR", "")
plasma3  <- viridisLite::plasma(3, end = 0.9)
PAL_COL  <- c(random_priv = unname(plasma3[1]), random_pub = unname(plasma3[2]))
PAL_GREY <- c(random_priv = "grey10",           random_pub = "grey52")
LAB_T    <- c(random_priv = "Random private-facility target", random_pub = "Random public-facility target")
M_FAC    <- c(random_priv = 732, random_pub = 34)   # facilities per ownership (for FPC)
WLV      <- c("Uniform (unweighted)", "Population-weighted")
DT_LV    <- c("random_priv", "random_pub")
METRICS  <- tibble::tribble(
  ~mkey,   ~mcol,                 ~mlab,
  "road",  "road_dist_m",         "Road distance",
  "metro", "metro_only_total_m",  "Metro-only total chain",
  "multi", "multi_total_m",       "Multimodal total best path")
MLV <- METRICS$mlab

# Two-colour legend-key glyph (private on top, public below) drawn with the key's
# linetype -> the Sampling-scheme legend shows solid priv+pub and dashed priv+pub.
# Same mechanism as Code/_r13_manuscript_figs_corrected.R (make_key_twoline).
.lt_pt <- 72.27 / 25.4
make_key_twoline <- function(c1, c2) function(data, params, size) {
  lt <- data$linetype; if (is.null(lt) || length(lt) == 0 || is.na(lt)) lt <- 1
  grid::gList(
    grid::segmentsGrob(0.05, 0.70, 0.95, 0.70, gp = grid::gpar(col = c1, lwd = 1.1 * .lt_pt, lty = lt)),
    grid::segmentsGrob(0.05, 0.30, 0.95, 0.30, gp = grid::gpar(col = c2, lwd = 1.1 * .lt_pt, lty = lt)))
}

base_theme <- theme_bw(base_size = 13) + theme(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
  axis.line = element_blank(), axis.ticks = element_line(colour = "grey45", linewidth = 0.3),
  axis.ticks.length = unit(2.4, "pt"), legend.position = "bottom", legend.box = "vertical",
  plot.title = element_text(face = "bold"), plot.subtitle = element_text(size = 9),
  axis.title = element_text(face = "bold"), legend.title = element_text(face = "bold"),
  strip.text = element_text(face = "bold", size = 9.5), strip.background = element_blank(),
  panel.spacing.x = unit(1, "lines"), panel.spacing.y = unit(1.1, "lines"), legend.key.width = unit(1.3, "cm"))

# ---- per-(tag,metric) statistics ----
prep_tag <- function(tag, seed = 7L) {
  res <- readRDS(sprintf("Data/sample_test_results_corrected_%s.rds", tag)) %>%
    filter(grepl("^random", dest_type))
  RUN <- list(); MC <- list(); AGG <- list()
  for (i in seq_len(nrow(METRICS))) {
    mk <- METRICS$mkey[i]; mc <- METRICS$mcol[i]; ml <- METRICS$mlab[i]
    d <- res %>% transmute(dest_type = factor(dest_type, levels = DT_LV), rp_id, km = .data[[mc]] / 1000)
    po <- d %>% filter(is.finite(km)) %>% group_by(dest_type, rp_id) %>%
      summarise(mbar = mean(km), s2 = if (n() > 1) var(km) else NA_real_, ni = n(), .groups = "drop")
    agg <- po %>% group_by(dest_type) %>%
      summarise(theta = mean(mbar), sbar2W = mean(s2, na.rm = TRUE),
                Norig = n(), pbar = mean(ni) / 10, elig = sum(ni), .groups = "drop")
    # MCSE vs m (1..100), with + without replacement (WOR capped at M facilities)
    mc_curve <- agg %>% crossing(m = 1:100, scheme = c("With replacement", "Without replacement")) %>%
      mutate(Mf = M_FAC[as.character(dest_type)],
             fpc = ifelse(scheme == "Without replacement", pmax(1 - m / Mf, 0), 1),
             mcse = sqrt(sbar2W / (Norig * pbar * m)) * sqrt(fpc), pct = 100 * mcse / theta) %>%
      filter(!(scheme == "Without replacement" & m > Mf))
    # running estimate over eligible draws (random within-origin order)
    set.seed(seed)
    run <- d %>% group_by(dest_type, rp_id) %>% mutate(ord = sample.int(n())) %>%
      arrange(ord, .by_group = TRUE) %>%
      mutate(ce = cumsum(is.finite(km)), cs = cumsum(ifelse(is.finite(km), km, 0)),
             cummean = ifelse(ce > 0, cs / ce, NA_real_)) %>%
      group_by(dest_type, ord) %>% summarise(est = mean(cummean, na.rm = TRUE), .groups = "drop") %>%
      rename(m = ord) %>% left_join(agg, by = "dest_type") %>%
      mutate(mcse_m = sqrt(sbar2W / (Norig * pbar * m)), lo = theta - 1.96 * mcse_m, hi = theta + 1.96 * mcse_m)
    RUN[[mk]] <- run %>% mutate(mkey = mk, mlab = ml)
    MC[[mk]]  <- mc_curve %>% mutate(mkey = mk, mlab = ml)
    AGG[[mk]] <- agg %>% mutate(mkey = mk, mlab = ml)
  }
  tag_w <- if (tag == "unweighted") WLV[1] else WLV[2]
  list(run = bind_rows(RUN) %>% mutate(weighting = tag_w),
       mc  = bind_rows(MC)  %>% mutate(weighting = tag_w),
       agg = bind_rows(AGG) %>% mutate(weighting = tag_w))
}

Pu <- prep_tag("unweighted"); Pw <- prep_tag("weighted")
RUN <- bind_rows(Pu$run, Pw$run) %>% mutate(weighting = factor(weighting, WLV), mlab = factor(mlab, MLV))
MC  <- bind_rows(Pu$mc,  Pw$mc)  %>% mutate(weighting = factor(weighting, WLV), mlab = factor(mlab, MLV))
AGG <- bind_rows(Pu$agg, Pw$agg) %>% mutate(weighting = factor(weighting, WLV), mlab = factor(mlab, MLV))

cat("\n==== MCSE at m=10 (km and % of estimate) ====\n")
MC %>% filter(m == 10) %>%
  transmute(weighting, mlab, dest_type, scheme, theta = round(theta, 2),
            mcse_km = round(mcse, 4), pct = round(pct, 3)) %>%
  arrange(weighting, mlab, dest_type, scheme) %>% as.data.frame() %>% print(row.names = FALSE)

# m=10 with-replacement annotation labels
ann10 <- MC %>% filter(m == 10, scheme == "With replacement", dest_type == "random_priv") %>%
  mutate(lab = sprintf("m=10: %.3f km (%.2f%%)", mcse, pct))

lt_scheme <- scale_linetype_manual(values = c("With replacement" = "solid", "Without replacement" = "dashed"))

# ---------- VARIANT A: 6-panel, MCSE only (metric rows x sample cols) ----------
mk_optA <- function(grey = FALSE) {
  pal <- if (grey) PAL_GREY else PAL_COL
  ggplot(MC, aes(m, mcse, colour = dest_type, linetype = scheme)) +
    geom_vline(xintercept = 10, linetype = "dotted", colour = "grey55", linewidth = 0.35) +
    geom_line(linewidth = 0.85) +
    geom_point(data = MC %>% filter(m %in% c(10, 50, 100), scheme == "With replacement"), size = 1.5) +
    geom_text(data = ann10, aes(x = 10, y = mcse, label = lab), inherit.aes = FALSE,
              colour = "grey25", size = 2.5, hjust = -0.05, vjust = -0.8) +
    scale_colour_manual(values = pal, labels = LAB_T, name = NULL) +
    lt_scheme + labs(linetype = "Sampling scheme") +
    scale_x_continuous(breaks = c(1, 10, 25, 50, 75, 100)) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.18))) +
    facet_grid(mlab ~ weighting, scales = "free_y") +
    labs(title = "Monte Carlo Error of the Random-Target Estimate by Metric and Sampling Scheme",
         subtitle = "Standard error of the origin-aggregated mean (mean of the 10,000 per-origin means) versus draws per origin. Dotted line marks the 10 draws used.",
         x = "Draws per origin (m)", y = "Monte Carlo standard error of the mean (km)") +
    base_theme +
    guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 1.1)),
           linetype = guide_legend(order = 2, override.aes = list(linewidth = 0.7)))
}

# ---------- VARIANT B: 12-panel, both views (sample x metric rows) ----------
row_lab <- function(df) df %>% mutate(row = factor(paste0(weighting, "\n", mlab),
  levels = as.vector(t(outer(WLV, MLV, function(w, m) paste0(w, "\n", m))))))
RUNb <- row_lab(RUN); MCb <- row_lab(MC); AGGb <- row_lab(AGG); ann10b <- row_lab(ann10)

mk_optB <- function(grey = FALSE) {
  pal <- if (grey) PAL_GREY else PAL_COL
  kt  <- make_key_twoline(unname(pal["random_priv"]), unname(pal["random_pub"]))   # two-colour scheme keys
  sch_dummy <- data.frame(m = NA_real_, mcse = NA_real_,
    scheme = factor(c("With replacement", "Without replacement"), levels = c("With replacement", "Without replacement")))
  pA <- ggplot(RUNb, aes(m, est, colour = dest_type, fill = dest_type)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    geom_hline(data = AGGb, aes(yintercept = theta, colour = dest_type), linetype = "dashed", linewidth = 0.35, alpha = 0.5) +
    geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
    scale_colour_manual(values = pal, labels = LAB_T, name = NULL) +
    scale_fill_manual(values = pal, guide = "none") + scale_x_continuous(breaks = c(1, 4, 7, 10)) +
    facet_grid(row ~ ., scales = "free_y", switch = "y") +
    labs(title = "Convergence of the estimate", subtitle = "Running estimate (axis zoomed to the Monte Carlo scale).",
         x = "Draws per origin (m)", y = "Estimated mean (km)") +
    base_theme + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, size = 8.5))
  pB <- ggplot(MCb, aes(m, mcse)) +
    geom_vline(xintercept = 10, linetype = "dotted", colour = "grey55", linewidth = 0.35) +
    geom_line(aes(colour = dest_type, linetype = scheme), linewidth = 0.8, show.legend = c(colour = FALSE, linetype = FALSE)) +
    geom_line(data = sch_dummy, aes(linetype = scheme), colour = NA, na.rm = TRUE,
              key_glyph = kt, show.legend = c(colour = FALSE, linetype = TRUE)) +   # hidden layer -> two-colour Sampling-scheme legend
    geom_text(data = ann10b, aes(x = 10, y = mcse, label = lab), inherit.aes = FALSE,
              colour = "grey25", size = 2.3, hjust = -0.05, vjust = -0.8) +
    scale_colour_manual(values = pal, labels = LAB_T, name = NULL, guide = "none") + lt_scheme + labs(linetype = "Sampling scheme") +
    scale_x_continuous(breaks = c(1, 10, 50, 100)) + scale_y_continuous(expand = expansion(mult = c(0.03, 0.16))) +
    facet_grid(row ~ ., scales = "free_y") +
    labs(title = "Monte Carlo error", subtitle = "SE of the origin-aggregated mean; with vs without replacement.",
         x = "Draws per origin (m)", y = "Monte Carlo standard error (km)") +
    base_theme + theme(strip.text.y = element_blank())
  (pA | pB) + plot_layout(guides = "collect") +
    plot_annotation(title = "Monte Carlo Convergence of the Random-Target Spatial Accessibility Estimate, by Metric and Sampling Scheme",
                    theme = theme(plot.title = element_text(face = "bold", size = 14))) &
    theme(legend.position = "bottom", legend.key.height = unit(0.72, "cm"))
}

save2 <- function(base, plt, w, h) {
  for (g in c(FALSE, TRUE)) {
    fn <- sprintf("Data/%s%s.tiff", base, if (g) "_grey" else "")
    ggsave(fn, plt(g), width = w, height = h, dpi = 300, compression = "lzw", bg = "white")
    cat("wrote", fn, "\n")
  }
}
save2("Fig_random_convergence_optA_mcse", mk_optA, 12, 10)
save2("Fig_random_convergence_optB_full", mk_optB, 13, 16)

if (nzchar(PREVIEW)) try({
  for (b in c("Fig_random_convergence_optA_mcse", "Fig_random_convergence_optB_full"))
    magick::image_write(magick::image_resize(magick::image_read(sprintf("Data/%s.tiff", b)), "1500x"),
                        file.path(PREVIEW, paste0(b, ".png")), format = "png")
}, silent = TRUE)
cat("ALL DONE\n")
