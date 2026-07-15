# =============================================================================
#  _r14_impact_figure.R  --  ONE professional figure: impact of the missing-data
#  method (complete-case / ratio / PMM-MI) on the accessibility results.
#  Frames every method-sensitive outcome as % deviation from the ratio (primary)
#  method on a single shared axis. Metro-only and multimodal transit shown
#  SEPARATELY (not combined). Reads cached rds only. House palette = plasma(3).
#  Unweighted. Colour only. Title+subtitle on figure; full caption is external.
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2); library(tidyr) }))

TAG  <- { t <- Sys.getenv("R14_FIG_TAG"); if (nzchar(t)) t else "unweighted" }   # "unweighted" | "weighted"
design_lab <- if (TAG == "weighted") "population-weighted" else "unweighted"
CORR <- Sys.getenv("R14_FIG_CORRECTED") == "1"                                   # read MM-reach-corrected chain break-even
CSFX <- if (CORR) "_CORRECTED" else ""
GREY <- Sys.getenv("R14_GREY") == "1"; GSFX <- if (GREY) "_grey" else ""   # greyscale option (main-text B/W)
pal3 <- if (GREY) c("grey15","grey45","grey68") else viridisLite::plasma(3, end = 0.9)   # methods also carry point shapes 16/17/15
meth_lv  <- c("complete_case","ratio","pmm")
meth_lab <- c("Complete-case (drop missing trips)",
              "Ratio imputation, primary (straight-line × detour factor)",
              "Predictive mean matching multiple imputation (20 imputations)")
names(pal3) <- meth_lab

sens <- readRDS(sprintf("Data/R14_imputation_sensitivity_%s.rds", TAG))   # direct/metro panels: unaffected by MM-reach correction
chbe <- readRDS(sprintf("Data/R14_chain_be_%s%s.rds", TAG, CSFX))         # CORR => corrected multimodal chain + break-even

# % missing per leg -- UNIFORM across targets (point-level road-snap failure): direct =
# RP->facility road leg; L1 = access leg (random point -> nearest transit stop). Shown in
# the panel labels (same value for every target x sector, so a per-row grid would repeat it).
.di_pct <- suppressWarnings(as.numeric(sens$accounting$pct_direct_imputed[1]))
.l1_pct <- suppressWarnings(as.numeric(sens$accounting$pct_L1_imputed[1]))
.dfmt <- function(p) if (is.finite(p)) sprintf("\n(%.1f%% missing)", p) else ""
.lfmt <- function(p) if (is.finite(p)) sprintf("\n(access leg: %.1f%% missing)", p) else ""

# panel labels: line 1 = outcome (repeated -> groups the mode pair); line 2 = mode + unit
P_ND  <- paste0("Direct network distance\n(km)", .dfmt(.di_pct))
P_CHM <- paste0("Transit chain length\nMetro-only (km)", .lfmt(.l1_pct))
P_CHX <- paste0("Transit chain length\nMultimodal (km)", .lfmt(.l1_pct))
P_BEMC <- "Break-even, Metro-only\n(car-initiated, km/h)"
P_BEXC <- "Break-even, Multimodal\n(car-initiated, km/h)"
P_BEMW <- "Break-even, Metro-only\n(walk-initiated, km/h)"
P_BEXW <- "Break-even, Multimodal\n(walk-initiated, km/h)"
panel_lv <- c(P_ND, P_CHM, P_CHX, P_BEMC, P_BEXC, P_BEMW, P_BEXW)

nd <- sens$network_distance_3method %>%
  transmute(anchor, complete_case, ratio, pmm) %>%
  pivot_longer(c(complete_case, ratio, pmm), names_to = "method", values_to = "value") %>%
  mutate(panel = P_ND)
ch_m <- chbe$chain_3method %>% transmute(anchor, method, value = metro_chain_km, panel = P_CHM)
ch_x <- chbe$chain_3method %>% transmute(anchor, method, value = multi_chain_km, panel = P_CHX)
be_mc <- chbe$breakeven50 %>% filter(Mode_family == "Metro-only", Initiation == "Car-initiated")  %>% transmute(anchor, method, value = be50, panel = P_BEMC)
be_xc <- chbe$breakeven50 %>% filter(Mode_family == "Multimodal", Initiation == "Car-initiated")  %>% transmute(anchor, method, value = be50, panel = P_BEXC)
be_mw <- chbe$breakeven50 %>% filter(Mode_family == "Metro-only", Initiation == "Walk-initiated") %>% transmute(anchor, method, value = be50, panel = P_BEMW)
be_xw <- chbe$breakeven50 %>% filter(Mode_family == "Multimodal", Initiation == "Walk-initiated") %>% transmute(anchor, method, value = be50, panel = P_BEXW)
dat <- bind_rows(nd, ch_m, ch_x, be_mc, be_xc, be_mw, be_xw)

tgt_lv <- c("Nearest","Median","Farthest","Random")
dat <- dat %>% mutate(
  sector = ifelse(grepl("priv", anchor), "Private facilities", "Public facilities"),
  target = factor(tools::toTitleCase(sub("_(priv|pub)$","",anchor)), levels = tgt_lv),
  panel  = factor(panel, levels = panel_lv))

# % deviation from the ratio (primary) method
dev <- dat %>% group_by(sector, target, panel) %>%
  mutate(ratio_val = value[method == "ratio"][1],
         pct_dev   = 100 * (value - ratio_val) / ratio_val) %>%
  ungroup() %>%
  mutate(method = factor(method, levels = meth_lv, labels = meth_lab))

seg <- suppressWarnings(dev %>% group_by(sector, target, panel) %>%
  summarise(lo = min(pct_dev, na.rm = TRUE), hi = max(pct_dev, na.rm = TRUE),
            ratio_val = ratio_val[1], .groups = "drop")) %>% filter(is.finite(lo))
labs_abs <- seg %>% mutate(lab = ifelse(grepl("km/h", panel), sprintf("%.0f km/h", ratio_val), sprintf("%.0f km", ratio_val)))
na_be <- dev %>% filter(grepl("km/h", panel)) %>% group_by(sector, target, panel) %>%
  summarise(all_na = all(is.na(pct_dev)), .groups = "drop") %>% filter(all_na)

# numbers for the EXTERNAL caption (printed, not drawn)
rp_spread <- dev %>% filter(!grepl("km/h", panel), method == meth_lab[3], is.finite(pct_dev)) %>% summarise(m = max(abs(pct_dev))) %>% pull(m) %>% round(1)
cc_spread <- dev %>% filter(!grepl("km/h", panel), method == meth_lab[1], is.finite(pct_dev)) %>% summarise(m = min(pct_dev)) %>% pull(m) %>% round(0)
be_spread <- dev %>% filter(grepl("km/h", panel), is.finite(value)) %>% group_by(sector, target, panel) %>% summarise(r = max(value)-min(value), .groups="drop") %>% summarise(m = max(r)) %>% pull(m) %>% round(2)
cat(sprintf("PMM-vs-ratio max = %.1f%% ; complete-case min = %.0f%% ; max break-even spread = %.2f km/h\n", rp_spread, cc_spread, be_spread))

band <- 5
p <- ggplot(dev, aes(y = target)) +
  annotate("rect", xmin = -band, xmax = band, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.55) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.5) +
  geom_segment(data = seg, aes(x = lo, xend = hi, y = target, yend = target),
               colour = "grey70", linewidth = 0.9, inherit.aes = FALSE) +
  geom_point(aes(x = pct_dev, colour = method, shape = method), size = 3.0, na.rm = TRUE) +
  geom_text(data = labs_abs, aes(x = 0, y = target, label = lab), inherit.aes = FALSE,
            vjust = -1.2, size = 2.5, colour = "grey35", fontface = "italic") +
  { if (nrow(na_be)) geom_text(data = na_be, aes(x = 0, y = target, label = "no break-even"),
            inherit.aes = FALSE, hjust = 0.5, vjust = 0.5, size = 2.5, colour = "grey45", fontface = "italic") } +
  facet_grid(sector ~ panel) +
  scale_colour_manual(values = pal3, name = "Missing-data handling method", drop = FALSE) +
  scale_shape_manual(values = c(16, 17, 15), name = "Missing-data handling method", drop = FALSE) +
  scale_x_continuous(breaks = seq(-20, 0, 10), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.12, 0.12))) +
  scale_y_discrete(expand = expansion(add = c(0.55, 0.95))) +
  coord_cartesian(xlim = c(-26, 6)) +   # common range across unweighted/weighted twins for comparability
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  labs(
    title = if (TAG == "weighted") "Missing-data Handling Impact on the Population-weighted Spatial Accessibility Results"
            else "Missing-data Handling Impact on the Spatial Accessibility Results",
    x = "Percentage difference from the ratio (primary) estimate  (0% = ratio,  negative = a lower value)",
    y = "Facility target") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4),
        legend.position = "bottom", legend.box = "horizontal", legend.key.width = unit(0.9, "cm"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, colour = "grey25"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 9, lineheight = 0.95),
        strip.background = element_blank())

ggsave(sprintf("Data/Fig_R14_imputation_impact_%s%s%s.tiff", TAG, CSFX, GSFX), p, width = 16, height = 7.4, dpi = 300, bg = "white", compression = "lzw")
ggsave(sprintf("Data/Fig_R14_imputation_impact_%s%s%s_preview.png", TAG, CSFX, GSFX), p, width = 16, height = 7.4, dpi = 105, bg = "white")
cat("wrote Fig_R14_imputation_impact_", TAG, CSFX, GSFX, ".tiff\n", sep = "")
