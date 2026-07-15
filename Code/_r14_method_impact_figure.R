# Build ONE reportable deliverable for the impact of missing-data handling
# (complete-case / ratio / PMM-MI) on the headline results: mean network distance
# + break-even speed, per anchor, unweighted & weighted. Reads the cached
# R14_imputation_sensitivity_{tag}.rds (no re-run). Writes a colour + grayscale
# TIFF and a compact docx table to Data/.
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2); library(tidyr) }))

# ---- HOUSE STYLE (matches the R1-5 travel-time figures' base_theme + palette) ----
base_theme <- theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
        legend.position = "bottom", legend.box = "horizontal",
        plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        strip.background = element_blank(), legend.key.width = unit(1.5, "cm"))
pal3 <- viridisLite::plasma(3, end = 0.9)          # same generator as mode_colors

anchor_lv <- c("nearest_priv","median_priv","farthest_priv","random_priv",
               "nearest_pub","median_pub","farthest_pub","random_pub")
anchor_lab <- c("Nearest (priv)","Median (priv)","Farthest (priv)","Random (priv)",
                "Nearest (pub)","Median (pub)","Farthest (pub)","Random (pub)")
meth_lv  <- c("complete_case","ratio","pmm")
meth_lab <- c("Complete-case","Ratio (primary)","PMM-MI (m=20)")
wt_lab   <- c(unweighted="Unweighted (uniform sample)", weighted="Population-weighted")

long <- list()
for (tag in c("unweighted","weighted")) {
  x <- readRDS(sprintf("Data/R14_imputation_sensitivity_%s.rds", tag))
  nd <- x$network_distance_3method %>%
    transmute(anchor, complete_case, ratio, pmm, se_pmm=pmm_se) %>%
    pivot_longer(c(complete_case,ratio,pmm), names_to="method", values_to="value") %>%
    mutate(se = ifelse(method=="pmm", se_pmm, NA_real_),
           outcome="Mean network distance (km)") %>% select(-se_pmm)
  be <- x$breakeven_3method %>%
    transmute(anchor, method, value=breakeven_metro, se=NA_real_,
              outcome="Break-even speed (km/h, metro)")
  long[[tag]] <- bind_rows(nd, be) %>% mutate(weighting = wt_lab[[tag]])
}
df <- bind_rows(long) %>%
  mutate(anchor   = factor(anchor, levels=anchor_lv, labels=anchor_lab),
         method   = factor(method, levels=meth_lv,  labels=meth_lab),
         weighting= factor(weighting, levels=unname(wt_lab)),
         outcome  = factor(outcome, levels=c("Mean network distance (km)","Break-even speed (km/h, metro)")))

# max relative spread across methods (network distance) -> caption
spread <- df %>% filter(outcome=="Mean network distance (km)") %>%
  group_by(weighting, anchor) %>%
  summarise(sp = 100*(max(value)-min(value))/value[method=="Ratio (primary)"], .groups="drop")
max_sp <- round(max(spread$sp), 1)
cap <- sprintf(paste0("Across all anchors the three methods agree within %.1f%% on mean network distance; ",
  "PMM-MI between-imputation SD rounds to 0 (geometric distance is a near-perfect predictor at the ~0.3-13%% ",
  "missingness here). Complete-case is marginally lowest (drops the longer, harder-to-route trips); ",
  "ratio and PMM-MI coincide. Conclusion: results are insensitive to the missing-data method."), max_sp)
cat("max relative spread (network distance):", max_sp, "%\n")

mk_plot <- function(grayscale=FALSE) {
  pd <- position_dodge(width=0.6)
  p <- ggplot(df, aes(x=anchor, y=value, group=method)) +
    geom_errorbar(aes(ymin=value-1.96*se, ymax=value+1.96*se), position=pd, width=0.3,
                  na.rm=TRUE, colour=if(grayscale)"grey40" else "grey55") +
    facet_grid(outcome ~ weighting, scales="free_y", switch="y") +
    labs(x=NULL, y=NULL,
         title="Impact of missing-data handling on accessibility results",
         subtitle="Mean network distance and break-even speed by facility anchor, under three imputation methods",
         caption=strwrap(cap, width=110) %>% paste(collapse="\n")) +
    base_theme +
    theme(axis.text.x=element_text(angle=35, hjust=1),
          strip.placement="outside", plot.caption=element_text(hjust=0, size=9))
  if (grayscale) {
    p + geom_point(aes(shape=method), position=pd, size=2.8, colour="black") +
        scale_shape_manual(values=c(16,17,15), name=NULL)
  } else {
    p + geom_point(aes(colour=method, shape=method), position=pd, size=3) +
        scale_colour_manual(values=pal3, name=NULL) +
        scale_shape_manual(values=c(16,17,15), name=NULL)
  }
}

ggsave("Data/Fig_R14_method_impact_colour.tiff",    mk_plot(FALSE), width=12, height=8, dpi=300, bg="white", compression="lzw")
ggsave("Data/Fig_R14_method_impact_grayscale.tiff", mk_plot(TRUE),  width=12, height=8, dpi=300, bg="white", compression="lzw")
ggsave("Data/Fig_R14_method_impact_preview.png",    mk_plot(FALSE), width=12, height=8, dpi=120, bg="white")
cat("wrote Data/Fig_R14_method_impact_{colour,grayscale}.tiff + preview.png\n")

# compact combined table (docx)
if (requireNamespace("flextable",quietly=TRUE) && requireNamespace("officer",quietly=TRUE)) {
  suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
  tab <- df %>%
    mutate(cell = ifelse(!is.na(se) & se>0, sprintf("%.2f (±%.2f)", value, se), sprintf("%.2f", value))) %>%
    select(weighting, outcome, anchor, method, cell) %>%
    pivot_wider(names_from=method, values_from=cell) %>%
    arrange(weighting, outcome, anchor)
  ft <- flextable(as.data.frame(tab)) %>% theme_booktabs() %>% autofit() %>%
    set_header_labels(weighting="Weighting", outcome="Outcome", anchor="Anchor") %>%
    add_header_lines("Impact of missing-data handling (complete-case / ratio / PMM-MI) on headline results") %>%
    add_footer_lines(cap)
  save_as_docx(ft, path="Data/R14_method_impact_summary.docx")
  cat("wrote Data/R14_method_impact_summary.docx\n")
}
cat("DONE\n")
