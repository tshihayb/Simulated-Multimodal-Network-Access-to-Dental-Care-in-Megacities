# =============================================================================
#  _r14_regen_optionA_docx.R  -- regenerate the 3 Option-A-affected R14 docx
#  (accounting, denominator bounds, competitiveness/tipping) for both sections,
#  merging the original imputation-share accounting with the Option-A
#  reachability re-categorisation. Leaves the (correct) 3-method + break-even
#  docx untouched. Overwrites the stale docx so the manuscript set is consistent.
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr); library(flextable); library(officer) }))
data <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data"

wx <- function(df, path, ttl) {
  ft <- flextable(as.data.frame(df)) %>% theme_booktabs() %>% autofit() %>% add_header_lines(values = ttl)
  save_as_docx(ft, path = path); cat("wrote", basename(path), "\n")
}
rnd <- function(df) df %>% mutate(across(where(is.numeric), ~round(.x, 3)))

for (tag in c("unweighted","weighted")) {
  r  <- readRDS(file.path(data, sprintf("R14_imputation_sensitivity_%s.rds", tag)))   # original (imputation shares)
  oa <- readRDS(file.path(data, sprintf("R14_competitiveness_optionA_%s.rds", tag)))  # Option-A recategorisation
  Tag <- tools::toTitleCase(tag)

  # --- accounting: original imputation shares, but non_reachable -> Option-A genuine, + too_close ---
  acct <- r$accounting %>%
    select(-pct_non_reachable) %>%
    left_join(oa %>% transmute(anchor, pct_too_close = too_close_pct,
                               pct_non_reachable = genuine_nonreach_pct), by = "anchor")
  wx(rnd(acct), file.path(data, sprintf("R14_accounting_%s.docx", tag)),
     sprintf("Imputation & reachability accounting — Option A (%s): non-reachable excludes same-station (“too close”) trips", Tag))

  # --- denominator bounds (counts + competitiveness), Option A ---
  den <- oa %>% transmute(anchor, n_total, reachable, genuine_nonreach, too_close,
                          compet_reachable_pct = conditional_pct, compet_alltrips_pct = composite_pct)
  wx(rnd(den), file.path(data, sprintf("R14_denominator_bounds_%s.docx", tag)),
     sprintf("Transit competitiveness denominators — Option A (%s): reachable-only vs all-trips (genuine non-reachable ≈ 0)", Tag))

  # --- competitiveness estimands + Manski bounds + tipping, Option A ---
  tip <- oa %>% transmute(anchor, conditional_pct, composite_pct, manski_upper_pct,
                          genuine_nonreach_pct, too_close_pct, tipping_delta_pct)
  wx(rnd(tip), file.path(data, sprintf("R14_competitiveness_tipping_%s.docx", tag)),
     sprintf("Competitiveness estimands + Manski bounds + tipping point — Option A (%s)", Tag))
}
cat("\nOption-A docx regenerated (accounting / denominator / tipping; both sections).\n")
cat("Unchanged (already correct): R14_network_distance_3method_*.docx, R14_breakeven_3method_*.docx\n")
