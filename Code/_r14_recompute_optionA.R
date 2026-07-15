# =============================================================================
#  _r14_recompute_optionA.R  -- apply Option A non-reachable refinement post-hoc
#  (no pipeline re-run). non_reachable = is.na(best_total_m) & !metro_same_stn;
#  same-station-but-undefined trips are "too close / walk-preferred" -> excluded
#  from the competitiveness estimand denominator, reported separately.
#  Uses the saved R14_*.rds (raw counts + competitiveness) + the per-trip caches
#  (best_total_m, metro_same_stn) for the same-station split. UNWEIGHTED is fully
#  population-safe; WEIGHTED prints only aggregate shares (no per-district pop).
#  Self-validates: the CONDITIONAL (reachable-only) estimand must be unchanged.
# =============================================================================
suppressWarnings(suppressMessages(library(dplyr)))
base <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities"
data <- file.path(base, "Data"); code <- file.path(base, "Code")
source(file.path(code, "_r14_imputation.R"))   # r14_tipping_point
options(width = 150)

# per-anchor count of "too close" trips: is.na(best_total_m) & metro_same_stn
too_close_counts <- function(orig_rds, new_rds) {
  rd <- function(f) { x <- readRDS(f); x %>% mutate(anchor = as.character(dest_type),
                        ss = if ("metro_same_stn" %in% names(x)) as.logical(metro_same_stn) else FALSE,
                        nr = is.na(best_total_m)) %>% select(anchor, ss, nr) }
  bind_rows(rd(orig_rds), rd(new_rds)) %>% group_by(anchor) %>%
    summarise(n_too_close = sum(nr & ss, na.rm = TRUE),
              n_nr_raw    = sum(nr, na.rm = TRUE), .groups = "drop")
}

recompute <- function(tag, orig_rds, new_rds) {
  cat(sprintf("\n################  OPTION A — %s  ################\n", toupper(tag)))
  r   <- readRDS(file.path(data, sprintf("R14_imputation_sensitivity_%s.rds", tag)))
  den <- r$denominator_bounds         # n_total, n_reachable, n_non_reachable, compet_*_pct
  tc  <- too_close_counts(orig_rds, new_rds)
  d <- den %>% left_join(tc, by = "anchor") %>%
    mutate(
      n_compet      = round(compet_alltrips_pct/100 * n_total),     # back out from saved
      n_too_close   = coalesce(n_too_close, 0),
      # sanity: cache raw-NA count should match saved n_non_reachable
      nr_match      = abs(coalesce(n_nr_raw,0) - n_non_reachable) <= 1,
      n_reach       = n_reachable,                                  # transit path exists (unchanged)
      n_nonreach_A  = n_non_reachable - n_too_close,                # genuine (same-station removed)
      in_scope      = n_reach + n_nonreach_A)                       # excludes too-close
  # corrected estimands
  d <- d %>% rowwise() %>% mutate(
      conditional_pct_A = 100 * n_compet / pmax(n_reach,1),
      composite_pct_A   = 100 * n_compet / pmax(in_scope,1),
      manski_upper_A    = 100 * (n_compet + n_nonreach_A) / pmax(in_scope,1),
      genuine_nonreach_pct = 100 * n_nonreach_A / n_total,
      too_close_pct     = 100 * n_too_close / n_total,
      tip_delta_A       = r14_tipping_point(n_compet, n_reach, n_nonreach_A)$tipping_delta) %>% ungroup()

  # SELF-VALIDATION: conditional must equal the saved (old) conditional
  chk <- r$competitiveness_tipping %>% select(anchor, conditional_old = conditional_pct)
  v <- d %>% left_join(chk, by = "anchor") %>%
    mutate(cond_delta = abs(conditional_pct_A - conditional_old))
  cat(sprintf("[self-check] max |conditional_A - conditional_old| = %.3e  (must be ~0)  | cache NA counts match saved: %s\n",
              max(v$cond_delta, na.rm = TRUE), all(d$nr_match)))

  cat("\n-- Reachability re-categorised (Option A) --\n")
  print(as.data.frame(d %>% transmute(anchor, n_total,
        reachable = n_reach, genuine_nonreach = n_nonreach_A, too_close = n_too_close,
        genuine_nonreach_pct = round(genuine_nonreach_pct,3), too_close_pct = round(too_close_pct,2))),
        row.names = FALSE)
  cat("\n-- Competitiveness @40km/h: conditional (reachable) / composite (genuine-nonreach=fail) / Manski upper --\n")
  print(as.data.frame(d %>% transmute(anchor,
        conditional_pct = round(conditional_pct_A,3), composite_pct = round(composite_pct_A,3),
        manski_upper_pct = round(manski_upper_A,3), tipping_delta_pct = round(100*tip_delta_A,1))),
        row.names = FALSE)

  out <- d %>% transmute(anchor, n_total, reachable=n_reach, genuine_nonreach=n_nonreach_A,
        too_close=n_too_close, genuine_nonreach_pct, too_close_pct,
        conditional_pct=conditional_pct_A, composite_pct=composite_pct_A,
        manski_upper_pct=manski_upper_A, tipping_delta_pct=100*tip_delta_A)
  saveRDS(out, file.path(data, sprintf("R14_competitiveness_optionA_%s.rds", tag)))
  cat(sprintf("wrote R14_competitiveness_optionA_%s.rds\n", tag))
  invisible(out)
}

recompute("unweighted",
          file.path(data, "sample_test_results.rds"),
          file.path(data, "sample_test_results_newanchors_N10.rds"))
recompute("weighted",
          file.path(data, "sample_test_results_weighted.rds"),
          file.path(data, "sample_test_results_weighted_newanchors_N10.rds"))
cat("\n==== Option A recompute COMPLETE ====\n")
