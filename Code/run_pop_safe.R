# =============================================================================
# run_pop_safe.R  —  Population-safe runner
# -----------------------------------------------------------------------------
# Executes "Analysis clean actual road distance.R" in a CHILD R process with ALL
# console output captured to a LOCAL log (Data/run_log.txt) that is never uploaded.
#
# Per-district population (GASTAT-restricted) is never read: Sections 7, 8, 11 and
# 12 are guarded to LOAD the supplied intermediate .rds files instead of rebuilding
# them (a rebuild would read the population table). As a fail-safe, this runner
# refuses to start if any of those supplied intermediates is missing — otherwise
# the script would fall through to a rebuild and read population.
#
# Usage:  Rscript "Code/run_pop_safe.R"
#         (then inspect Data/run_log.txt locally; it is not sent anywhere)
# =============================================================================

base     <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities"
data_dir <- file.path(base, "Data")
script   <- file.path(base, "Code", "Analysis clean actual road distance.R")
log_f    <- file.path(data_dir, "run_log.txt")
rscript  <- file.path(R.home("bin"), "Rscript.exe")

# Intermediates whose rebuild would read population — must be present.
required <- c(
  "graph_c_motorcar_clipped.rds",
  "graph_c_motorcar_clipped_OPTION1_directed.rds",
  "graph_w_motorcar_clipped_OPTION1_directed.rds",
  "roads_sf_riyadh_clipped_32638.rds",
  "random_points_geo_vs_network_EDGE_METHOD_populated_only.rds",
  "random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds"
)
missing <- required[!file.exists(file.path(data_dir, required))]
if (length(missing) > 0) {
  cat("[RUN STATUS] REFUSING TO RUN -- missing pop-derived intermediates",
      "(a rebuild would read population):\n")
  cat(paste0("  - ", missing, collapse = "\n"), "\n")
  quit(status = 2)
}

cat("[RUN STATUS] starting; all output -> ", log_f, " (local only)\n", sep = "")
t0   <- Sys.time()
code <- system2(rscript, args = c("--vanilla", shQuote(script)),
                stdout = log_f, stderr = log_f)
dt   <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)

cat(sprintf("[RUN STATUS] exit=%s  elapsed=%s min  (full log kept locally: %s)\n",
            code, dt, log_f))
