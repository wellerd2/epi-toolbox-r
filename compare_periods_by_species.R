# compare_periods_by_species.R
# Helper for model fit using a modified version of the FoodNetTrends
# code. Specificaly, works to compare periods when the spline is fit  
# using a "by" variable 
# see https://github.com/CDCgov/FoodNetTrends
#
# Requirements (enforced in code):
# - Input `catch` must include: year, .draw, .epred, population, count, and `species_col`
# Dependencies:
# - dplyr, tidyr
# - bayestestR for hdi() (user-confirmed)
# - brms is commonly upstream of this function (e.g., generating .draw/.epred), though this function does not call brms directly.

compare_periods_by_species <- function(catch,
                                       baseline_start = 2007,
                                       baseline_end   = 2014,
                                       compare_start  = 2015,
                                       compare_end    = 2024,
                                       species_col    = "species",
                                       output_file    = NULL) {

  # Basic checks
  req <- c("year", ".draw", ".epred", "population", "count", species_col)
  missing <- setdiff(req, names(catch))
  if (length(missing) > 0) stop(paste("Missing required columns:", paste(missing, collapse = ", ")))
  if (!exists("hdi")) stop("hdi() not found. Load the package that provides hdi(), e.g., HDInterval or bayestestR.")

  # Helper for safe HDI extraction
  hdi_low  <- function(x) as.numeric(hdi(x, credMass = 0.95)[1])
  hdi_high <- function(x) as.numeric(hdi(x, credMass = 0.95)[2])

  # Add IR
  dat <- catch %>%
    dplyr::mutate(ir = .epred / (population / 100000))

  # Keep only the two periods
  dat <- dat %>%
    dplyr::filter((year >= baseline_start & year <= baseline_end) |
                    (year >= compare_start  & year <= compare_end))

  if (nrow(dat) == 0) stop("No data in the requested year ranges.")

  # Label periods
  dat <- dat %>%
    dplyr::mutate(period = dplyr::case_when(
      year >= baseline_start & year <= baseline_end ~ "baseline",
      year >= compare_start  & year <= compare_end  ~ "compare",
      TRUE ~ NA_character_
    )) %>%
    dplyr::filter(!is.na(period))

  # Period-level summaries within each draw + species
  # (Using median across years within period, consistent with your baseline logic)
  period_draw <- dat %>%
    dplyr::group_by(.draw, .data[[species_col]], period) %>%
    dplyr::summarise(
      period_epred = median(.epred, na.rm = TRUE),
      period_ir    = median(ir,     na.rm = TRUE),
      period_count = median(count,  na.rm = TRUE),  # "reported" count median across years
      period_pop   = median(population, na.rm = TRUE),
      .groups = "drop"
    )

  # Wide format: baseline vs compare within each draw/species
  wide <- period_draw %>%
    tidyr::pivot_wider(
      names_from = period,
      values_from = c(period_epred, period_ir, period_count, period_pop)
    )

  # Check both periods exist per species/draw (some may be missing)
  wide <- wide %>%
    dplyr::filter(!is.na(period_ir_baseline) & !is.na(period_ir_compare))

  if (nrow(wide) == 0) stop("No draw/species rows have data in BOTH periods.")

  # Compute RR and percent change within each draw/species
  wide <- wide %>%
    dplyr::mutate(
      rr_ir = period_ir_compare / period_ir_baseline,
      pct_change_ir = ((period_ir_compare - period_ir_baseline) / period_ir_baseline) * 100
    )

  # Summarise across draws by species
  out <- wide %>%
    dplyr::group_by(.data[[species_col]]) %>%
    dplyr::summarise(
      baseline_period = paste0(baseline_start, "-", baseline_end),
      compare_period  = paste0(compare_start,  "-", compare_end),

      # Baseline: estimated count & IR (from epred & IR)
      baseline_epred_median = round(median(period_epred_baseline, na.rm = TRUE), 6),
      baseline_epred_mean   = round(mean(period_epred_baseline,   na.rm = TRUE), 6),
      baseline_epred_q025   = round(stats::quantile(period_epred_baseline, 0.025, na.rm = TRUE), 6),
      baseline_epred_q975   = round(stats::quantile(period_epred_baseline, 0.975, na.rm = TRUE), 6),
      baseline_epred_hdi_lo = round(hdi_low(period_epred_baseline), 6),
      baseline_epred_hdi_hi = round(hdi_high(period_epred_baseline), 6),

      baseline_ir_median = round(median(period_ir_baseline, na.rm = TRUE), 6),
      baseline_ir_mean   = round(mean(period_ir_baseline,   na.rm = TRUE), 6),
      baseline_ir_q025   = round(stats::quantile(period_ir_baseline, 0.025, na.rm = TRUE), 6),
      baseline_ir_q975   = round(stats::quantile(period_ir_baseline, 0.975, na.rm = TRUE), 6),
      baseline_ir_hdi_lo = round(hdi_low(period_ir_baseline), 6),
      baseline_ir_hdi_hi = round(hdi_high(period_ir_baseline), 6),

      # Compare: estimated count & IR
      compare_epred_median = round(median(period_epred_compare, na.rm = TRUE), 6),
      compare_epred_mean   = round(mean(period_epred_compare,   na.rm = TRUE), 6),
      compare_epred_q025   = round(stats::quantile(period_epred_compare, 0.025, na.rm = TRUE), 6),
      compare_epred_q975   = round(stats::quantile(period_epred_compare, 0.975, na.rm = TRUE), 6),
      compare_epred_hdi_lo = round(hdi_low(period_epred_compare), 6),
      compare_epred_hdi_hi = round(hdi_high(period_epred_compare), 6),

      compare_ir_median = round(median(period_ir_compare, na.rm = TRUE), 6),
      compare_ir_mean   = round(mean(period_ir_compare,   na.rm = TRUE), 6),
      compare_ir_q025   = round(stats::quantile(period_ir_compare, 0.025, na.rm = TRUE), 6),
      compare_ir_q975   = round(stats::quantile(period_ir_compare, 0.975, na.rm = TRUE), 6),
      compare_ir_hdi_lo = round(hdi_low(period_ir_compare), 6),
      compare_ir_hdi_hi = round(hdi_high(period_ir_compare), 6),

      # RR and % change (IR)
      rr_ir_median = round(median(rr_ir, na.rm = TRUE), 6),
      rr_ir_q025   = round(stats::quantile(rr_ir, 0.025, na.rm = TRUE), 6),
      rr_ir_q975   = round(stats::quantile(rr_ir, 0.975, na.rm = TRUE), 6),
      rr_ir_hdi_lo = round(hdi_low(rr_ir), 6),
      rr_ir_hdi_hi = round(hdi_high(rr_ir), 6),

      pct_change_ir_median = round(median(pct_change_ir, na.rm = TRUE), 6),
      pct_change_ir_q025   = round(stats::quantile(pct_change_ir, 0.025, na.rm = TRUE), 6),
      pct_change_ir_q975   = round(stats::quantile(pct_change_ir, 0.975, na.rm = TRUE), 6),
      pct_change_ir_hdi_lo = round(hdi_low(pct_change_ir), 6),
      pct_change_ir_hdi_hi = round(hdi_high(pct_change_ir), 6),

      # Optional checks from reported data (not epred)
      baseline_raw_count_median = round(median(period_count_baseline, na.rm = TRUE), 6),
      compare_raw_count_median  = round(median(period_count_compare,  na.rm = TRUE), 6),
      baseline_pop_median       = round(median(period_pop_baseline,   na.rm = TRUE), 6),
      compare_pop_median        = round(median(period_pop_compare,    na.rm = TRUE), 6),

      .groups = "drop"
    )

  # Write to file if specified
  if (!is.null(output_file)) {
    dir_path <- dirname(output_file)
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(out, output_file, row.names = FALSE)
  }

  return(out)
}
