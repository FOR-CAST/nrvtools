test_that("BC seral stage calculations work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("map")

  run <- 1L
  outputDir <- file.path("~/GitHub/BC_HRV/outputs",
                         "NRD_Quesnel_scfm_hrv_FRT_res125",
                         sprintf("rep%02d", run))

  skip_if_not(dir.exists(outputDir))

  ml <- readRDS(file.path(outputDir, "ml_preamble.rds"))

  fNDTBEC <- tempfile(fileext = ".shp")
  sf::st_write(ml$`ecoregionLayer (NDTxBEC)`, fNDTBEC, delete_dsn = TRUE, quiet = TRUE)

  rm(ml)

  years <- c(0, 800, 1000, 1200) ## year 0 has no pixelGroup 0 values; other years do.
  areas_df <- lapply(years, function(yr) {
    cd <- file.path(outputDir, sprintf("cohortData_year%04d.qs", yr))
    pgm <- file.path(outputDir, sprintf("pixelGroupMap_year%04d.tif", yr))

    ssm <- seralStageMapGeneratorBC(cd, pgm, fNDTBEC)

    areas <- patchAreasSeral(ssm) |>
      dplyr::mutate(rep = run, time = yr, poly = "NDTBEC")

    expect_identical(unique(areas$class), c("early", "mid", "mature", "old"))

    return(areas)
  }) |>
    dplyr::bind_rows()

  summary_df <- areas_df |>
    dplyr::group_by(class, time, poly, metric) |>
    dplyr::summarise(
      N = length(value),
      mm = ifelse(N > 0, min(value, na.rm = TRUE), NA_real_),
      q1 = ifelse(N > 0, quantile(value, 0.25, na.rm = TRUE), NA_real_),
      md = ifelse(N > 0, median(value, na.rm = TRUE), NA_real_),
      mn = ifelse(N > 0, mean(value, na.rm = TRUE), NA_real_),
      q3 = ifelse(N > 0, quantile(value, 0.75, na.rm = TRUE), NA_real_),
      mx = ifelse(N > 0, max(value, na.rm = TRUE), NA_real_),
      sd = ifelse(N > 0, sd(value, na.rm = TRUE), NA_real_),
      se = ifelse(N > 0, sd / sqrt(N), NA_real_),
      ci = ifelse(N > 1, se * qt(0.975, N - 1), NA_real_)
    ) |>
    dplyr::mutate(class = as.factor(class))
  levels(summary_df$class) <- c("early", "mid", "mature", "old")

  if (interactive()) {
    plot_by_class(dplyr::filter(summary_df, time > 0), type = "box", page = 1) +
      geom_point(data = dplyr::filter(summary_df, time == 0), col = "darkred", size = 2.5)

    plot_over_time_by_class(dplyr::filter(summary_df, time > 0), ylabel = "Mean area (ha)", page = 1)
  }

  unlink(fNDTBEC)
})
