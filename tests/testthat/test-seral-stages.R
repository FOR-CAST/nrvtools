test_that("BC seral stage calculations work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("map")
  skip_if_not_installed("withr")

  run <- 8L
  outputDir <- file.path("~/GitHub/BC_HRV/outputs",
                         "NRD_Quesnel_scfm_hrv_FRT_res125",
                         sprintf("rep%02d", run))

  skip_if_not(all(dir.exists(outputDir)))

  test_dir <- file.path(tempdir(), "test-seralStages")
  dir.create(test_dir, showWarnings = FALSE)
  withr::local_dir(test_dir)

  ml <- readRDS(file.path(outputDir, "ml_preamble.rds"))
  studyArea2 <- map::studyArea(ml, 2)
  NDTBEC <- suppressWarnings({
    sf::st_crop(ml$`ecoregionLayer (NDTxBEC)`, studyArea2)
  })
  rm(ml)

  fNDTBEC <- file.path(test_dir, "NDTBEC.shp")
  sf::st_write(NDTBEC, fNDTBEC, append = FALSE, quiet = TRUE)
  withr::defer(unlink(fNDTBEC))

  years <- c(0, 800, 1000, 1200) ## year 0 has no pixelGroup 0 values; other years do.

  fcd <- vapply(outputDir, function(d) {
    file.path(d, sprintf("cohortData_year%04d.qs", years))
  }, character(length(years))) |> as.vector()

  fpgm <- vapply(outputDir, function(d) {
    file.path(d, sprintf("pixelGroupMap_year%04d.tif", years))
  }, character(length(years))) |> as.vector()

  repDirs <- file.path(test_dir, basename(dirname(fpgm))) |> unique()
  expect_true(all(vapply(repDirs, dir.create, recursive = TRUE, logical(1))))
  withr::defer(unlink(repDirs, recursive = TRUE))

  fssm <- file.path(repDirs, sub("pixelGroupMap", "seralStageMap", basename(fpgm)))

  areas_df <- lapply(seq_along(years), function(yr) {
    ssm <- seralStageMapGeneratorBC(fcd[yr], fpgm[yr], fNDTBEC)
    expect_true(all(levels(ssm)[[1]][["values"]] %in% .seralStagesBC))

    areas <- patchAreasSeral(ssm) |>
      dplyr::mutate(rep = run, time = years[yr], poly = "NDTBEC")

    expect_true(all(unique(areas$class) %in% .seralStagesBC))

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
    dplyr::mutate(class = factor(class, levels = .seralStagesBC))

  if (interactive()) {
    withr::local_package("ggplot2")

    plot_by_class(dplyr::filter(summary_df, time > 0), type = "box", page = 1) +
      geom_point(data = dplyr::filter(summary_df, time == 0), col = "darkred", size = 2.5)

    plot_over_time_by_class(dplyr::filter(summary_df, time > 0), ylabel = "Mean area (ha)", page = 1) +
      facet_wrap(~ class)
  }

  withr::deferred_run()
})

test_that("BC seral stage calculations work in parallel", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  skip_if_not_installed("map")
  skip_if_not_installed("withr")

  runs <- 1L:3L
  outputDirs <- file.path("~/GitHub/BC_HRV/outputs",
                          "NRD_Quesnel_scfm_hrv_FRT_res125",
                          sprintf("rep%02d", runs))

  skip_if_not(all(dir.exists(outputDirs)))

  withr::local_package("future.callr")
  oldPlan <- plan("callr", workers = 2L)
  withr::defer(plan(oldPlan))

  test_dir <- file.path(tempdir(), "test-seralStages")
  dir.create(test_dir, showWarnings = FALSE)
  withr::local_dir(test_dir)

  ml <- readRDS(file.path(outputDirs[1], "ml_preamble.rds"))
  studyArea2 <- map::studyArea(ml, 2)
  NDTBEC <- suppressWarnings({
    sf::st_crop(ml$`ecoregionLayer (NDTxBEC)`, studyArea2)
  })

  fNDTBEC <- file.path(test_dir, "NDTBEC.shp")
  sf::st_write(NDTBEC, fNDTBEC, append = FALSE, quiet = TRUE)
  withr::defer(unlink(fNDTBEC))

  years <- c(0, 800, 1000, 1200) ## year 0 has no pixelGroup 0 values; other years do.

  fcd <- vapply(outputDirs, function(d) {
    file.path(d, sprintf("cohortData_year%04d.qs", years))
  }, character(length(years))) |> as.vector()

  fpgm <- vapply(outputDirs, function(d) {
    file.path(d, sprintf("pixelGroupMap_year%04d.tif", years))
  }, character(length(years))) |> as.vector()

  repDirs <- file.path(test_dir, basename(dirname(fpgm))) |> unique()
  expect_true(all(vapply(repDirs, dir.create, recursive = TRUE, logical(1))))
  withr::defer(unlink(repDirs, recursive = TRUE))

  fssm <- file.path(repDirs, sub("pixelGroupMap", "seralStageMap", basename(fpgm)))

  ssmFiles <- writeSeralStageMapBC(cd = fcd, pgm = fpgm, ndtbec = fNDTBEC, ssm = fssm)

  expect_identical(fssm, ssmFiles)

  fflm <- file.path(dirname(outputDirs[1]), "rstFlammable.tif")
  fssm0 <- grep("seralStageMap_year0000.tif", ssmFiles, value = TRUE)
  fssm <- grep("seralStageMap_year0000.tif", ssmFiles, invert = TRUE, value = TRUE)

  funList <- default_patch_metrics_seral()[[1]] ## only test subset of metrics

  rptPolygons <- ml

  md <- rptPolygons@metadata
  cols <- which(grepl("analysisGroup", colnames(md)))
  rowIDs <- which(md[, ..cols] == "NRV_summary", arr.ind = TRUE)[, "row"]
  rptPolyNames <- md[["layerName"]][rowIDs]

  p <- rptPolyNames[1] ## only test subset of reporting polygons
  rptPoly <- rptPolygons[[p]]

  rptPoly <- suppressWarnings({
    sf::st_crop(rptPoly, studyArea2) ## ensure cropped to studyArea
  })
  rptPolyCol <- md[layerName == p, ][["columnNameForLabels"]]
  refCode <- paste0("sspm_", md[layerName == p, ][["shortName"]])
  refCodeCC <- paste0(refCode, "_CC")

  ## CC
  dfl_cc <- calculatePatchMetricsSeral(ssm = fssm0, flm = fflm,
                                       summaryPolys = rptPoly, polyCol = rptPolyCol,
                                       funList = funList)
  expect_true(all(NDTBEC$NDTBEC %in% unique(dfl_cc$patchAreasSeral$poly)))

  sdfl_cc <- suppressWarnings({
    summarizePatchMetricsSeral(dfl_cc)
  })
  expect_true(all(NDTBEC$NDTBEC %in% unique(sdfl_cc$patchAreasSeral$poly)))

  ## SIM
  dfl <- calculatePatchMetricsSeral(ssm = fssm, flm = fflm,
                                    summaryPolys = rptPoly, polyCol = rptPolyCol,
                                    funList = funList)
  expect_true(all(NDTBEC$NDTBEC %in% unique(dfl$patchAreasSeral$poly)))

  sdfl <- suppressWarnings({
    summarizePatchMetricsSeral(dfl)
  })
  expect_true(all(NDTBEC$NDTBEC %in% unique(sdfl$patchAreasSeral$poly)))

  if (interactive()) {
    withr::local_package("ggplot2")

    plot_by_class(sdfl$patchAreasSeral, type = "box", page = 1) +
      geom_point(data = sdfl_cc$patchAreasSeral, col = "darkred", size = 2.5)

    plot_over_time_by_class(sdfl$patchAreasSeral, ylabel = "Mean area (ha)", page = 1)
  }

  withr::deferred_run()
})
