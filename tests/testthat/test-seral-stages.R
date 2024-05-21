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
  sf::st_write(ml$`ecoregionLayer (NDTxBEC)`, fNDTBEC, delete_dsn = TRUE)

  rm(ml)

  years <- c(0, 800, 1000, 1200) ## year 0 has no pixelGroup 0 values; other years do.
  lapply(years, function(yr) {
    cd <- file.path(outputDir, sprintf("cohortData_year%04d.qs", yr))
    pgm <- file.path(outputDir, sprintf("pixelGroupMap_year%04d.tif", yr))

    ssm <- seralStageMapGeneratorBC(cd, pgm, fNDTBEC)

    seral <- terra::levels(ssm)[[1]]
    ptchs <- landscapemetrics::get_patches(ssm)[[1]]
    names(ptchs) <- seral[match(sub("^class_", "", names(ptchs)), seral[["id"]]), "values"]

    expect_identical(names(ptchs), c("early", "mature", "mid", "old"))
  })
})
