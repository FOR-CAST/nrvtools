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

  ## includes ZONE and NATURAL_DISTURBANCE columns
  # targetCRS <- paste("+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=49 +lat_2=77",
  #                    "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ## LCC2010
  # becZones <- bcdata::bcdc_get_data("f358a53b-ffde-4830-a325-a5a03ff672c3") |>
  #   sf::st_cast("MULTIPOLYGON") |>
  #   sf::st_transform(targetCRS)

  studyArea3 <- map::studyArea(ml, 3) ## ml[[grep("\\(studyArea\\)", names(ml), value = TRUE)]]
  NDTBEC <- sf::st_crop(ml$`BEC zones`, studyArea3) |>
    dplyr::mutate(NDTBEC = paste0(NATURAL_DISTURBANCE, "_", ZONE)) |>
    dplyr::group_by(NDTBEC) |>
    dplyr::summarise()

  cd <- file.path(outputDir, "cohortData_year0800.qs")
  pgm <- file.path(outputDir, "pixelGroupMap_year0800.tif")

  ssm <- seralStageMapGeneratorBC(cd, pgm, NDTBEC)

  seral <- terra::levels(ssm)[[1]]
  ptchs <- landscapemetrics::get_patches(ssm)[[1]]
  names(ptchs) <- seral[match(sub("^class_", "", names(ptchs)), seral[["id"]]), "values"]

  expect_identical(names(ptchs), c("early", "mature", "mid", "old"))
})
