utils::globalVariables(c(
  "age", "B", "NDTBEC", "newPixelGroup", "pixelGroup", "propB", "propB.x", "propB.y",
  "Rank", "SeralStage", "speciesCode", "totalB", "weightedAge"
))

#' Create map of seral stage classes for BC forests
#'
#' Seral stages based on Table 7 of Cariboo regional Biodiversity Conservation Strategy Report,
#' and Update Note 3 defining the IDF_Fir and IDF_Pine groups:
#'
#' paste0("https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/",
#'        "natural-resource-use/land-water-use/crown-land/land-use-plans-and-objectives/",
#'        "cariboo-region/cariboochilcotin-rlup/bio_strategy_report.pdf")
#'
#' paste0("https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/",
#'        "natural-resource-use/land-water-use/crown-land/land-use-plans-and-objectives/"
#'        "cariboo-region/cariboochilcotin-rlup/update_note_3.pdf")
#'
#' @template cd
#'
#' @template pgm
#'
#' @param ndtbec `sf` polygons object delineating BEC zanes and Natural Disturbance Type polygons.
#'
#' @return seral stage map (`SpatRaster`)
#'
#' @export
#'
#' @examples
#' if (require("bcdata", quietly = TRUE)) {
#'   becZones <- bcdata::bcdc_get_data("f358a53b-ffde-4830-a325-a5a03ff672c3")
#' }
seralStageMapGeneratorBC <- function(cd, pgm, ndtbec) {
  cohortData <- qs::qread(cd)
  pixelGroupMap <- terra::rast(pgm)

  rstNDTBEC <- terra::rasterize(ndtbec, pixelGroupMap, "NDTBEC")
  assertthat::assert_that(terra::compareGeom(rstNDTBEC, pixelGroupMap))

  lvls <- terra::levels(rstNDTBEC)[[1]]
  ndtbec <- lvls[match(values(rstNDTBEC, mat = FALSE), lvls$ID), "NDTBEC"]
  assertthat::assert_that(terra::ncell(pixelGroupMap) == length(ndtbec))

  pgmByNdtbec <- data.table(
    pixelID = seq(terra::ncell(pixelGroupMap)),
    pixelGroup = terra::values(pixelGroupMap, mat = FALSE),
    NDTBEC = ndtbec
  ) |>
    na.omit("pixelGroup")
  pgmByNdtbec <- pgmByNdtbec[pixelGroup > 0, ] |> unique()

  ## reassign pixelGoup ids to account for NDTBEC
  pgmByNdtbec[, newPixelGroup := .I]

  rcl <- data.table::copy(pgmByNdtbec) |>
    set(NULL, "pixelID", NULL) |>
    set(NULL, "NDTBEC", NULL) |>
    as.matrix()

  pixelGroupMap2 <- terra::classify(pixelGroupMap, rcl) ## zeroes stay zeroes

  cohortData2 <- data.table::copy(cohortData)
  cohortData2 <- cohortData2[pgmByNdtbec, on = "pixelGroup"]

  PgNdtBec <- c("newPixelGroup")
  SpPgNdtBec <- c("speciesCode", "newPixelGroup")

  setkey(cohortData2, newPixelGroup)
  cohortData2[, totalB := sum(B, na.rm = TRUE), by = PgNdtBec]
  cohortData2[, propB := sum(B, na.rm = TRUE) / totalB[1], by = SpPgNdtBec]
  cohortData2[, weightedAge := floor(sum(age * B) / sum(B) / 10) * 10, by = PgNdtBec]
  set(cohortData2, NULL, c("age", "aNPPAct", "B", "ecoregionGroup", "mortality", "totalB"), NULL)
  cohortData2 <- unique(cohortData2)

  ## seral stages based on Table 7 of Cariboo regional Biodiversity Conservation Strategy Report,
  ## and Update Note 3 defining the IDF_Fir and IDF_Pine groups:
  ##
  ## paste0("https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/",
  ##        "natural-resource-use/land-water-use/crown-land/land-use-plans-and-objectives/",
  ##        "cariboo-region/cariboochilcotin-rlup/bio_strategy_report.pdf")
  ##
  ## paste0("https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/",
  ##        "natural-resource-use/land-water-use/crown-land/land-use-plans-and-objectives/"
  ##        "cariboo-region/cariboochilcotin-rlup/update_note_3.pdf")

  ## NDT1
  cohortData2[NDTBEC %in% c("NDT1_ESSF", "NDT1_MH") & weightedAge < 40, SeralStage := "early"]
  cohortData2[NDTBEC %in% c("NDT1_ESSF", "NDT1_MH") & weightedAge >= 40 & weightedAge < 120, SeralStage := "mid"]
  cohortData2[NDTBEC %in% c("NDT1_ESSF", "NDT1_MH") & weightedAge >= 120 & weightedAge < 250, SeralStage := "mature"]
  cohortData2[NDTBEC %in% c("NDT1_ESSF", "NDT1_MH") & weightedAge >= 250, SeralStage := "old"]

  cohortData2[NDTBEC == "NDT1_ICH" & weightedAge < 40, SeralStage := "early"]
  cohortData2[NDTBEC == "NDT1_ICH" & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[NDTBEC == "NDT1_ICH" & weightedAge >= 100 & weightedAge < 250, SeralStage := "mature"]
  cohortData2[NDTBEC == "NDT1_ICH" & weightedAge >= 250, SeralStage := "old"]

  ## NDT2
  cohortData2[NDTBEC == "NDT2_CWH" & weightedAge < 40, SeralStage := "early"]
  cohortData2[NDTBEC == "NDT2_CWH" & weightedAge >= 40 & weightedAge < 80, SeralStage := "mid"]
  cohortData2[NDTBEC == "NDT2_CWH" & weightedAge >= 80 & weightedAge < 250, SeralStage := "mature"]
  cohortData2[NDTBEC == "NDT2_CWH" & weightedAge >= 250, SeralStage := "old"]

  cohortData2[NDTBEC == "NDT2_ESSF" & weightedAge < 40, SeralStage := "early"]
  cohortData2[NDTBEC == "NDT2_ESSF" & weightedAge >= 40 & weightedAge < 120, SeralStage := "mid"]
  cohortData2[NDTBEC == "NDT2_ESSF" & weightedAge >= 120 & weightedAge < 250, SeralStage := "mature"]
  cohortData2[NDTBEC == "NDT2_ESSF" & weightedAge >= 250, SeralStage := "old"]

  cohortData2[NDTBEC %in% c("NDT2_ICH", "NDT2_SBS") & weightedAge < 40, SeralStage := "early"]
  cohortData2[NDTBEC %in% c("NDT2_ICH", "NDT2_SBS") & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[NDTBEC %in% c("NDT2_ICH", "NDT2_SBS") & weightedAge >= 100 & weightedAge < 250, SeralStage := "mature"]
  cohortData2[NDTBEC %in% c("NDT2_ICH", "NDT2_SBS") & weightedAge >= 250, SeralStage := "old"]

  ## NDT3
  cohortData2[grepl("NDT3", NDTBEC) & weightedAge < 40, SeralStage := "early"]
  cohortData2[grepl("NDT3", NDTBEC) & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[grepl("NDT3", NDTBEC) & weightedAge >= 100 & weightedAge < 140, SeralStage := "mature"]
  cohortData2[grepl("NDT3", NDTBEC) & weightedAge >= 140, SeralStage := "old"]

  ## NDT4 -- requires further specification by Pine / Fir group before assigning seral stages
  cohortDataNDT4 <- cohortData2[NDTBEC %in% c("NDT4_BG", "NDT4_IDF"), ]

  ## all spruce considered equal here, so merge them together
  allSpecies <- levels(cohortDataNDT4$speciesCode)
  allSpecies[grepl("^Pice_", allSpecies)] <- "Pice_sp"
  allSpecies <- unique(allSpecies)

  cohortDataNDT4[, speciesCode := as.character(speciesCode)]
  cohortDataNDT4[grepl("^Pice_", speciesCode), speciesCode := "Pice_sp"]
  cohortDataNDT4[grepl("^Pice_", speciesCode), propB := sum(propB, na.rm = TRUE), by = SpPgNdtBec]
  cohortDataNDT4 <- unique(cohortDataNDT4)

  fulldt <- expand.grid(
    speciesCode = allSpecies,
    newPixelGroup = unique(cohortDataNDT4$newPixelGroup),
    NDTBEC = unique(cohortDataNDT4$NDTBEC),
    stringsAsFactors = FALSE
  ) |> as.data.table()
  fulldt[, propB := 0.0]

  cohortDataNDT4 <- merge(cohortDataNDT4, fulldt, by = SpPgNdtBec, all = TRUE)
  cohortDataNDT4[!is.na(propB.x), propB := propB.x]
  cohortDataNDT4[is.na(propB.x), propB := propB.y]
  set(cohortDataNDT4, NULL, c("propB.x", "propB.y"), NULL)

  cohortDataNDT4[, Rank := as.integer(rank(-propB, ties.method = "min")), by = PgNdtBec]

  ## NOTE: 'leading' is simply most abundant species, but we'll use 0.5 here
  vegLeadingProportion <- 0.5

  pgFir1 <- cohortDataNDT4[speciesCode == "Pseu_men" & Rank == 1 & propB >= vegLeadingProportion, ]$newPixelGroup
  pgFir2 <- cohortDataNDT4[speciesCode == "Pinu_pon" & Rank == 1 & propB >= vegLeadingProportion, ]$newPixelGroup
  pgFir3 <- cohortDataNDT4[speciesCode == "Pinu_con" & Rank == 1 & propB >= vegLeadingProportion, ]$newPixelGroup
  pgFir3 <- cohortDataNDT4[(newPixelGroup %in% pgFir3) &
                             (speciesCode %in% c("Pinu_pon", "Pseu_men") & Rank > 1 & propB > 0.15), ]$newPixelGroup
  pgFir4 <- cohortDataNDT4[(speciesCode == "Popu_tre" & Rank == 1 & propB >= vegLeadingProportion), ]$newPixelGroup
  pgFir4 <- cohortDataNDT4[(newPixelGroup %in% pgFir4) &
                             (speciesCode %in% c("Pinu_pon", "Pseu_men") & Rank > 1 & propB > 0.15), ]$newPixelGroup
  pgFir4 <- cohortDataNDT4[(newPixelGroup %in% pgFir4) &
                             (grepl("^(Betu_sp|Pice_sp|Popu_sp|Thuj_pli)", speciesCode) & Rank > 1 & propB < 0.06), ]$newPixelGroup
  pgFir <- c(pgFir1, pgFir2, pgFir3, pgFir4) |> unique()

  ## 2024-05-02 everything else not in fir group is in pine group; keep old code for now
  pgPine <- cohortDataNDT4[!(newPixelGroup %in% pgFir), ]$newPixelGroup

  if (FALSE) {
    ## TODO: this does not catch all possible non-fir species combinations
    pgPine1 <- cohortDataNDT4[(speciesCode == "Pinu_con" & Rank == 1 & propB >= vegLeadingProportion), ]$newPixelGroup
    pgPine1 <- cohortDataNDT4[(newPixelGroup %in% pgPine1) & (speciesCode == "Pinu_pon" & propB <= 0.15), ]$newPixelGroup
    pgPine1 <- cohortDataNDT4[(newPixelGroup %in% pgPine1) & (speciesCode == "Pseu_men" & propB <= 0.15), ]$newPixelGroup

    pgPine2 <- cohortDataNDT4[(speciesCode == "Popu_tre" & Rank == 1 & propB >= vegLeadingProportion), ]$newPixelGroup
    pgPine2 <- cohortDataNDT4[(newPixelGroup %in% pgPine2) &
                                ((speciesCode %in% c("Pinu_pon", "Pseu_men") & Rank > 1 & propB <= 0.15) |
                                   (grepl("^(Betu_sp|Pice_sp|Popu_sp|Thuj_pli)", speciesCode) & Rank > 1 & propB >= 0.06)), ]$newPixelGroup
    pgPine3 <- cohortDataNDT4[grepl("^Pice_sp", speciesCode) & Rank == 1 & propB >= vegLeadingProportion, ]$newPixelGroup
    pgPine4 <- cohortDataNDT4[grepl("^Thuj_pli", speciesCode) & Rank == 1 & propB >= vegLeadingProportion, ]$newPixelGroup
    pgPine5 <- cohortDataNDT4[grepl("^Popu_sp", speciesCode) & Rank == 1 & propB >= vegLeadingProportion, ]$newPixelGroup
    pgPine6 <- cohortDataNDT4[grepl("^Betu_sp", speciesCode) & Rank == 1 & propB >= vegLeadingProportion, ]$newPixelGroup
    pgPine <- c(pgPine1, pgPine2, pgPine3, pgPine4, pgPine5, pgPine6)
  }

  assertthat::assert_that(!any(pgFir %in% pgPine))

  ## TODO: 'other' here corresponds to newPixelGroups that don't fall into either Fir or Pine categories
  ## keep track of them for now, but need to dive into these further (e.g., Pinu_alb leading)
  pgNDT4 <- unique(cohortDataNDT4[propB > 0, newPixelGroup])
  pgOther <- pgNDT4[!pgNDT4 %in% c(pgFir, pgPine)]

  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgFir, NDTBEC := paste0(NDTBEC, "_Fir")]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgFir & weightedAge < 40, SeralStage := "early"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgFir & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgFir & weightedAge >= 100 & weightedAge < 250, SeralStage := "mature"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgFir & weightedAge >= 250, SeralStage := "old"]

  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine, NDTBEC := paste0(NDTBEC, "_Pine")]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge < 40, SeralStage := "early"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge >= 100 & weightedAge < 140, SeralStage := "mature"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge >= 140, SeralStage := "old"]

  ## 'other' matches pine group seral stages
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther, NDTBEC := paste0(NDTBEC, "_Other")]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge < 40, SeralStage := "early"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge >= 100 & weightedAge < 140, SeralStage := "mature"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge >= 140, SeralStage := "old"]

  ## TODO: some rows have B == 0 | age == 0. set to 'early' as though recently disturbed? or omitted?
  cohortData2[!grepl("^NDT5_", NDTBEC) & is.na(SeralStage), SeralStage := "early"]

  assertthat::assert_that(NROW(cohortData2[!grepl("^NDT5_", NDTBEC) & is.na(SeralStage), ]) == 0)

  cohortData2[, SeralStage := as.factor(SeralStage)] ## needs to be a factor for rasterizedReduced

  ## build seral stage map raster from cohortData2 and pixelGroupMap2
  ssm <- rasterizeReduced(cohortData2, pixelGroupMap2, "SeralStage", mapcode = "newPixelGroup")

  return(ssm)
}

## TODO: seral stage versions of:
## - calculatePatchMetrics(..., vtm, sam) ==> calculatePatchMetricsSeral(..., ssm)
## - patchMetrics(sim) ==> patchMetricsSeral(sim)
