utils::globalVariables(c(
  "age", "B", "NDTBEC", "newPixelGroup", "pixelGroup", "propB", "propB.x", "propB.y",
  "Rank", "SeralStage", "speciesCode", "totalB", "weightedAge"
))

#' Seral stage classes for BC forests
#'
#' "early", "mid", "mature", and "old" are for NDT1, NDT2, NDT3;
#' "early_Fir", "mid_Fir", "mature_Fir", and "old_Fir" are for NDT4 with Fir leading;
#' "early_Pine", "mid_Pine", "mature_Pine", and "old_Pine" are for NDT4 with Pine leading;
#' "early_Other", "mid_Other", "mature_Other", and "old_Other" are for NDT4 with Other leading.
#'
#' @keywords internal
.seralStagesBC <- c("early", "early_Fir", "early_Pine", "early_Other",
                    "mid", "mid_Fir", "mid_Pine", "mid_Other",
                    "mature", "mature_Fir", "mature_Pine", "mature_Other",
                    "old", "old_Fir", "old_Pine", "old_Other")

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
#' @param ndtbec character specifying the file path to e.g., a shapefile delineating
#'        BEC zones and Natural Disturbance Type polygons.
#'
#' @return seral stage map (`SpatRaster`)
#'
#' @export
#' @rdname seralStageMapGeneratorBC
#'
#' @examples
#' \dontrun{
#' if (require("bcdata", quietly = TRUE)) {
#'   becZones <- bcdata::bcdc_get_data("f358a53b-ffde-4830-a325-a5a03ff672c3")
#' }
#' }
seralStageMapGeneratorBC <- function(cd, pgm, ndtbec) {
  stopifnot(
    is.character(cd) && file.exists(cd),
    is.character(pgm) && file.exists(pgm),
    is.character(ndtbec) && file.exists(ndtbec)
  )
  cohortData <- qs::qread(cd)
  pixelGroupMap <- terra::rast(pgm)
  NDTBEC <- sf::st_read(ndtbec, quiet = TRUE)

  rstNDTBEC <- terra::rasterize(NDTBEC, pixelGroupMap, "NDTBEC")
  assertthat::assert_that(terra::compareGeom(rstNDTBEC, pixelGroupMap))

  lvls <- terra::levels(rstNDTBEC)[[1]]
  idcol <- which(grepl("id|ID", names(lvls)))
  ndtbec <- lvls[match(values(rstNDTBEC, mat = FALSE), lvls[[idcol]]), "NDTBEC"]
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
  nrows <- NROW(pgmByNdtbec)
  assertthat::assert_that(identical(nrows, length(unique(pgmByNdtbec$pixelID))))

  pixelGroupMap2 <- terra::deepcopy(pixelGroupMap)
  pixelGroupMap2[pgmByNdtbec$pixelID] <- pgmByNdtbec$newPixelGroup

  cohortData2 <- data.table::copy(cohortData)
  cohortData2 <- cohortData2[pgmByNdtbec, on = "pixelGroup"]

  PgNdtBec <- c("newPixelGroup")
  SpPgNdtBec <- c("speciesCode", "newPixelGroup")

  setkey(cohortData2, newPixelGroup)
  cohortData2[, totalB := sum(B, na.rm = TRUE), by = PgNdtBec]
  cohortData2[, propB := sum(B, na.rm = TRUE) / totalB[1], by = SpPgNdtBec]
  cohortData2[, weightedAge := floor(sum(age * B) / sum(B) / 10) * 10, by = PgNdtBec]
  for (col2rm in c("age", "aNPPAct", "B", "ecoregionGroup", "mortality", "totalB")) {
    if (col2rm %in% colnames(cohortData2)) {
      set(cohortData2, NULL, col2rm, NULL)
    }
  }
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
  cohortData2[grepl("^NDT3_", NDTBEC) & weightedAge < 40, SeralStage := "early"]
  cohortData2[grepl("^NDT3_", NDTBEC) & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[grepl("^NDT3_", NDTBEC) & weightedAge >= 100 & weightedAge < 140, SeralStage := "mature"]
  cohortData2[grepl("^NDT3_", NDTBEC) & weightedAge >= 140, SeralStage := "old"]

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
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgFir, SeralStage := paste0(SeralStage, "_Fir")]

  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine, NDTBEC := paste0(NDTBEC, "_Pine")]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge < 40, SeralStage := "early"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge >= 100 & weightedAge < 140, SeralStage := "mature"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine & weightedAge >= 140, SeralStage := "old"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgPine, SeralStage := paste0(SeralStage, "_Pine")]

  ## 'other' matches pine group seral stages
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther, NDTBEC := paste0(NDTBEC, "_Other")]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge < 40, SeralStage := "early"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge >= 40 & weightedAge < 100, SeralStage := "mid"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge >= 100 & weightedAge < 140, SeralStage := "mature"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther & weightedAge >= 140, SeralStage := "old"]
  cohortData2[grepl("NDT4", NDTBEC) & newPixelGroup %in% pgOther, SeralStage := paste0(SeralStage, "_Other")]

  ## TODO: some rows have B == 0 | age == 0. set to 'early' as though recently disturbed? or omit?
  cohortData2[!grepl("^NDT5_", NDTBEC) & is.na(SeralStage), SeralStage := "early"]

  assertthat::assert_that(NROW(cohortData2[!grepl("^NDT5_", NDTBEC) & is.na(SeralStage), ]) == 0)

  ## SeralStage needs to be a factor for rasterizedReduced
  cohortData2[, SeralStage := factor(SeralStage, levels = .seralStagesBC)]

  ## build seral stage map raster from cohortData2 and pixelGroupMap2
  ssm <- rasterizeReduced(cohortData2, pixelGroupMap2, "SeralStage", mapcode = "newPixelGroup")

  return(ssm)
}

#' Write multiple seral stage maps to disk
#'
#' Wraps `seralStageMapGeneratorBC()` in `future.apply::future_mapply()` to
#' writes multiple seral stage maps to disk, in parallel.
#' See `?future::plan` for configuring parallel processing.
#'
#' @param ssm character, specifying the filename(s) to use for seral stage rasters.
#'        Defaults to using `ssm` as a template, replacing `pixelGroupMap` with `seralStageMap`.
#'
#' @return character vector of seral stage map filenames. invoked for side effect of writing to disk.
#'
#' @export
#' @rdname seralStageMapGeneratorBC
writeSeralStageMapBC <- function(cd, pgm, ndtbec, ssm = sub("pixelGroupMap", "seralStageMap", pgm)) {
  ## TODO: very slow; ¿because of automatic serializing of objs in the FUN envir?
  ## see <https://github.com/HenrikBengtsson/future.apply/issues/98>
  ## and <https://github.com/HenrikBengtsson/future/issues/608>
  future.apply::future_mapply(
    FUN = function(cd, pgm, ndtbec, ssm) {
      seralStageMapGeneratorBC(cd, pgm, ndtbec) |>
        terra::writeRaster(ssm, datatype = "INT1U", overwrite = TRUE)

      return(ssm)
    },
    cd = cd,
    pgm = pgm,
    ssm = ssm,
    MoreArgs = list(ndtbec = ndtbec),
    future.globals = FALSE,
    future.packages = c("nrvtools", "terra")
  ) |> unname()
}
