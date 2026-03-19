#' Terrestrial Ecodistricts of Canada
#'
#' @format ## `ecodistricts`
#' An `sf` polygons object with 1025 features.
#' \describe{
#'   \item{OBJECTID, SHAPE_Length, SHAPE_Area}{System-generated attributes}
#'   \item{ECODISTRICT_ID}{Ecodistrict identifier}
#'   \item{ECOREGION_ID}{Ecoregion identifier}
#'   \item{ECOZONE_ID}{Ecozone identifier}
#'   \item{ECOPROVINCE_ID}{Ecoprovince identifier}
#'   \item{geometry}{`sfc_POLYGON`}
#' }
#'
#' @source <https://open.canada.ca/data/en/dataset/fe9fd41c-1f67-4bc5-809d-05b62986b26b>
#'
"ecodistricts"

#' Terrestrial Ecoregions of Canada
#'
#' @format ## `ecoregions`
#' An `sf` polygons object with 218 features.
#' \describe{
#'   \item{OBJECTID, SHAPE_Length, SHAPE_Area}{System-generated attributes}
#'   \item{ECOREGION_ID}{Ecoregion identifier}
#'   \item{ECOZONE_ID}{Ecozone identifier}
#'   \item{ECOPROVINCE_ID}{Ecoprovince identifier}
#'   \item{ECOREGION_NAME_EN}{English ecoregion name}
#'   \item{ECOREGION_NAME_FR}{French ecoregion name}
#'   \item{geometry}{`sfc_POLYGON`}
#' }
#'
#' @source <https://open.canada.ca/data/en/dataset/ade80d26-61f5-439e-8966-73b352811fe6>
#'
"ecoregions"

#' Terrestrial Ecoprovinces of Canada
#'
#' @format ## `ecoprovinces`
#' An `sf` polygons object with 68 features.
#' \describe{
#'   \item{OBJECTID, SHAPE_Length, SHAPE_Area}{System-generated attributes}
#'   \item{ECOZONE_ID}{Ecozone identifier}
#'   \item{ECOPROVINCE_ID}{Ecoprovince identifier}
#'   \item{ECOPROVINCE_NAME_EN}{English ecoprovince name}
#'   \item{ECOPROVINCE_NAME_FR}{French ecoprovince name}
#'   \item{geometry}{`sfc_POLYGON`}
#' }
#'
#' @source <https://open.canada.ca/data/en/dataset/98fa7335-fbfe-4289-9a0e-d6bf3874b424>
#'
"ecoprovinces"

#' Terrestrial Ecozones of Canada
#'
#' @format ## `ecozones`
#' An `sf` polygons object with 25 features.
#' \describe{
#'   \item{OBJECTID, SHAPE_Length, SHAPE_Area}{System-generated attributes}
#'   \item{ECOZONE_ID}{Ecozone identifier}
#'   \item{ECOZONE_NAME_EN}{English ecozone name}
#'   \item{ECOZONE_NAME_FR}{French ecozone name}
#'   \item{geometry}{`sfc_POLYGON`}
#' }
#'
#' @source <https://open.canada.ca/data/en/dataset/7ad7ea01-eb23-4824-bccc-66adb7c5bdf8>
#'
"ecozones"
