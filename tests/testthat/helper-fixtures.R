## Shared raster fixtures for all test files.
## testthat auto-sources helper-*.R before running tests.

## 5x5, EPSG:32610, ext(0,5000,0,5000): 1000m pixels = 100 ha/pixel
## Species: 1 = Abie_sp, 2 = Pice_mar, 0 = non-forest
.write_vtm <- function(dir) {
  m <- matrix(
    c(
      ## fmt: table
      1, 1, 1, 1, 0,
      1, 1, 2, 2, 0,
      1, 2, 2, 2, 0,
      2, 2, 2, 2, 0,
      0, 0, 0, 0, 0
    ),
    nrow = 5, ncol = 5, byrow = TRUE
  )
  r <- terra::rast(m, crs = "EPSG:32610")
  terra::ext(r) <- terra::ext(0, 5000, 0, 5000)
  levels(r) <- data.frame(ID = 1:2, values = c("Abie_sp", "Pice_mar"))
  path <- file.path(dir, "vegTypeMap_year0100.tif")
  terra::writeRaster(r, path, overwrite = TRUE)
  path
}

## Ages spanning all four default classes:
##   Young [0,40), Immature [40,80), Mature [80,120), Old [120,Inf)
.write_sam <- function(dir) {
  m <- matrix(
    c(
      ## fmt: table
      20,  60, 100, 130, 0,
      10,  45,  50,  80, 0,
      30, 110,  90,  70, 0,
      50, 140,  85,  60, 0,
      0,   0,   0,   0, 0
    ),
    nrow = 5, ncol = 5, byrow = TRUE
  )
  r <- terra::rast(m, crs = "EPSG:32610")
  terra::ext(r) <- terra::ext(0, 5000, 0, 5000)
  path <- file.path(dir, "standAgeMap_year0100.tif")
  terra::writeRaster(r, path, overwrite = TRUE)
  path
}

## Single polygon covering the full vtm extent, polyName = "testPoly"
.make_poly <- function(vtm_path) {
  v <- terra::rast(vtm_path)
  poly <- terra::as.polygons(terra::ext(v), crs = terra::crs(v)) |> sf::st_as_sf()
  poly$polyName <- "testPoly"
  poly
}

## Two side-by-side polygons splitting the vtm extent at mid-x
.make_two_polys <- function(vtm_path) {
  v <- terra::rast(vtm_path)
  e <- as.vector(terra::ext(v))
  mid_x <- (e[["xmin"]] + e[["xmax"]]) / 2
  crs_v <- sf::st_crs(terra::crs(v))
  poly1 <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = e[["xmin"]], xmax = mid_x, ymin = e[["ymin"]], ymax = e[["ymax"]]),
    crs = crs_v
  ))
  poly2 <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = mid_x, xmax = e[["xmax"]], ymin = e[["ymin"]], ymax = e[["ymax"]]),
    crs = crs_v
  ))
  sf::st_sf(polyName = c("polyA", "polyB"), geometry = c(poly1, poly2))
}

## Create rep01 and CC subdirectories, write vtm/sam into each, return file paths
.setup_reps <- function(td) {
  dirs <- file.path(td, c("rep01", "CC"))
  lapply(dirs, dir.create)
  list(
    vtm = c(.write_vtm(dirs[[1]]), .write_vtm(dirs[[2]])),
    sam = c(.write_sam(dirs[[1]]), .write_sam(dirs[[2]]))
  )
}

## In-memory 5x5 categorical raster (CRS EPSG:3857): 1=forest, 2=grass, 3=water
.create_mock_vtm <- function() {
  m <- matrix(
    # fmt: skip
    c(1, 1, 1, 2, 2, 1, 1, 2, 2, 3, 1, 1, 2, 3, 3, 1, 2, 3, 3, 3, 2, 2, 3, 3, 3) ,
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )
  vtm <- terra::rast(m, crs = "EPSG:3857")
  levels(vtm) <- data.frame(ID = 1:3, values = c("forest", "grass", "water"))
  vtm
}

## In-memory 5x5 continuous raster (CRS EPSG:3857): values 1:25
.create_mock_sam <- function() {
  terra::rast(
    matrix(
      1:25 ,
      nrow = 5,
      ncol = 5,
      byrow = TRUE
    ),
    crs = "EPSG:3857"
  )
}
