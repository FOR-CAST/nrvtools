## Pixel group map exercising the edge cases: NA cells, a 0 (no pixel group), and a group (9) that
## the reduced table does not contain.
.mock_pixel_groups <- function() {
  m <- matrix(
    # fmt: skip
    c(3,  3, 1, 1, NA,
      3,  5, 1, 2,  2,
      0,  5, 5, 2,  7,
      4,  4, 6, 6,  7,
      4, NA, 6, 9,  9),
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )
  terra::rast(m, crs = "EPSG:3857")
}

## Cohort-level rows: several per pixel group, with B differing between the rows of a group so that
## first-row matching is observable, and a group (8) absent from the map.
.mock_reduced <- function() {
  data.table::data.table(
    newPixelGroup = c(1L, 1L, 2L, 3L, 3L, 3L, 4L, 5L, 6L, 6L, 7L, 8L),
    B = c(100, 50, 3000, 20, 10, 5, 800, 2500, 400, 300, 150, 9000),
    n = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L),
    SeralStage = factor(
      # fmt: skip
      c("mid", "mid", "old_Fir", "early", "early", "early", "mature_Pine", "old_Fir",
        "mid", "mid", "early_Other", "old"),
      levels = .seralStagesBC
    )
  )
}

testthat::test_that(".paintPixelGroups() paints a factor column as a categorical raster", {
  pgm <- .mock_pixel_groups()
  ssm <- .paintPixelGroups(.mock_reduced(), pgm, "SeralStage", "newPixelGroup")

  testthat::expect_identical(as.vector(terra::ext(ssm)), as.vector(terra::ext(pgm)))
  testthat::expect_identical(terra::crs(ssm), terra::crs(pgm))
  ## terra names a categorical layer after its active category column
  testthat::expect_identical(names(ssm), "values")
  testthat::expect_identical(
    terra::values(ssm, mat = FALSE),
    # fmt: skip
    c(1,  1,  5,  5, NA,
      1, 14,  5, 14, 14,
     NA, 14, 14, 14,  4,
     11, 11,  5,  5,  4,
     11, NA,  5, NA, NA)
  )
  ## only the classes present, in order of first appearance
  testthat::expect_identical(
    terra::cats(ssm),
    list(data.frame(
      id = c(1L, 5L, 14L, 4L, 11L),
      values = c("early", "mid", "old_Fir", "early_Other", "mature_Pine")
    ))
  )
})

testthat::test_that(".paintPixelGroups() takes the first matching row", {
  b <- .paintPixelGroups(.mock_reduced(), .mock_pixel_groups(), "B", "newPixelGroup")

  testthat::expect_identical(names(b), "B")
  testthat::expect_identical(terra::is.factor(b), FALSE)
  testthat::expect_identical(
    terra::values(b, mat = FALSE),
    # fmt: skip
    c(  20,   20,  100,  100,   NA,
        20, 2500,  100, 3000, 3000,
        NA, 2500, 2500, 3000,  150,
       800,  800,  400,  400,  150,
       800,   NA,  400,   NA,   NA)
  )
})

testthat::test_that(".paintPixelGroups() paints integer, float and categorical maps alike", {
  tmp <- withr::local_tempdir()
  pgm <- .mock_pixel_groups()
  reduced <- .mock_reduced()

  maps <- lapply(c("INT1U", "INT4U", "FLT4S", "FLT8S"), function(dt) {
    f <- file.path(tmp, paste0("pgm_", dt, ".tif"))
    terra::writeRaster(pgm, f, datatype = dt, NAflag = if (dt == "INT1U") 255 else NA)
    terra::rast(f)
  })
  categorical <- terra::deepcopy(pgm)
  levels(categorical) <- data.frame(ID = c(0, 1:7, 9), group = as.character(c(0, 1:7, 9)))
  maps <- c(maps, categorical)

  for (col in c("B", "n", "SeralStage")) {
    expected <- .paintPixelGroups(reduced, pgm, col, "newPixelGroup")
    for (map in maps) {
      out <- .paintPixelGroups(reduced, map, col, "newPixelGroup")
      testthat::expect_identical(
        terra::values(out, mat = FALSE),
        terra::values(expected, mat = FALSE)
      )
      testthat::expect_identical(terra::cats(out), terra::cats(expected))
    }
  }
})

testthat::test_that(".paintPixelGroups() keys a categorical map by its active labels", {
  groups <- c(0, 1:7, 9)
  pgm <- .mock_pixel_groups()
  reduced <- .mock_reduced()
  expected <- terra::values(.paintPixelGroups(reduced, pgm, "B", "newPixelGroup"), mat = FALSE)
  painted <- function(reduced, map) {
    terra::values(.paintPixelGroups(reduced, map, "B", "newPixelGroup"), mat = FALSE)
  }

  ## cell codes that differ from the labels, in a category table not sorted by code
  relabelled <- pgm + 100
  levels(relabelled) <- data.frame(ID = rev(groups) + 100, group = as.character(rev(groups)))
  testthat::expect_identical(painted(reduced, relabelled), expected)

  ## the active label column, not the first
  twoLabels <- pgm + 100
  levels(twoLabels) <- data.frame(
    ID = groups + 100,
    other = paste0("x", groups),
    group = as.character(groups)
  )
  terra::activeCat(twoLabels) <- 2
  testthat::expect_identical(painted(reduced, twoLabels), expected)

  ## numeric ids that as.character() writes in scientific notation
  bigIds <- data.table::copy(reduced)
  data.table::set(bigIds, j = "newPixelGroup", value = reduced$newPixelGroup * 1e5)
  bigLabels <- pgm + 100
  levels(bigLabels) <- data.frame(
    ID = groups + 100,
    group = format(groups * 1e5, scientific = FALSE, trim = TRUE)
  )
  testthat::expect_identical(painted(bigIds, bigLabels), expected)

  ## character ids
  chrIds <- data.table::copy(reduced)
  data.table::set(chrIds, j = "newPixelGroup", value = paste0("pg", reduced$newPixelGroup))
  chrLabels <- pgm + 100
  levels(chrLabels) <- data.frame(ID = groups + 100, group = paste0("pg", groups))
  testthat::expect_identical(painted(chrIds, chrLabels), expected)
})

testthat::test_that("seralStageMapGeneratorBC() classifies plain and categorical pixel group maps alike", {
  testthat::skip_if_not_installed("qs2")
  tmp <- withr::local_tempdir()

  ## columns 1-3 are NDT3_SBS and 4-5 NDT4_IDF, so groups 2, 5 and 6 are split across both zones
  groups <- matrix(
    # fmt: skip
    c(1,  1, 2, 2, 3,
      1,  1, 2, 2, 3,
      4,  4, 5, 5, 3,
      4,  4, 5, 5, 6,
      0, NA, 6, 6, 6),
    nrow = 5,
    byrow = TRUE
  )
  template <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 500, ymin = 0, ymax = 500)
  terra::crs(template) <- "EPSG:3005"
  plain <- terra::rast(template, vals = as.vector(t(groups)))

  ndtbec <- sf::st_sf(
    NDTBEC = c("NDT3_SBS", "NDT4_IDF"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(300, 0), c(300, 500), c(0, 500), c(0, 0)))),
      sf::st_polygon(list(rbind(c(300, 0), c(500, 0), c(500, 500), c(300, 500), c(300, 0)))),
      crs = 3005
    )
  )
  ndtbec_f <- file.path(tmp, "ndtbec.gpkg")
  sf::st_write(ndtbec, ndtbec_f, quiet = TRUE)

  ## weighted ages: 1 = 20, 2 = 260 (Douglas-fir leading), 3 = 120 (pine), 4 = 60, 5 = 150 (fir), 6 = 50 (pine)
  cd_f <- file.path(tmp, "cohortData.qs2")
  qs2::qs_save(
    data.table::data.table(
      pixelGroup = c(1L, 2L, 2L, 3L, 4L, 5L, 6L),
      speciesCode = factor(c(
        "Pice_gla",
        "Pseu_men",
        "Pinu_con",
        "Pinu_con",
        "Popu_tre",
        "Pseu_men",
        "Pinu_con"
      )),
      age = c(20L, 260L, 260L, 120L, 60L, 150L, 50L),
      B = c(1000, 3000, 500, 2000, 800, 2000, 900)
    ),
    cd_f
  )

  write_map <- function(r, name) {
    f <- file.path(tmp, paste0(name, ".tif"))
    terra::writeRaster(r, f, datatype = "INT4U")
    f
  }
  ## codes differ from the labels, in a category table not sorted by code
  relabelled <- plain + 100
  levels(relabelled) <- data.frame(ID = rev(c(0:6)) + 100, group = as.character(rev(c(0:6))))
  identity <- terra::deepcopy(plain)
  levels(identity) <- data.frame(ID = 0:6, group = as.character(0:6))

  classes <- function(pgm_f) {
    ssm <- seralStageMapGeneratorBC(cd_f, pgm_f, ndtbec_f)
    lvls <- terra::cats(ssm)[[1]]
    lvls$values[match(terra::values(ssm, mat = FALSE), lvls$id)]
  }
  expected <- as.vector(t(matrix(
    # fmt: skip
    c("early", "early", "old", "old_Fir",    "mature_Pine",
      "early", "early", "old", "old_Fir",    "mature_Pine",
      "mid",   "mid",   "old", "mature_Fir", "mature_Pine",
      "mid",   "mid",   "old", "mature_Fir", "mid_Pine",
      NA,      NA,      "mid", "mid_Pine",   "mid_Pine"),
    nrow = 5,
    byrow = TRUE
  )))

  testthat::expect_identical(classes(write_map(plain, "plain")), expected)
  testthat::expect_identical(classes(write_map(relabelled, "relabelled")), expected)
  testthat::expect_identical(classes(write_map(identity, "identity")), expected)
})
