## leadingVegTypeByAgeClass ------------------------------------------------

## Abie_sp  (7 pixels): Young=3, Immature=2, Mature=1, Old=1
## Pice_mar (9 pixels): Young=0, Immature=4, Mature=4, Old=1

testthat::test_that("leadingVegTypeByAgeClass returns data.table with correct columns", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- leadingVegTypeByAgeClass(vtm, sam, poly, "polyName", id = "rep01")

  testthat::expect_s3_class(result, "data.table")
  testthat::expect_named(
    result,
    c("zone", "ageClass", "vegCover", "NPixels", "proportion", "group"),
    ignore.order = TRUE
  )
})

testthat::test_that("leadingVegTypeByAgeClass includes all zone x ageClass x vegCover combos", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- leadingVegTypeByAgeClass(vtm, sam, poly, "polyName", id = "rep01")

  expected_n <- 1L * 4L * 3L  ## 1 zone x 4 ageClasses x (2 spp + "All_species")
  testthat::expect_equal(nrow(result), expected_n)
  testthat::expect_all_true(c("Abie_sp", "Pice_mar", "All_species") %in% result$vegCover)
  testthat::expect_all_true(.ageClasses %in% result$ageClass)
})

testthat::test_that("leadingVegTypeByAgeClass NPixels are correct", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- leadingVegTypeByAgeClass(vtm, sam, poly, "polyName", id = "rep01")

  abie <- result[vegCover == "Abie_sp"]
  testthat::expect_equal(abie[ageClass == "Young",    NPixels], 3L)
  testthat::expect_equal(abie[ageClass == "Immature", NPixels], 2L)
  testthat::expect_equal(abie[ageClass == "Mature",   NPixels], 1L)
  testthat::expect_equal(abie[ageClass == "Old",      NPixels], 1L)

  pice <- result[vegCover == "Pice_mar"]
  testthat::expect_equal(pice[ageClass == "Young",    NPixels], 0L)
  testthat::expect_equal(pice[ageClass == "Immature", NPixels], 4L)
  testthat::expect_equal(pice[ageClass == "Mature",   NPixels], 4L)
  testthat::expect_equal(pice[ageClass == "Old",      NPixels], 1L)
})

testthat::test_that("leadingVegTypeByAgeClass proportions sum to 1 per species per zone", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- leadingVegTypeByAgeClass(vtm, sam, poly, "polyName", id = "rep01")

  sums <- result[vegCover != "All_species",
                 .(s = sum(proportion)), by = .(zone, vegCover)]
  testthat::expect_equal(sums$s, rep(1, nrow(sums)), tolerance = 0.001)
})

testthat::test_that("leadingVegTypeByAgeClass All_species proportions sum to 1 per zone", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- leadingVegTypeByAgeClass(vtm, sam, poly, "polyName", id = "rep01")

  sums <- result[vegCover == "All_species", .(s = sum(proportion)), by = zone]
  testthat::expect_equal(sums$s, rep(1, nrow(sums)), tolerance = 0.001)
})

testthat::test_that("leadingVegTypeByAgeClass group column equals id", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- leadingVegTypeByAgeClass(vtm, sam, poly, "polyName", id = "rep01")

  testthat::expect_equal(unique(result$group), "rep01")
})

testthat::test_that("leadingVegTypeByAgeClass respects custom ageClasses", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- leadingVegTypeByAgeClass(
    vtm, sam, poly, "polyName", id = "rep01",
    ageClasses      = c("Early", "Late"),
    ageClassCutOffs = c(0L, 80L)
  )

  testthat::expect_setequal(unique(result$ageClass), c("Early", "Late"))
})

## calculateLeadingVegTypeByAgeClass ---------------------------------------

testthat::test_that("calculateLeadingVegTypeByAgeClass combines results from all reps", {
  td <- withr::local_tempdir()
  for (d in c("rep01", "rep02", "CC")) dir.create(file.path(td, d))

  vtm_files <- c(
    .write_vtm(file.path(td, "rep01")),
    .write_vtm(file.path(td, "rep02")),
    .write_vtm(file.path(td, "CC"))
  )
  sam_files <- c(
    .write_sam(file.path(td, "rep01")),
    .write_sam(file.path(td, "rep02")),
    .write_sam(file.path(td, "CC"))
  )
  poly <- .make_poly(vtm_files[[1]])

  result <- calculateLeadingVegTypeByAgeClass(poly, "polyName", vtm_files, sam_files)

  testthat::expect_s3_class(result, "data.table")
  testthat::expect_setequal(unique(result$group), c("rep01", "rep02", "CC"))
})

testthat::test_that("calculateLeadingVegTypeByAgeClass group names come from vtm directories", {
  td <- withr::local_tempdir()
  for (d in c("run_A", "run_B")) dir.create(file.path(td, d))

  vtm_files <- c(.write_vtm(file.path(td, "run_A")), .write_vtm(file.path(td, "run_B")))
  sam_files <- c(.write_sam(file.path(td, "run_A")), .write_sam(file.path(td, "run_B")))
  poly <- .make_poly(vtm_files[[1]])

  result <- calculateLeadingVegTypeByAgeClass(poly, "polyName", vtm_files, sam_files)

  testthat::expect_setequal(unique(result$group), c("run_A", "run_B"))
})
