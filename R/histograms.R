utils::globalVariables(c(
  ":=", ".N", ".SD",
  "ageClass", "group", "medianAge", "N", "NCC", "patch", "polygonName",
  "proportion", "proportionCC", "rep", "sizeInHa", "totalArea", "totalAreaCC",
  "totalPixels", "totalPixels2", "vegCover", "zone", "NPixels"
))

#' @keywords internal
.doPlotHistogram <- function(
  data,
  colName,
  colNameCC,
  xlim,
  force.min.n = FALSE,
  fname = NULL,
  ...
) {
  minNumBars <- 6
  maxNumBars <- 30
  rangeNClusters <- if (isTRUE(force.min.n)) {
    range(c(0, xlim, minNumBars))
  } else {
    xlim
  }
  attemptedNumBars <- max(minNumBars, min(maxNumBars, diff(rangeNClusters)))
  breaksRaw <- seq(rangeNClusters[1], rangeNClusters[2], length.out = attemptedNumBars)
  prettyBreaks <- if (isTRUE(force.min.n)) {
    pretty(breaksRaw, n = attemptedNumBars, min.n = min(attemptedNumBars, minNumBars))
  } else {
    pretty(breaksRaw, n = attemptedNumBars)
  }

  dataForBreaks <- dataForHistogram <- if (NROW(data) == 0) {
    ## add a bar at zero if there are no patches
    graphics::hist(0, plot = FALSE, breaks = prettyBreaks)
  } else {
    graphics::hist(data[[colName]], plot = FALSE, breaks = prettyBreaks)
  }

  breaksLabels <- dataForBreaks$breaks
  breaksInterval <- diff(breaksLabels)[1]

  histogramData <- dataForHistogram$counts / sum(dataForHistogram$counts) ## use proportion
  histogramData[is.na(histogramData)] <- 0

  barplotBreaks <- seq_along(breaksLabels) - 0.5
  addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks)
  verticalLineAtX <- unique(data[[colNameCC]])[1] / breaksInterval + 0.5 ## barplot x-axis is 1/2 a barwidth off

  if (!is.null(fname)) {
    grDevices::png(fname, width = 800, height = 600, units = "px")
  }
  graphics::barplot(histogramData, ...)
  if (!is.null(addAxisParams)) {
    do.call(graphics::axis, addAxisParams)
  }
  if (!is.null(verticalLineAtX)) {
    graphics::abline(v = verticalLineAtX, col = "red", lwd = 3)
  }
  if (!is.null(fname)) grDevices::dev.off()
}

#' Tabulate large patches by species and age class for one vtm/sam pair
#'
#' Identifies connected patches per species from `vtm`, assigns each patch a
#' median stand age from `sam`, classifies it into an age class, and returns
#' patch areas in hectares.
#'
#' @template vtm
#' @template sam
#' @template summaryPolys
#' @template polyCol
#'
#' @param ageClasses character vector of age class labels, youngest to oldest.
#' @param ageClassCutOffs integer vector of lower bounds (years) for each age class.
#'
#' @return `data.table` with columns `group`, `polygonName`, `vegCover`,
#'   `ageClass`, and `sizeInHa`.
#'
#' @keywords internal
.largePatchesByClass <- function(vtm, sam, summaryPolys, polyCol, ageClasses, ageClassCutOffs) {
  v <- terra::rast(vtm)
  s <- terra::rast(sam)

  pixelAreaHa <- prod(terra::res(v)) / 1e4

  spp <- terra::levels(v)[[1]]
  idcol <- which(grepl("id", names(spp), ignore.case = TRUE))

  grp <- basename(dirname(vtm))
  polyNames <- unique(summaryPolys[[polyCol]])

  ageBreaks <- c(ageClassCutOffs, Inf)

  data.table::rbindlist(lapply(polyNames, function(polyName) {
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    vc <- terra::crop(v, subpoly)
    vcm <- terra::mask(vc, subpoly)

    sc <- terra::crop(s, subpoly)
    scm <- terra::mask(sc, subpoly)

    ptchs <- landscapemetrics::get_patches(vcm)[[1]]
    ptchs$class_0 <- NULL ## class 0 = no forested vegetation

    if (length(ptchs) == 0L) {
      return(NULL)
    }

    spp2 <- spp
    spp2$classKey <- paste0("class_", spp2[[idcol]])
    names(ptchs) <- stats::setNames(spp2[["values"]], spp2[["classKey"]])[names(ptchs)]

    data.table::rbindlist(lapply(names(ptchs), function(sppName) {
      patchVals <- terra::values(ptchs[[sppName]], mat = FALSE)
      ageVals <- terra::values(scm, mat = FALSE)

      keep <- !is.na(patchVals)
      if (!any(keep)) {
        return(NULL)
      }

      dt <- data.table::data.table(patch = patchVals[keep], age = ageVals[keep])
      dt <- dt[,
        .(sizeInHa = .N * pixelAreaHa, medianAge = stats::median(age, na.rm = TRUE)),
        by = patch
      ]
      dt[,
        ageClass := as.character(cut(
          medianAge,
          breaks = ageBreaks,
          labels = ageClasses,
          include.lowest = TRUE,
          right = FALSE
        ))
      ]
      dt[, `:=`(
        group = grp,
        polygonName = polyName,
        vegCover = sppName,
        patch = NULL,
        medianAge = NULL
      )]
      dt
    }))
  }))
}

#' Generate histograms for large patches
#'
#' For each reporting polygon in `summaryPolys` and each patch size threshold in
#' `patchSizes`, counts the number of large patches per simulation replicate,
#' per species, and per age class, then writes histograms (PNG) and count
#' tables (CSV) to `dPath`.
#'
#' Groups whose name matches `ccPattern` are treated as current-condition (CC)
#' reference runs and overlaid as a red vertical line on each histogram.
#'
#' @template summaryPolys
#' @template polyCol
#' @template vtm
#' @template sam
#'
#' @param dPath character, destination path for the resulting CSV and PNG files.
#'
#' @param ccPattern character, regex pattern identifying current-condition
#'   replicates within the group names derived from `basename(dirname(vtm))`.
#'   Defaults to `"CC"`.
#'
#' @param patchSizes numeric vector of minimum patch sizes (ha) to use as
#'   thresholds. Defaults to `c(100, 500, 1000, 5000)`.
#'
#' @param ageClasses character vector of age class labels, youngest to oldest.
#'   Defaults to `c("Young", "Immature", "Mature", "Old")`.
#'
#' @param ageClassCutOffs integer vector of lower age bounds (years) for each
#'   class in `ageClasses`. Defaults to `c(0L, 40L, 80L, 120L)`.
#'
#' @return named list (one element per reporting polygon) of character vectors
#'   giving paths to the PNG files written. Invoked primarily for the side
#'   effects of writing CSV and PNG files.
#'
#' @export
runHistsLargePatches <- function(
  summaryPolys,
  polyCol,
  vtm,
  sam,
  dPath,
  ccPattern = "CC",
  patchSizes = c(100, 500, 1000, 5000),
  ageClasses = .ageClasses,
  ageClassCutOffs = .ageClassCutOffs
) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }

  dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
  polyNames <- unique(summaryPolys[[polyCol]])

  allResults <- future.apply::future_mapply(
    .largePatchesByClass,
    vtm = vtm,
    sam = sam,
    MoreArgs = list(
      summaryPolys = summaryPolys,
      polyCol = polyCol,
      ageClasses = ageClasses,
      ageClassCutOffs = ageClassCutOffs
    ),
    SIMPLIFY = FALSE,
    future.packages = c("data.table", "landscapemetrics", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )

  allData <- data.table::rbindlist(allResults, use.names = TRUE)

  lapply(polyNames, function(poly) {
    allData_poly <- allData[polygonName == poly]

    if (nrow(allData_poly) == 0L) {
      return(NULL)
    }

    data <- allData_poly[!grepl(ccPattern, group)]
    dataCC <- allData_poly[grepl(ccPattern, group)]

    data[, rep := as.numeric(factor(group))]

    slices <- c("ageClass", "polygonName", "vegCover", "rep")
    slicesNoRep <- slices[slices != "rep"]

    data <- data[!ageClass == "NA" | !vegCover == "NA"]

    emptyDT <- data.table::data.table(expand.grid(
      ageClass = unique(data$ageClass),
      vegCover = unique(data$vegCover),
      polygonName = unique(data$polygonName),
      rep = unique(data$rep),
      stringsAsFactors = FALSE
    ))

    fout <- lapply(patchSizes, function(minPatchSize) {
      nClustersDT <- data[
        sizeInHa >= minPatchSize,
        c(N = .N, .(totalArea = sum(sizeInHa))),
        by = slices
      ]
      nClustersDT <- nClustersDT[emptyDT, on = slices, nomatch = NA]
      nClustersDT[is.na(N), `:=`(N = 0L, totalArea = 0)]

      nClustersDT_CC <- dataCC[
        sizeInHa >= minPatchSize,
        c(NCC = .N, .(totalAreaCC = sum(sizeInHa))),
        by = slicesNoRep
      ]
      nClustersDT_CC <- nClustersDT_CC[emptyDT, on = slicesNoRep, nomatch = NA]
      nClustersDT_CC[is.na(NCC), `:=`(NCC = 0L, totalAreaCC = 0)]

      nClustersDT <- nClustersDT[nClustersDT_CC, on = slicesNoRep]

      try(utils::write.csv(
        nClustersDT,
        file.path(dPath, paste0("largePatches_", gsub(" ", "_", poly), "_", minPatchSize, ".csv"))
      ))

      saveDir <- file.path(dPath, "largePatches", poly, minPatchSize)
      dir.create(saveDir, recursive = TRUE, showWarnings = FALSE)
      savePng <- quote(file.path(
        saveDir,
        paste0(
          paste(unique(polygonName), unique(vegCover), unique(ageClass), collapse = " "),
          ".png"
        )
      ))

      xlim <- c(0, max(nClustersDT[["N"]], nClustersDT[["NCC"]]))

      nClustersDT[,
        tryCatch(
          .doPlotHistogram(
            data = .SD,
            colName = "N",
            colNameCC = "NCC",
            fname = eval(savePng),
            border = "grey",
            col = "darkgrey",
            main = paste(unique(polygonName), unique(vegCover), unique(ageClass), collapse = " "),
            space = 0,
            xlab = paste0("Number of patches greater than ", minPatchSize, " ha"),
            ylab = "Proportion in NRV",
            xlim = xlim,
            ylim = c(0, 1),
            force.min.n = TRUE
          ),
          error = function(e) warning(e)
        ),
        .SDcols = c(slices, "N", "NCC"),
        by = slicesNoRep
      ]

      nClustersDT[, list(filename = eval(savePng)), by = slicesNoRep]
    })
    unlist(fout)
  })
}

#' Generate histograms for leading vegetation cover
#'
#' For each reporting polygon in `summaryPolys`, species, and age class,
#' computes the distribution of leading-species proportions across simulation
#' replicates and writes histograms (PNG) to `dPath`.
#'
#' Groups whose name matches `ccPattern` are treated as current-condition (CC)
#' reference runs and overlaid as a red vertical line on each histogram.
#'
#' @template summaryPolys
#' @template polyCol
#' @template vtm
#' @template sam
#'
#' @param dPath character, destination path for the resulting PNG files.
#'
#' @param ccPattern character, regex pattern identifying current-condition
#'   replicates within the group names derived from `basename(dirname(vtm))`.
#'   Defaults to `"CC"`.
#'
#' @param pixelRes optional numeric vector of length 2 giving pixel resolution
#'   in metres (e.g., `c(250, 250)`). When supplied, the x-axis label includes
#'   the total forested area in hectares.
#'
#' @param ageClasses character vector of age class labels, youngest to oldest.
#'   Defaults to `c("Young", "Immature", "Mature", "Old")`.
#'
#' @param ageClassCutOffs integer vector of lower age bounds (years) for each
#'   class in `ageClasses`. Defaults to `c(0L, 40L, 80L, 120L)`.
#'
#' @return named list (one element per reporting polygon) of `data.table` objects
#'   with columns `zone`, `vegCover`, `ageClass`, and `filename` giving PNG
#'   output paths. Invoked primarily for the side effect of writing PNG files.
#'
#' @export
runHistsVegCover <- function(
  summaryPolys,
  polyCol,
  vtm,
  sam,
  dPath,
  ccPattern = "CC",
  pixelRes = NULL,
  ageClasses = .ageClasses,
  ageClassCutOffs = .ageClassCutOffs
) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }

  dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
  polyNames <- unique(summaryPolys[[polyCol]])

  allResults <- future.apply::future_mapply(
    .vegCoverByAgeClass,
    vtm = vtm,
    sam = sam,
    MoreArgs = list(
      summaryPolys = summaryPolys,
      polyCol = polyCol,
      ageClasses = ageClasses,
      ageClassCutOffs = ageClassCutOffs
    ),
    SIMPLIFY = FALSE,
    future.packages = c("data.table", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )

  allData <- data.table::rbindlist(allResults, use.names = TRUE)

  lapply(polyNames, function(poly) {
    polyData <- unique(allData[zone == poly])

    if (nrow(polyData) == 0L) {
      return(NULL)
    }

    polyData$ageClass <- factor(polyData$ageClass, ageClasses)

    data <- polyData[!grepl(ccPattern, group)]
    dataCC <- polyData[grepl(ccPattern, group)]

    data[, rep := as.numeric(factor(group))]

    data.table::setnames(dataCC, "proportion", "proportionCC")
    dataCC <- dataCC[, c("group", "NPixels") := list(NULL, NULL)]

    data2 <- dataCC[data, on = .(zone, vegCover, ageClass)]
    data2[is.na(NPixels), NPixels := 0]

    data2[,
      totalPixels := as.double(base::sum(NPixels, na.rm = TRUE)),
      by = c("group", "vegCover", "zone")
    ]
    data2[,
      totalPixels2 := as.double(base::mean(totalPixels, na.rm = TRUE)),
      by = c("vegCover", "zone")
    ]

    saveDir <- file.path(dPath, "leading", poly)
    dir.create(saveDir, recursive = TRUE, showWarnings = FALSE)
    savePng <- quote(file.path(
      saveDir,
      paste0(paste(unique(zone), unique(vegCover), unique(ageClass), collapse = " "), ".png")
    ))
    slices <- c("zone", "vegCover", "ageClass")

    data2[,
      tryCatch(
        .doPlotHistogram(
          data = .SD,
          colName = "proportion",
          colNameCC = "proportionCC",
          fname = eval(savePng),
          border = "grey",
          col = "darkgrey",
          main = paste(unique(zone), unique(vegCover), unique(ageClass), collapse = "_"),
          space = 0,
          xlab = if (!is.null(pixelRes)) {
            paste0(
              "Proportion of forest area (total ",
              format(unique(totalPixels2) * prod(pixelRes) / 1e4, big.mark = ","),
              " ha)"
            )
          } else {
            "Proportion of forest area"
          },
          ylab = "Proportion in NRV",
          xlim = c(0, 1),
          ylim = c(0, 1),
          force.min.n = FALSE
        ),
        error = function(e) warning(e)
      ),
      .SDcols = c(slices, "rep", "proportion", "proportionCC", "totalPixels2"),
      by = slices
    ]
    data2[, list(filename = eval(savePng)), by = slices]
  })
}
