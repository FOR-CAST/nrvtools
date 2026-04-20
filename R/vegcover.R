utils::globalVariables(c(
  ".", "group", "NPixels", "proportion", "proportionCC",
  "totalPixels", "totalPixels2", "vegCover", "zone", "ageClass",
  "totalForested"
))

#' Age class names
#' @keywords internal
.ageClasses <- c("Young", "Immature", "Mature", "Old")

#' Age class cut-offs (years)
#' @keywords internal
.ageClassCutOffs <- c(0L, 40L, 80L, 120L)

#' @keywords internal
.doPlotBoxplot <- function(data, fname = NULL, ageClasses, fout = NULL, vegCover, zone, ...) {
  if (!is.null(fname)) grDevices::png(fname, height = 600, width = 800, units = "px")
  a <- graphics::boxplot(proportion ~ as.factor(ageClass), data, ...)

  ## calculate the 12.5% and 87.5% quantiles in addition to the quartiles
  q12.5 <- data |>
    dplyr::group_by(as.factor(ageClass)) |>
    dplyr::summarize(
      quants = quantile(proportion, probs = 0.125)
    ) |>
    purrr::pluck("quants")

  q87.5 <- data |>
    dplyr::group_by(as.factor(ageClass)) |>
    dplyr::summarize(
      quants = quantile(proportion, probs = 0.875)
    ) |>
    purrr::pluck("quants")

  boxplotData <- data.table::data.table(
    zone = rep(zone, 4),
    vegCover = rep(vegCover, 4),
    ageClass = ageClasses,
    proportionCC = data$proportionCC[c(4, 1:3)], ## order by age not alphabet
    MIN = a$stats[1, ],
    q12_5 = q12.5,
    q25_0 = a$stats[2, ],
    MED = a$stats[3, ],
    q75_0 = a$stats[4, ],
    q87_5 = q87.5,
    MAX = a$stats[5, ]
  )

  if (!is.null(fout)) {
    try(utils::write.table(boxplotData,
      file = fout, append = TRUE, col.names = FALSE,
      row.names = FALSE, sep = ","
    ))
  }

  ids <- match(factor(data$ageClass[1:4]), data[1:4, ]$ageClass)
  graphics::points(data$proportionCC[ids], factor(data$ageClass[1:4]), col = "red", pch = 20, cex = 3)

  if (!is.null(fname)) grDevices::dev.off()
}

#' Tabulate leading vegetation cover by age class for one vtm/sam pair
#'
#' For each pixel, assigns the leading species (from `vtm`) and its age class
#' (derived from `sam` using `ageClassCutOffs`), then computes the proportion
#' of total forested pixels in each species × age class combination.
#'
#' @template vtm
#' @template sam
#' @template summaryPolys
#' @template polyCol
#'
#' @param ageClasses character vector of age class labels, youngest to oldest.
#' @param ageClassCutOffs integer vector of lower bounds (years) for each age class.
#'
#' @return `data.table` with columns `group`, `zone`, `vegCover`, `ageClass`,
#'   `NPixels`, and `proportion`.
#'
#' @keywords internal
.vegCoverByAgeClass <- function(vtm, sam, summaryPolys, polyCol, ageClasses, ageClassCutOffs) {
  v <- terra::rast(vtm)
  s <- terra::rast(sam)

  spp <- terra::levels(v)[[1]]
  idcol <- which(grepl("id", names(spp), ignore.case = TRUE))

  grp <- basename(dirname(vtm))
  polyNames <- unique(summaryPolys[[polyCol]])

  data.table::rbindlist(lapply(polyNames, function(polyName) {
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    vc <- terra::crop(v, subpoly)
    vcm <- terra::mask(vc, subpoly)

    sc <- terra::crop(s, subpoly)
    scm <- terra::mask(sc, subpoly)

    vvals <- terra::values(vcm, mat = FALSE)
    svals <- terra::values(scm, mat = FALSE)

    keep <- !is.na(vvals) & vvals != 0 & !is.na(svals)
    vvals <- vvals[keep]
    svals <- svals[keep]

    if (length(vvals) == 0L) return(NULL)

    totalForested <- length(vvals)
    sppNames <- spp[match(vvals, spp[[idcol]]), ][["values"]]

    ageBreaks <- c(ageClassCutOffs, Inf)
    ageClassVals <- as.character(
      cut(svals, breaks = ageBreaks, labels = ageClasses, include.lowest = TRUE, right = FALSE)
    )

    dt <- data.table::data.table(vegCover = sppNames, ageClass = ageClassVals)
    dt <- dt[, .(NPixels = .N), by = .(vegCover, ageClass)]
    dt[, proportion := NPixels / totalForested]
    dt[, zone := polyName]
    dt[, group := grp]

    dt
  }))
}

#' Generate box and whisker plots for leading vegetation cover
#'
#' For each reporting polygon in `summaryPolys`, crops and masks the supplied
#' vegetation type and stand age rasters to that polygon, tabulates leading
#' species by age class across simulation replicates, and writes box-and-whisker
#' plots (PNG) and summary tables (CSV) to `dPath`.
#'
#' Groups whose name matches `ccPattern` are treated as current-condition (CC)
#' reference runs and overlaid as red points on each plot.
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
#'   with columns `zone`, `vegCover`, and `filename` giving PNG output paths.
#'   Invoked primarily for the side effects of writing CSV and PNG files.
#'
#' @export
runBoxplotsVegCover <- function(summaryPolys, polyCol, vtm, sam, dPath,
                                ccPattern = "CC",
                                pixelRes = NULL,
                                ageClasses = .ageClasses,
                                ageClassCutOffs = .ageClassCutOffs) {
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

    if (nrow(polyData) == 0L) return(NULL)

    polyData$ageClass <- factor(polyData$ageClass, ageClasses)

    data <- polyData[!grepl(ccPattern, group)]
    dataCC <- polyData[grepl(ccPattern, group)]
    data.table::setnames(dataCC, "proportion", "proportionCC")
    dataCC <- dataCC[, c("group", "NPixels") := list(NULL, NULL)]

    data2 <- dataCC[data, on = .(zone, vegCover, ageClass)]
    data2[is.na(NPixels), NPixels := 0]

    ## sum per (group, vegCover, zone) = total pixels for that species in that run
    data2[, totalPixels := as.double(base::sum(NPixels, na.rm = TRUE)),
      by = c("group", "vegCover", "zone")
    ]
    ## mean across runs, used for the plot x-axis label
    data2[, totalPixels2 := as.double(base::mean(totalPixels, na.rm = TRUE)),
      by = c("vegCover", "zone")
    ]

    try(utils::write.csv(data2, file.path(dPath, paste0("leading_", gsub(" ", "_", poly), ".csv"))))

    ## write an empty header row first so append-mode rows have column context
    empty <- data.table::data.table(
      zone = character(0), vegCover = character(0),
      ageClass = character(0), proportionCC = numeric(0),
      MIN = numeric(0), q12_5 = numeric(0), q25_0 = numeric(0),
      MED = numeric(0), q75_0 = numeric(0), q87_5 = numeric(0),
      MAX = numeric(0)
    )
    fout <- file.path(dPath, paste0("leading_boxplots_", gsub(" ", "_", poly), ".csv"))
    try(utils::write.csv(empty, fout, row.names = FALSE))

    saveDir <- file.path(dPath, poly)
    dir.create(saveDir, recursive = TRUE, showWarnings = FALSE)
    savePng <- quote(file.path(saveDir, paste0(unique(paste(zone, vegCover, collapse = " ")), ".png")))
    slices <- c("zone", "vegCover")

    data2[, tryCatch(
      .doPlotBoxplot(
        data = .SD,
        col = "limegreen",
        fname = eval(savePng),
        ageClasses = ageClasses,
        fout = fout,
        vegCover = vegCover,
        zone = zone,
        horizontal = TRUE,
        main = unique(paste(zone, vegCover, collapse = "_")),
        xlab = if (!is.null(pixelRes)) {
          paste0(
            "Proportion of forest area (total ",
            format(
              unique(totalPixels2) * prod(pixelRes) / 1e4,
              big.mark = ","
            ),
            " ha)"
          )
        } else {
          "Proportion of forest area"
        },
        ylab = "Age class",
        ylim = c(0, 1)
      ),
      error = function(e) warning(e)
    ),
    .SDcols = c("ageClass", "proportion", "proportionCC", "NPixels", "totalPixels2"),
    by = slices
    ]
    data2[, list(filename = eval(savePng)), by = slices]
  })
}
