# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`nrvtools` is an R package providing tools for post-processing and summarizing outputs from Natural Range of Variability (NRV) simulations. It calculates landscape and patch metrics ("X over time" analyses) for use with the `NRV_summary` SpaDES module (<https://github.com/FOR-CAST/NRV_summary>).

## Development Commands

All standard R package development is done via `devtools`:

```r
devtools::load_all()    # Load package for interactive development
devtools::test()        # Run test suite
devtools::check()       # Full R CMD check
devtools::document()    # Re-generate Rd files and NAMESPACE via roxygen2
```

Run a single test file:

```r
testthat::test_file("tests/testthat/test-patches.R")
```

R CMD check uses: `--no-manual --compact-vignettes=gs+qpdf`

## Architecture

### Core Processing Pipelines

**Landscape metrics** (`R/landscape.R`): `calculateLandscapeMetrics()` iterates over vegetation type map (vtm) raster files using `future_lapply`, crops/masks to each summary polygon, and applies `landscapemetrics` functions.

**Patch metrics** (`R/patches.R`): `calculatePatchMetrics()` pairs vtm + stand age map (sam) files via `future_mapply`, calling `patchStats()` → `patchAreas()` + `patchAges()` + `landscapemetrics::lsm_c_*`. Summarization is separate: `summarizePatchMetrics()`.

**Seral stage pipeline** (`R/seral-stages.R`): `seralStageMapGeneratorBC()` generates seral stage maps from cohort data + pixel group maps + NDT/BEC shapefiles. NDT-specific age thresholds define 16 stage classes. `writeSeralStageMapBC()` is a parallel wrapper. `calculatePatchMetricsSeral()` / `summarizePatchMetricsSeral()` handle seral-stage-specific metrics.

**Visualization** (`R/plotting.R`): `plot_over_time()`, `plot_over_time_by_class()`, `plot_by_class()` use ggplot2 + `ggforce::facet_wrap_paginate()` for multi-page polygon output.

**Default metrics** (`R/default-metrics.R`): `default_landscape_metrics()`, `default_patch_metrics()`, `default_patch_metrics_seral()` define which metrics to calculate; all main functions accept a `funList` parameter to override.

**Ecological polygon data** (`R/data.R`): `ecozones`, `ecoprovinces`, `ecoregions`, `ecodistricts` — Canadian sf polygon objects for use as summary reporting regions.

### Key Design Patterns

- **Parallel execution**: Functions use `future.apply::future_lapply()` / `future_mapply()` with `future.packages` and `future.seed = TRUE` passed explicitly to workers.
- **Raster stack**: `terra` for raster operations; `sf` for vectors; `exactextractr` for extraction.
- **Separation of concerns**: Calculation functions (e.g., `calculatePatchMetrics`) are separate from summarization functions (e.g., `summarizePatchMetrics`).
- **Reusable doc templates**: `man-roxygen/` contains `.R` template files (e.g., `sam.R`, `vtm.R`) for shared parameter documentation.

## Dependencies

Key remote (non-CRAN) packages required:

- `PredictiveEcology/map@development`
- `PredictiveEcology/pemisc@development`
- `PredictiveEcology/SpaDES.tools@development`
- `ropensci/NLMR` (a suggested dependency of `SpaDES.tools`)

## Code Style

- 2-space indentation (no tabs)
- Native pipe `|>` (requires R >= 4.1)
- `data.table` for performance-critical data manipulation
- `roxygen2` with markdown support for all documentation
- Spelling: en_CA dictionary

## Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- Avoid `expect_true()` and `expect_false()` in favour of a specific expectation which will give a better failure message. A few expectations in newer releases that you might not know about are `expect_all_true()`, `expect_all_false()`, `expect_all_equal()`, and `expect_r6_class()`.
- Shared test fixtures live in `tests/testthat/helper-fixtures.R` (auto-sourced by testthat). Reuse these helpers rather than redefining them locally: `.write_vtm(dir)`, `.write_sam(dir)`, `.make_poly(vtm_path)`, `.make_two_polys(vtm_path)`, `.setup_reps(td)`, `.create_mock_vtm()`, `.create_mock_sam()`.

## Documentation

- Every user-facing function should be exported and have `roxygen2` documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Always re-document the package after changing a `roxygen2` comment.

## `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.

## Proofreading

- If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing.
- Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading.
- Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.
- Only report what you have changed.