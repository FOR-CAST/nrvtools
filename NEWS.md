# nrvtools 0.3.2

Mirrors corrections made to a sibling implementation of the same CEF section 3.2.2 protocol, which reruns found had invalidated a published analysis.

- `patchAreaStatsSeral()` and `patchSizeClassesSeral()` now exclude patches below `cef_patch_params()$min_patch_ha`, matching the protocol's residual-patch threshold and the threshold arcpy `Eliminate` applies. Without it the reported minimum patch area is whatever the grid's smallest speckle happens to be, which is a property of the cell size rather than of the landscape. This is a reporting filter only: it does not alter the patch map, `patchAreasSeral()`'s raw distribution, or the \pkg{landscapemetrics} class metrics. What it removed is reported alongside as `n_patches_below_floor` and `area_ha_below_floor`, so an excluded patch is visible rather than silently absent.
- `patchAreaStatsSeral()` gains a `params` argument to supply that threshold.

The floor is inert at the 120 m cell size these simulations use, where a single cell is already 1.44 ha; it bites at 30 or 90 m. Measured on a real seral map: 0 patches excluded at 120 m, and 11-31 per seral class once the same map is expressed at 30 m.

## Documented, not changed

`conditionSeralPatchMap()` absorbs sub-threshold patches with `terra::sieve()`, which merges into the **largest neighbouring patch**, where arcpy `Eliminate` merges into the neighbour sharing the **longest border**. The two agree on a binary mask but can differ on the multi-class seral map. The step is inert at 120 m, so the divergence is currently dormant; it is now called out in the documentation rather than left implicit.

## Confirmed not applicable

Several defects corrected in the sibling implementation do not arise here, checked rather than assumed: it used `terra::union()` and `terra::erase()` (the latter hitting terra #2175), where the vector backend here uses `sf::st_union()` / `sf::st_difference()`; and its interior-forest target was not restricted to the mature/old classes, where the targets here are built from explicit class codes. A regression test now pins the latter.

# nrvtools 0.3.1

- `interiorForestSeral()` gains a `method` argument. The new default, `"subgrid"`, refines the map by `subgrid_factor` and thresholds a distance transform per edge-influence band instead of buffering and erasing polygons. It is linear in cells where the polygon route is superlinear in geometry complexity, which is what makes a district-sized landscape feasible: on a 7.2M cell map it takes about 3 minutes per snapshot, where `method = "vector"` did not finish a single snapshot in an hour. `"vector"` remains available and is still the exact answer.
- Measured against `"vector"` on a real 120 m seral map at `subgrid_factor = 4`, interior area agrees to +1.0% (mature+old) and +0.3% (old). The bias is slightly high: `terra::distance()` reports the distance to the nearest cell *centre* of a band whereas the buffer is measured from its edge, corrected here by half a sub-cell, which marginally overestimates the distance to a diagonally-placed stand.
- `subgrid_factor = 4` is a floor, not a preference. At `2` the 25 m mature band is narrower than a sub-cell and disappears entirely, reporting old interior forest as 85% of old extent on a district-sized map against 64% at `4`. Peak memory reached roughly 13 GB at that size, so concurrent workers need sizing accordingly.

## Bug fix

- The sub-grid backend built its target masks with `%in%`, which is not an S4 group generic in \pkg{terra}. With terra imported rather than attached this fell through to `base::%in%` and returned a plain logical vector instead of a `SpatRaster`, erroring in the next `terra::ifel()` call. Target masks are now built with `==`, which dispatches either way.

# nrvtools 0.3.0

## Seral patch metrics now follow the CEF forest-biodiversity protocol

Seral-stage patches were previously whatever `landscapemetrics::get_patches()` returned: an eight-neighbour connected component of same-class cells, with no minimum size, no edge-influence erosion and no size classes. They now follow the landscape-patch definition of the Interim Assessment Protocol for Forest Biodiversity in British Columbia (Cumulative Effects Framework), section 3.2.2.

- `cef_patch_params()` (new, exported) holds the protocol's patch parameters -- edge-influence buffer distances, minimum patch area, patch separation distance, and patch size classes -- as one list, so that raster and vector implementations of the protocol read the same numbers from a single place.
- `conditionSeralPatchMap()` (new, exported) applies the map-level steps of the definition (bridging same-class stands closer together than the separation distance, absorbing sub-threshold residual patches) so that downstream patch metrics are computed on a map whose connected components *are* protocol patches. Both steps are resolution-dependent and either may be inexpressible at coarse cell sizes; the realised thresholds are recorded on the result as the `"cef_realised"` attribute and reported via `message()`, rather than being silently applied at the wrong scale.
- `interiorForestSeral()` (new, exported) computes interior forest area in vector space, on dissolved class geometries only. This is deliberate: edge influence reaches 25-200 m into a patch, which is below the cell width for all but the widest band at a 120 m cell size, so a grid-based erosion returns interior forest indistinguishable from total mature+old area. Results are returned as areas and proportions per subregion, never rasterized back.
- `patchSizeClassesSeral()` (new, exported) reports patch counts and total area per seral class per protocol size class (0-40 / 41-80 / 81-250 / >250 ha). A landscape can hold a constant total area of old forest while that area migrates from a few large patches into many small ones; the class-total metrics alone do not show this.
- `patchAreaStatsSeral()` (new, exported) adds minimum, median and maximum patch area per class, complementing the existing `lsm_c_area_mn` / `_sd` / `_cv` moments.
- `default_patch_metrics_seral()` gains `patchAreaStatsSeral`, `patchSizeClassesSeral`, and the Euclidean nearest-neighbour (interpatch distance) metrics `lsm_p_enn`, `lsm_c_enn_mn`, `lsm_c_enn_cv`, `lsm_c_enn_sd`.

### Two behaviours worth knowing about

- `lsm_*_enn_*` report the distance between the nearest *cell centres* of two patches, which is **one cell width larger than the gap between them**. Subtract the cell size before comparing an interpatch distance against one measured edge-to-edge on polygons.
- The mature+old interior-forest target erases only the early and mid edge-influence bands, not the 25 m mature band. The 25 m band is the edge influence of *mature* stands, which are themselves part of a mature+old patch; erasing it collapses mature+old interior forest onto old-only interior forest, making the two targets numerically identical. `cef_patch_params()$interior_bands` encodes this, and it is covered by a regression test.

# nrvtools 0.2.11

* **fix:** `calculateLandWebMetrics()` mis-labelled its results when a reporting layer mixed polygon names with different numbers of `_`-separated tokens. It split names on `"_"` and `purrr::transpose()`d them, which requires every element to be the same length; a layer holding both `ANC` and `DawsonCreek_TSA` therefore transposed to the wrong shape and recombined into the **cartesian product** of the tokens -- for LandWeb's WesternAlbertaUpland tenure layer that produced 45 fabricated names (`ANC_Edson`, `Canfor_GrandePr`, `Sundre_Hinton`, ...) in place of the 11 real ones, with `ANC`, `BlueRidge`, `Canfor`, `CanforWhitecourt`, `MillarWestern` and `Sundre` dropped entirely. It now uses the shared `_year<YYYY>_` marker parser (`.parse_metric_labels()`) introduced in 0.2.10 for `patchStats()` / `patchStatsSeral()` / `nrv_metrics_landscape()`, which this function was missed from. The labels were wrong but non-blank and the run completed normally, so `lw` aggregates produced by earlier versions should be regenerated;

# nrvtools 0.2.10

- `label_rat_classes()` (new, exported) generalises `label_vegtype_classes()` to any categorical map's RAT; `label_vegtype_classes()` is retained as the vegetation-type spelling of the same operation.
- `nrv_metrics_landscape()` no longer mis-assigns `rep`/`time`/`poly` when a reporting subregion is empty. The 0.2.8 empty-subregion guard makes such a subregion contribute zero rows, but the identifiers were stamped onto the already row-bound table from full-length vectors, shifting every subsequent row's identifiers by one subregion; they are now stamped per subregion before binding (as on the patch-metric path).
- `patchStatsSeral()` now relabels class-level `lsm_c_*` integer class codes with their seral stage names via the seral-stage map's RAT, matching the vegetation-type relabelling `patchStats()` has done since 0.2.5. Class-level seral metrics previously reported raw raster codes (`1`, `5`, `9`, `13`) rather than `early`/`mid`/`mature`/`old`.
- `plot_nrv_envelope()` and `plot_nrv_distribution()` order panels by the faceting columns' own ordering rather than alphabetically by the pasted panel label, so a factor facet column (e.g. `factor(class, levels = seral_stages())`) panels in its declared order. Both also gain `rptPoly` as the first default faceting column.
- `summarize_nrv()` auto-detects `rptPoly` as an identifier column, naming the reporting polygon LAYER a metric was summarised over (where `poly` is the subregion within that layer), so metrics computed over several reporting layers aggregate without collapsing across them.
- Reporting polygon names containing more than one underscore no longer abort `calculatePatchMetrics()`, `calculatePatchMetricsSeral()`, and `nrv_metrics_landscape()` with "polyName contains too many underscores"; names are now parsed as everything following the `_year<YYYY>_` marker, so names containing `_` or `.` (BEC subzones, NDT-BEC codes, landscape unit names) all parse correctly.

# nrvtools 0.2.9

- `funList` entries passed to `nrv_metrics_landscape()`, `patchStats()`, `patchStatsSeral()`, and `calculateLandWebMetrics()` may now be explicit `pkg::fun` strings (e.g. `"landscapemetrics::lsm_l_ta"`) in addition to bare function names, resolved from the named package's namespace so a caller can disambiguate or reach a function not otherwise on the search path (#1).

# nrvtools 0.2.8

- `patchStatsSeral()`, `patchAreasSeral()`, and `nrv_metrics_landscape()` now guard an empty / no-forest reporting subregion (an all-`NA` crop after crop/mask) instead of erroring in `landscapemetrics` (`get_patches()` / `lsm_*` -> "attempt to select less than one element"), returning an empty table per metric so the subregion still appears with no rows in the assembled output. This extends the 0.2.2/0.2.3 empty-subregion guards, previously only on the non-seral `patchStats()`/`patchAreas()`/`patchAges()` path, to the seral and landscape-metric paths. Also fixes a `terra::mask()` partial-argument match (`maskvalue` -> `maskvalues`) in `patchStats()`/`patchStatsSeral()`.

# nrvtools 0.2.7

- `subregion_forested_area()` (new, exported) tabulates the forested area (ha) of each leading-vegetation species within each subregion of a reporting layer from a categorical vegetation-type map, with an "All species" total per subregion (the v2 / NW_AB comparative-boxplot area calculation).
- `plot_leading_boxplot()` gains a `caption` argument, rendered bottom-right below the figure (e.g. the subregion's total forested area).

# nrvtools 0.2.6

- `plot_nrv_envelope()` draws a single plain panel (no placeholder "all" facet strip) when none of the candidate faceting columns vary, e.g. a per-subregion landscape-metric envelope that has no class dimension.

# nrvtools 0.2.5

- `label_vegtype_classes()` (new, exported) relabels integer vegetation-type class codes in a `class` column with their species labels from a categorical VTM's RAT, leaving already-labelled classes and non-matches unchanged (idempotent). `patchStats()` now applies it so class-level `lsm_c_*` patch metrics report species names instead of raw integer codes, matching `patchAges()` / `patchAreas()`.

# nrvtools 0.2.4

- `plot_nrv_envelope()` gains a `title` argument and optional pagination (`page`, `ncol`, `nrow`): with `page = NULL` it draws every panel in one figure as before, while an integer `page` splits the panels across pages via `ggforce::facet_wrap_paginate()` (use `ggforce::n_pages()` on the `page = 1` result to get the page count). This lets a large panel set be written as several PNGs instead of one crammed figure.

# nrvtools 0.2.3

- `patchStats()` skips a reporting subregion with no forested pixels (an all-`NA` crop) instead of erroring, guarding the whole metric set at once (`landscapemetrics::get_patches()`/`lsm_*` fail on an all-`NA` raster). The per-metric RAT guards added in 0.2.2 did not catch this because the category table persists even when all cells are `NA`.

# nrvtools 0.2.2

- `patchAges()`, `patchAreas()`, and `.cat_labels()` (used by `leadingVegByAgeClass()` /
  `largePatchCounts()`) now return an empty / all-`NA` result for a tiny or empty reporting subregion
  (one with no forested pixels, whose cropped vegetation-type map has no category table) instead of
  erroring with "attempt to select less than one element". This surfaced once reporting-polygon
  subregions stopped being collapsed together. `patchAreas()` also now reads the RAT label from its
  positional (non-value) column rather than a hard-coded "values" name.

# nrvtools 0.2.1

- `plot_leading_boxplot()` and `plot_largepatch_histogram()`: the v2-form LandWeb-summary plots for a single reporting unit. `plot_leading_boxplot()` draws horizontal green age-class box-and-whiskers of the across-replicate proportion of forest area, with the current condition as a red dot (replacing the earlier busy distribution histograms for the Leading analysis). `plot_largepatch_histogram()` draws one file per species with four age-class panels (young to old), each the across-replicate "Proportion in NRV" of the patch count, with a red current-condition vertical line.

# nrvtools 0.2.0

- `calculateLandWebMetrics()`, `leadingVegByAgeClass()`, `largePatchCounts()`, `default_landweb_metrics()`: port the v2 `LandWeb_summary` "Leading vegetation type by age class" and "LargePatches" analyses onto the Arrow-native raw-producer contract. `leadingVegByAgeClass()` computes the proportion of each vegetation class in each age class (plus an "All species" roll-up); `largePatchCounts()` bins age, delineates contiguous (age class x vegetation class) patches with `landscapemetrics::get_patches()` (default 4-connectivity, matching the v2 GDAL polygonize) and counts patches at/above each size threshold; `calculateLandWebMetrics()` is the per-replicate wrapper (crops to each reporting polygon, no flammable masking). These metrics pool the NRV distribution across replicates and summary years, so summarize with `time` excluded from the id columns.
- `plot_nrv_distribution()`: plot the across-replicate distribution of a metric (pooled over the summary period) as a histogram with an optional current-condition reference line, reproducing the v2 LandWeb_summary histograms. Unlike `plot_nrv_envelope()`, this reads the raw per-replicate parquet (not the collapsed envelope) and is not a time series.

# nrvtools 0.1.3

- `patchAges()`: fix the per-patch median stand-age summary. `sam[ids]` returns a one-column data.frame, so the summary column was not `lyr.1` (`dplyr::summarise(median(lyr.1))` errored with `object 'lyr.1' not found`). Extract the values vector (`sam[ids][[1L]]`) and summarise `median(sam)`. Together with the 0.1.2 RAT fix, `patchAges()` now runs end-to-end.

# nrvtools 0.1.2

- `patchAges()`: read the terra category table (RAT) by column position, not hard-coded `ID`/`values` names. LandR writes the RAT columns lowercase (`id`/`values`), so the previous `spp[["ID"]]` was NULL and every patch was renamed to `NA`, causing `ptchs[[NA]]` -> `terra::values(NULL)` in the patch-age summary. Now works for both LandR (`id`) and LANDIS-derived (`ID`) vegetation-type maps.

# nrvtools 0.1.1

- Drop the `pemisc` dependency (its `optimalClusterNum` was imported but never used) and the `map` dependency (only reached by data-gated seral-stage integration tests that read `map`/`simList` objects). This removes the unresolvable upstream transitive dependencies `PredictiveEcology/Require@development` (via `pemisc`) and `ropensci/tiler` (via `map`) from the install graph, which had been breaking dependency resolution on CI. The `map`-dependent seral integration tests are removed; `patchAreasSeral()` remains covered by `test-patches.R`.

# nrvtools 0.1.0

This is a breaking release: the replicated-metric summary path is now Arrow-native and memory-bounded, and the former in-memory summarising functions are removed. Consumers (e.g. the `FOR-CAST/NRV_summary` module) must be updated (see the "Breaking changes" below).

## Breaking changes

- `calculateLandscapeMetrics()` is removed and replaced by `nrv_metrics_landscape()`, which returns the raw per-replicate landscape metrics (one row per replicate x time x polygon x metric, sharing the `level`/`class`/`metric`/`value`/`rep`/`time`/`poly` schema of the patch producers) instead of an across-replicate summary; feed it through `summarize_nrv()` to build the range-of-variation envelope. The rename is deliberate so callers of the old summarising behaviour fail loudly rather than silently mis-aggregate.
- `summarizePatchMetrics()` and `summarizePatchMetricsSeral()` are removed; the across-replicate reduction is now done once, memory-bounded, by `summarize_nrv()` (order seral classes for display with `factor(class, levels = seral_stages())`).
- `plot_over_time()`, `plot_over_time_by_class()`, and `plot_by_class()` now read the `summarize_nrv()` envelope columns (`mean`/`sd`) instead of the former `mn`/`sd`.

## New features

- `summarize_nrv()`, `open_nrv_dataset()`, and `write_nrv_parquet()` add an Arrow-native path for range-of-variation summaries: each replicate's metrics are written to a partitioned parquet (`write_nrv_parquet()`, published atomically so concurrent writers on an NFS mount never collide) and the across-replicate envelope is computed by pushing the reduction down to Arrow compute (`summarize_nrv()`), so replicate rows are never all held in memory at once.
- `summarize_nrv()` returns the full five-number summary (`min`, `q25`, `median`, `q75`, `max`, all Arrow-approximate for the quantiles) in addition to `mean`/`sd`/`n_reps`/`se`/`ci`, so both ribbon and box-and-whisker range-of-variation plots can be drawn from the same envelope.
- `tidy_nrv_metrics()` row-binds a raw metric list (from `nrv_metrics_landscape()` / `calculatePatchMetrics()` / `calculatePatchMetricsSeral()`) into one long table ready for `write_nrv_parquet()`, optionally stamping `studyArea`/`scenario`.
- `seral_stages()` returns the ordered BC seral-stage class labels for use as factor levels.
- `plot_nrv_envelope()` plots a `summarize_nrv()` envelope, faceting by whichever categorical columns vary so replicate envelopes never overlay within a panel, in one of two styles via `type`: `"ribbon"` (mean line + min-max ribbon) or `"boxplot"` (box-and-whisker showing the median and quartiles the ribbon hides).
- add a "Memory-bounded NRV summaries with nrvtools" usage vignette walking through the raw-metrics -> parquet -> `summarize_nrv()` -> `plot_nrv_envelope()` workflow.
- `patchAreasSeral()` (and `patchAreas()`) now resolve the raster-attribute-table cell-value column via the new internal `.rat_value_col()` helper (match `ID` or `value`, else fall back to the first column) instead of assuming a column name containing `"id"`; the seral-stage map from `seralStageMapGeneratorBC()` names that column `value`, so the previous lookup returned `integer(0)` and `patchAreasSeral()` crashed with a `.subset2` "select less than one element" error.
- make explicit the dependency on R >= 4.1 due to use of native pipe (`|>`);
- remove dependency package `raster` (#2);
- remove defunct package `qs`, use `qs2` instead;
- add `exactextractr` and `tidyterra` to Imports;
- move `SpaDES.tools` to Suggests;
- rework analyses to use `terra`, which is generally faster and lower memory than `sf`;
- add `ecozones`, `ecoprovinces`, `ecoregions`, and `ecodistricts` polygon data for use as reporting polygons;

# nrvtools 0.0.21

- fixed issue with seral stage map generator;

# nrvtools 0.0.20

- fixed minor issue with seral stage summary calculations;

# nrvtools 0.0.19

- fixed issue with factor levels in seral stage summary calculations;

# nrvtools 0.0.18

- improved seral stage calculations;

# nrvtools 0.0.17

- improve handling of IDT4 seral stages in `seralStageMapGeneratorBC()`;

# nrvtools 0.0.16

- split summary table creation from `calculatePatchMetrics()` and `calculatePatchMetricsSeral()` to allow for saving of the full data.frame;
- new functions `summarizePatchMetrics()` and `summarizePatchMetricsSeral()` to create summary tables;
- new function `writeSeralStageMapBC()` wraps `seralStageMapGeneratorBC()` in `future.apply::future_mapply()` for processing in parallel;
- improved testing;

# nrvtools 0.0.15

- `group_by()` and `summarize()` steps take place on the entire data.frame to ensure stats for reporting polygons with multiple subpolygons of same name are correctly calculated;

# nrvtools 0.0.14

- fix mask bug in `patchStats()` and `patchStatsSeral()`;

# nrvtools 0.0.13

- more factor level fixes for seral stages;

# nrvtools 0.0.12

- ensure consistent factor levels for seral stages;
- ensure consistent level ids used for matching;

# nrvtools 0.0.11

- add quartiles (including median) to all summary data.frames;
- rename `plot_by_species()` to `plot_by_class()` to reflect that it can plot by any class (e.g., seral stage);
- new function `plot_over_time_by_class()` which is analogous to `plot_over_time()` but plots all classes on same panel;
- use `future.globals = FALSE` as we are already passing the vars needed to the fun;
- updated seral stage tests;

# nrvtools 0.0.10

- pass `funList` to `calculatePatchMetricsSeral()` to allow the user to pass their own list of function names;

# nrvtools 0.0.9

- remove direct dependency on `future` package;
- no longer call `plan()` nor `tweak()` - to allow user to set their `future` plan;
- pass `ndtbec` as filename instead of `sf` polygons object to `seralStageMapGeneratorBC()` to avoid serializing this object in `future.apply` calls;
- improved documentation;

# nrvtools 0.0.8

- updated default metrics to include aggregation indices and additional area metrics (see `?default_metrics`);

# nrvtools 0.0.7

- pass `future.seed = TRUE` to `future.apply` calls to avoid 'UNRELIABLE VALUE' from RNG;

# nrvtools 0.0.6

- `calculateLandscapeMetrics()` and `calculatePatchMetrics()` gain argument `funList` to to allow the user to pass their own list of function names. If not specified, defaults to `default_landscape_metrics()` and `default_patch_metrics()`, respectively.

# nrvtools 0.0.5

- fix and speedup pixel group reclassification problem in `seralStageMapGeneratorBC()`;
- test `seralStageMapGeneratorBC()` on additional simulated landscapes;
- fixed `@seealso` typos;

# nrvtools 0.0.4

- fix use of `tweak()` prior to parallel calculations;
- fix issue with `patchStatsSeral()` where raster filename was being passed instead of the raster object;
- ensure `nrvtools` gets loaded in `future.apply` workers;

# nrvtools 0.0.3

- simplify seral stage classifications to put all non-fir into pine group for NDT4;

# nrvtools 0.0.2

- add `calculatePatchMetrics()` and `calculatePatchMetricsSeral()`;

# nrvtools 0.0.1

- initial version;
- move functions and utilities from `FOR-CAST/NRV_summary` module here to facilitate updates and testing;
