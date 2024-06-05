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
