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
