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
