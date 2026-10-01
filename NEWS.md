# pemisc v0.0.5

* `termsInData()` is deprecated: it drops the first token of the model text as the response,
  which is wrong for a transformed response such as `log(age)`. Use `all.vars()` on the model's
  formula instead. LandR, its only known caller, no longer uses it (PredictiveEcology/LandR#253).
* drop support for R < 4.3 due to changes in dependency packages;
* minor bugfix in `factorValues2` for `SpatRaster` class;
* new function `getOrUpdatePkg()`;
* use load balanced `clusterMap()` call in `MapOrDoCall()`;

# pemisc version v0.0.1

* `factorValues2` now works with `terra::SpatRaster` objects;
