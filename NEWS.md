# pemisc v0.0.5

* `createPrjFile()` writes the `.prj` beside the shapefile it is given. With one path it dropped that path
  and looked for `./.prj` in the working directory, downloading one there if it was missing; with two it
  handled only the first, and with more it stopped. It now takes one or more shapefile paths and returns
  the `.prj` paths invisibly.
* `termsInData()` is deprecated: it drops the first token of the model text as the response,
  which is wrong for a transformed response such as `log(age)`. Use `all.vars()` on the model's
  formula instead. LandR, its only known caller, no longer uses it (PredictiveEcology/LandR#253).
* drop support for R < 4.3 due to changes in dependency packages;
* minor bugfix in `factorValues2` for `SpatRaster` class;
* new function `getOrUpdatePkg()`;
* use load balanced `clusterMap()` call in `MapOrDoCall()`;

# pemisc version v0.0.1

* `factorValues2` now works with `terra::SpatRaster` objects;
