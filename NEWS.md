# pemisc v0.0.5

* drop support for R < 4.4, the oldest R version its checks run on (#17);
* minor bugfix in `factorValues2` for `SpatRaster` class;
* new function `getOrUpdatePkg()`;
* use load balanced `clusterMap()` call in `MapOrDoCall()`;

# pemisc version v0.0.1

* `factorValues2` now works with `terra::SpatRaster` objects;
