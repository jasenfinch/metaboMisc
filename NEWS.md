# metaboMisc 0.5.5

* Added `gzip_ext` argument to `convertSampleInfo()` to enable the addition of the `.gz` file extension to file names in the `fileNames` column.

# metaboMisc 0.5.4

* Added `exportModelling()` method for the `Analysis` class.

# metaboMisc 0.5.3

* Added generic methods for exporting individual elements of the `Binalysis`, `MetaboProfile`, `Analysis` and `Assignment` classes.

* `export` methods now return vectors of file paths of the exported files.

# metaboMisc 0.5.2

* Added `addAssignments()` method for `Analysis` and `Assignment` classes.

* Added `exportCSV()` that exports data to .csv format and returns the file path of the exported file.

* Added `convertSampleInfo()` to convert sample information returned from `grover::runInfo` to a format compatible with the `binneR` and `profilePro` packages.

# metaboMisc 0.5.1

* Fixed error in `detectPretreatmentParameters()`.

# metaboMisc 0.5.0

* Added `pkgdown` site available [here](https://jasenfinch.github.io/metaboMisc).

* Removed redundant methods and functions: `detectPairwises()`, `addAssignments()`, `filterCorrelations()`, `theme_neat()`.

* Improved documentation and added examples.

* Added unit testing infrastructure.

* Added `detectPretreatmentParameters` and `detectModellingParameters` methods.

* The `magrittr` pipe (`%>%`) is now re-exported.

# metaboMisc 0.4.6

* Fixed parameter handling in `preTreatModes`.

# metaboMisc 0.4.5

* Fixed breakages and function imports caused by [`metabolyseR`](https://jasenfinch.github.io/metabolyseR) to version 0.14.0.

# metaboMisc 0.4.4

* Fixed [`binneR::sampleInfo()`](https://aberhrml.github.io/binneR/reference/results.html) imports with binneR v2.5.0 update.

# metaboMisc 0.4.3

* Fixed missing method exports for `metaboMisc::addAssignments()`, `metaboMisc::detectBatchDiff()`, `metaboMisc::detectMissInjections()`, `metaboMisc::detectPairwises()` and `metaboMisc::reduce()`.

# metaboMisc 0.4.2

* Added `metaboMisc::featureSummary()` to summarise spectral features in the [`Binalysis`](https://aberhrml.github.io/binneR/reference/Binalysis-class.html) S4 class.

# metaboMisc 0.4.1

* Added a `NEWS.md` file to track changes to the package.

* Added `metaboMisc::suitableParallelPlan()` to generate a suitable parallel processing [`future`](https://cran.r-project.org/packages=future) plan.
