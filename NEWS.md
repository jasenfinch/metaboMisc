# metaboMisc 0.6.0

* `injOrder` is now the default column for sample indexes returned from `detectMissInjections()`.

* The returned miss injection indexes are now sorted.

* Fixed the `MetaboProfile` S4 class `export()` method for GC-MS techniques.

* Added the `strategy` and `workers` arguments in `suitableParallelPlan()` to allow the manual selection of parallel options.

* Added an `export()` method for the [`Construction`](https://jasenfinch.github.io/construction/) S4 class.

* `detectModellingParameters()` now returns parameters for unsupervised random forest when a single class response is specified.

* Added `miss_injections` and `batch_correction` arguments to `detectPretreatmentParameters()` to specify if miss injection detection and/or batch correction detection is performed.

* The `RSDthresh` parameter is now set by `detectPretreatmentParameters()` for the `MetaboProfile` S4 class depending on the technique used.

* Miss injections are now detected below a percentage threshold of the median total ion count in `detectMissInjections()`.

* Added the `isotopic_adducts` argument to `reduce()` to allow the removal of *m/z* features assigned isotopic adducts.

* The row order of the output of `convertSampleInfo()` will now always match the row order of the input.

* Added the `user_text` argument to `convertSampleInfo()` to denote the column names for the `User text` fields.

# metaboMisc 0.5.11

* `exportCSV()` now recursively creates the destination directory if it does not already exist.

# metaboMisc 0.5.10

* `detectPretreatmentParameters()` now fully matches the `QCidx` argument against the supplied class information when detecting the presence of QC samples.

# metaboMisc 0.5.9

* Corrected the exported file name for mode-less processed data for the `MetaboProfile` class.

* Fixed the character sanitation in `sanitiseTable()`.

* Fixed extraneous console messages in `detectMissInjections()` and `detectBatchDiff()` methods for the `MetaboProfile` class.

* Enabled unit tests for methods for the `MetaboProfile` class.

* `detectPretreatmentParameters()` now detects the presence of QC samples.

# metaboMisc 0.5.8

* Numbers of characters in strings are now limited by `sanitiseTable()`.

# metaboMisc 0.5.7

* Removed `aberHRML/metaboData` from the Remotes field in the DESCRIPTION to ensure that the CRAN version  of metaboData is installed.

* Added `sanitiseTable()` that can be used to restrict the number of  rows in a table and round numeric columns to a given number of significant figures. 

# metaboMisc 0.5.6

* Fixed missing argument error in `detectPretreatmentParameters()` when miss injections are detected.

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
