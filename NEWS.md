# metaboMisc 0.5.0

* Added `pkgdown` site available [here](https://jasenfinch.github.io/metaboMisc).

* Removed redundant methods and functions: `detectPairwises()`, `addAssignments()`, `filterCorrelations()`, `theme_neat()`.

* Improved documentation and added examplesl

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

* Added `metaboMisc::suitableParallelPlan()` to generate a suitable parallel processing [`future`](https://cran.r-project.org/web/packages/future/index.html) plan.
