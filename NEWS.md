# TestDesign 1.3.3.9000

## Bug fixes

* Fixed where `Shadow()` was not working when `data` was supplied with `true_theta` left empty. (#118)
* Fixed where `BIGM` exposure control was ignoring the `M` value and defaulting to (maximum information + 1) even when it was supplied.
* Fixed where `Shadow()` was not working when the `FIXED` item selection method was used with a single `$fixed_theta` value in the config. (#132)
* Fixed where if interim theta estimation method was `EB` or `FB`, prior sample generation was ignoring prior distribution type and assuming normal distribution.
* Fixed where if prior distribution type was uniform, prior density generation was ignoring prior parameters and using U(0, 1).
* Fixed where EAP shrinkage correction was ignoring prior standard deviation when it was supplied in the config.

## Updates

* The `FIXED` item selection method for `Shadow()` now does not approximate the supplied value to the nearest theta quadrature when computing information.
* `config_Shadow@interim_theta$prior_dist` and `config_Shadow@final_theta$prior_dist` now accept `RAW`. This is introduced to accommodate supplying raw densities to the `prior` argument of `Shadow()`.

# TestDesign 1.3.3

## Updates

* Streamlined error handling in Shiny app `TestDesign()`.
* `config_Static` and `config_Shadow` and are now exported classes.

# TestDesign 1.3.2

## New features

* `config_Shadow` and `createShadowTestConfig()` gain a new slot `exclude_policy` for configuring how excluded items are treated. Excluded items are supplied through the `exclude` argument in `Shadow()`.

## Updates

* Shiny app `TestDesign()` gains a 'close app' button.

## Bug fixes

* Fixed where the Shiny app was not starting normally.
* Fixed where attempting to use an exposure acceleration factor greater than 1 in set-based adaptive assembly using `Shadow()` would trigger a 'incorrect number of subscripts' error.

# TestDesign 1.3.1

## Bug fixes

* Fixed where attempting to `loadConstraints()` that vary the number of stimuli would incorrectly fail validation.

# TestDesign 1.3.0

## New features

* Added `Split()` for partitioning an item pool into parallel tests/sub-pools that satisfy all constraints. This is an experimental method we are currently working on. See `vignette('split')` for a quick introduction.

## Bug fixes

* Fixed where alpha adjustment for exposure control was being applied excessively.

# TestDesign 1.2.7

## Updates

* `plot()` now works better with 'knitr' markdown documents. This is enabled by removing the use of `recordPlot()` inside `plot()`. As a result, `p <- plot()` will not work anymore. Use `recordPlot()` for assigning plots into objects.
* Shiny app `TestDesign()` is updated to work with the above change.
* `plot()` gains a new `use_par` argument to override using default graphical parameters that are hard-coded into the function. This allows for more flexible formatting.
* `theta_segment` argument in `plot(type = "exposure")` is renamed to `theta_type`. The existing argument is deprecated and using it will raise a warning.
* Added validation check for test length constraints. In shadow test approach, test length is expected to be a fixed value.

# TestDesign 1.2.6

## Updates

* `Shadow()` gains a new argument `seed`.
* `plot(type = "exposure")` gains new arguments `segment`, `rmse`, and `use_segment_label`.
* `summary()` for `output_Shadow_all` objects now also summarizes the number of selected items for `ENEMY`, `INCLUDE`, `EXCLUDE`, and `ALLORNONE` constraints.
* Updated the citation in `citation("TestDesign")`

## Bug fixes

* Fixed where `summary()` of an `output_Shadow_all` object used to throw an `incorrect number of dimensions` error when there was only one constraint.
* Fixed where setting `diagnostic_stats = TRUE` option in `createShadowTestConfig()` would generate unnecessarily large amount of data when running `Shadow()`.

# TestDesign 1.2.5

## Updates
* Streamlined `eap()` to handle input formats consistently with `mle()`. This should only affect use cases where `eap()` is called directly.
* `Shadow()` now utilizes `include_items_for_estimation` for EAP estimation.

## Deprecated functions
* Deprecated `plotInfo()` is removed. This is now `plot(type = "info")`.
* Deprecated `plotExposure()` is removed. This is now `plot(type = "exposure")`.

# TestDesign 1.2.4

## Updates
* `output_Shadow` gains a new slot for initial theta estimates. `plot()` utilizes the new slot.
* `TestDesign()` now checks whether each solver works, and only shows solvers available on the user system.

## Bug fixes
* Fixed where graphic parameters were not being reset when using `plot()`.
* `toggleConstraints()` now also accepts character IDs.

# TestDesign 1.2.3

## Updates
* Internal constraint indices are now suppressed in printing. This affects `summary()` for `output_Shadow_all` and `output_Static` objects, and `print()` for `constraints` objects. User-supplied constraint indices are still printed as before.
* `NA` values are now suppressed in printing, in `summary()` of `output_Shadow_all` and `output_Static` objects.

## Bug fixes
* Fixed where subsetting `constraints` objects raised a warning related to internal constraint indices.


# TestDesign 1.2.2

* Fixed to pass automated checks on CRAN.

# TestDesign 1.2.0

## New features
* Added goal programming method for adaptive assembly.
* `Shadow()` has a new argument `include_items_for_estimation` for including existing response data to be used to supplement interim theta estimation.
* The argument `exclude_items` in `Shadow()` is now renamed to `exclude`. The argument `exclude` now also accepts stimulus IDs.
* MLE estimator has new arguments `use_step_size` and `step_size` for limiting the amount of change between item administrations.

## Changes
* MLEF item augmentation is now only performed for extreme responses.

## QoL updates
* `loadConstraints()` now retains stimulus IDs in the `item_index_by_stimulus` slot for easier access.
* `loadConstraints()` now accepts non-numeric character IDs in the `CONSTRAINT_ID` column.
* `plot()` now accepts `ylim`.

## Bug fixes
* Fixed the error message associated with validating a `config_Shadow` object for whether its solver was installed on the system.

## Deprecated functions
* Deprecated `updateConstraints()` is removed. This is now `toggleConstraints()`.
* Deprecated `showConstraints()` is removed. This is now `print()`.
* Deprecated `plotCAT()` is removed. This is now `plot(type = 'audit')`.
* Deprecated `plotShadow()` is removed. This is now `plot(type = 'shadow')`.

# TestDesign 1.1.3

## New features

* Now supports MLE with Fences ([Han, 2016](https://doi.org/10.1177/0146621616631317)) for interim and final theta estimates in `Shadow()`. This can be configured using `config_Shadow@interim_theta` and `config_Shadow@final_theta`.

## Updates

* Added missing documentation entries for the `output_Shadow_all` class.
* `output_Shadow_all` objects now have `print()` and `show()` extensions.
* Removed a deprecated class.

# TestDesign 1.1.2

## QoL updates

* Web documentation is now available at [https://choi-phd.github.io/TestDesign/](https://choi-phd.github.io/TestDesign/)
* `loadItemPool()`, `loadItemAttrib()`, `loadStAttrib()`, `loadConstraints()`, `buildConstraints()` are now capable of reading from data frame objects.
* `loadItemPool()` is now capable of reading from `SingleGroupClass` objects from `mirt` package.
* `item_pool` objects can be now subsetted and combined with `[` and `c()`.
* `constraints` objects can be now subsetted and combined with `[` and `c()`.
* `SUM` constraints now accept `VARIABLE, EXPRESSION` in `CONDITION` for imposing constraints on conditional sums (e.g. `WORDCOUNT, DOK == 1`).
* `Static()` and `Shadow()` now asks for confirmation when attempting to use solvers other than `Rsymphony`, `lpsymphony` and `gurobi` for set-based assembly. This can be overridden by passing `force_solver = TRUE`.
* `Shadow()` now uses `progress` package if available.
* `Static()` now returns an `output_Static` object.
* `Shadow()` now returns an `output_Shadow_all` object.
* Added `print()` extensions for most objects.
* Added `summary()` extensions for most objects.
* Added `plot()` extensions for most objects.
* Added `dataset_bayes` example dataset.
* Now checks whether each solver returns solution properly upon loading the package.
* `plot(type = 'shadow')` (formerly `plotShadow()`) now groups items by stimulus.

## New features
* `config_*` objects now have a new `MIP$retry` slot. This specifies the number of retry attempts when the solver encounters a 'no solution' error. This error is incorrect in some cases. The intent of retrying is to verify whether the 'no solution' error indeed indicates a true error.
* `Shadow()` now has a new `excluded_items` argument.

## Deprecated functions

* `updateConstraints()` is now `toggleConstraints()`.
* `plotInfo()` is now `plot(type = 'info')`.
* `plotCAT()` is now `plot(type = 'audit')`.
* `plotShadow()` is now `plot(type = 'shadow')`.
* `plotExposure()` is now `plot(type = 'exposure')`.
* `calcDerivative()` is removed.
* `calcDerivative2()` is removed.

## Bug fixes
* Fixed a rare bug where `Shadow()` was marking eligible items as ineligible when using `ELIGIBILITY` exposure control.
* Fixed where `loadItemPool()` was parsing standard errors incorrectly for `GR` models.
* Fixed where `loadConstraints()` was creating unnecessarily large numbers of constraints when `TYPE = 'NUMBER'` and `CONDITION` was an item/stimulus attribute name, and the attribute did not have NA values.
* Fixed where `loadConstraints()` was creating constraints incorrectly when `TYPE = 'NUMBER'` and `CONDITION` was an item/stimulus attribute name, and the attribute had NA values.
* Fixed where `loadConstraints()` was creating constraints incorrectly when `TYPE = 'NUMBER'` and `CONDITION` was a stimulus attribute name, and `LB` and `UB` were not equal.
* Fixed where `loadConstraints()` was creating constraints incorrectly when `TYPE = 'SUM'`.

# TestDesign 1.0.2

## Default solver

* Reverted the default solver to `lpSolve` to address `lpsymphony` being unavailable on Solaris.

## Bug fixes

* Fixed where using diagnostic stats was preventing `Shadow()` to run.
* Fixed an error affecting `BIGM` exposure control method on set-based items.

# TestDesign 1.0.1

## Default solver

* `lpsymphony` is now the default solver. `lpsymphony` allows faster solving of set-based assembly tasks, and is easily installable on various platforms.

## New helper functions

* `getSolution()` prints the indices of the selected items from the results of `Static()` or `Shadow()`.
* `showConstraints()` returns the constraints table from a `constraints` object. This is a shortcut to access the `@constraints` slot.

## Bug fixes

* Now recognizes `TM_TARGET_GAP_ACHIEVED` as a valid status message in `lpsymphony` and `Rsymphony` solvers.
* `loadItemAttrib()` now ignores malformatted values in 'INDEX' column and regenerates correct indices.
* `loadStAttrib()` now ignores malformatted values in 'STINDEX' column and regenerates correct indices.
* `config_Static` object now shows the objective tolerance value in slot `@MIP$obj_tol`.

# TestDesign 1.0.0

TestDesign 1.0.0 is a major release that provides structural changes to better streamline the usage of functions and also achieve more structured abstraction.

## User-visible structural changes

* The function for fixed-test assembly `ATA()` is now named `Static()` to match with `Shadow()` for adaptive assembly.
* `Shadow()` now uses fewer arguments to match with `Static()` and to reduce redundant information in the arguments.
* `plotMaxInfo()` is removed. The functionality is subsumed under `plotInfo()`, which can be used by supplying a `constriants` object to the function.
* `Static()` now does not return the information plot by itself. The plotting should be done with `plotInfo()`.
* `plotInfo()` is now an S4 method.
* * Supplying an `item_pool` object gives a pool-level information plot.
* * Supplying the result from `Static()` gives an information plot based on the selected items.
* * Supplying a `constraints` object gives an information range plot from the test length specified in the constraints.
* * The comparison in the information range plot is now based on *k* randomly drawn items instead of *k* worst items.

## Non-visible structural changes

* Now uses S4 classes for item attributes (`item_attrib`), set attributes (`st_attrib`), and constraints (`constraints`).
* `ATA()` and `STA()` are merged into a single core function `runAssembly()`. The function translates high-level user data to low-level solver data.
* `ATA()` and `STA()` are removed. Deprecating the functions was not feasible because of the structural changes.

## New features

* Now supports item pools that include both set-based and discrete items.
* Now supports `lpsymphony` solver package from Bioconductor repository. Note: The current version 1.12.0 of `lpsymphony` will not install on R-devel due to `R CMD config F77` being deprecated. Installs normally on R 3.6.1.
* `plotShadow()` has a new `simple` argument that simplifies the chart by hiding items that are not included in any shadow test.
* Now uses disambiguated solver names. (e.g. `lpsymphony` and `Rsymphony` instead of `SYMPHONY`)
* `vignette('rsymphony')` is improved.
* `plotCAT()` and `plotShadow()` now uses a separate color for polytomous item responses.
* `plotInfo()` now uses a more polished plotting style.

## Bug fixes

* Fixed where `plotExposure()` was incorrectly plotting stimulus-level exposure rates along with item-level exposure rates.
* Fixed where using Bayesian methods would trigger an error in `Shadow()`.

# TestDesign 0.2.5

## Bug fixes

* Fixed where `gap_limit` was being incorrectly passed onto `SYMPHONY` in `ATA()` and `Shadow()`, instead of `gap_limit_abs`.
* Fixed where `gap_limit` was not being passed onto `GUROBI` in `ATA()` and `Shadow()`.
* Fixed where `time_limit` was not being passed onto `GUROBI` in `Shadow()`.
* Fixed where `time_limit` was being incorrectly passed in microseconds to `GLPK` in `Shadow()`.
* Fixed where a valid interval-based refresh policy triggered an error in `Shadow()`.
* Now prints an error message instead of crashing, when requesting adaptive assembly with set-based refresh policy on non-set-based item pools.

## Others

* `config_ATA@MIP` now has a new slot `$obj_tol` for controlling objective value tolerance.

# TestDesign 0.2.3

## Bug fixes

* Fixed where `ATA()` and `Shadow()` were returning incorrect solutions with set-based item pools in some cases.
* Fixed where `ATA()` and `Shadow()` were returning fewer than the specified number of items, due to solution vectors being not strictly binary in some cases.

## Others

* Improved `vignette('constraints')`.
* Added `URL` and `BugReports` fields to `DESCRIPTION` file.

# TestDesign 0.2.2

* This is the first published version.
