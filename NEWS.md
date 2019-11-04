# TestDesign 1.0.0

TestDesign 1.0.0 is a major release that provides structural changes to better streamline the usage of the functions and also achieve more structured abstraction.

## User-visible structural changes

* The function for fixed-test assembly `ATA()` is now named `Static()` to match with `Shadow()` for adaptive assembly.
* `Shadow()` now uses fewer arguments to match with `Static()` and to reduce redundant information in the arguments.
* `plotMaxInfo()` is removed. The functionality is subsumed under `plotInfo()`, which can be used by supplying a `constriants` class object to the function.
* `Static()` now does not return the information plot by itself. The plotting should be done with `plotInfo()`.
* `plotInfo()` is now an S4 method.
* * Supplying `item_pool` object gives pool-level information plot.
* * Supplying the result from `Static` gives information plot based on the selected items.
* * Supplying `constraints` object gives information range plot from the test length specified in the constraints.
* * The comparison in the information range plot is now based on *k* randomly drawn items instead of the *k* worst items.

## Non-visible structural changes

* Now uses S4 classes for item attributes (`item_attrib`), set attributes (`st_attrib`), and constraints (`constraints`).
* `ATA()` and `STA()` are merged into a single core function `runAssembly()`. The function translates high-level user data to low-level solver data.
* `ATA()` and `STA()` are removed. Deprecating the functions was not feasible because of the structural changes.

## New features

* Now supports item pools that include both set-based and discrete items.
* Now supports `lpsymphony` solver package from Bioconductor repository. Note: The current version 1.12.0 of `lpsymphony` will not install on R-devel due to `R CMD config F77` being deprecated. Installs normally on R 3.6.1.
* `plotShadow()` has a new `simple` argument that simplifies the chart by hiding the items not included in any shadow test.
* Use disambiguated solver names. (e.g. `lpsymphony` and `Rsymphony` instead of `SYMPHONY`)
* `vignette('rsymphony')` is improved.
* `plotCAT()` and `plotShadow()` now uses a separate color for polytomous item responses.
* `plotInfo()` now uses a more polished plotting style.

## Bug fixes

* Fixes where `plotExposure()` was incorrectly plotting stimulus-level exposure rates along with item-level exposure rates.
* Fixes where using Bayesian methods would trigger an error in `Shadow()`.

# TestDesign 0.2.5

## Bug fixes

* Fixes where `gap_limit` was incorrectly passed onto `SYMPHONY` in `ATA()` and `Shadow()`, instead of `gap_limit_abs`.
* Fixes where `gap_limit` was not being passed onto `GUROBI` in `ATA()` and `Shadow()`.
* Fixes where `time_limit` was not being passed onto `GUROBI` in `Shadow()`.
* Fixes where `time_limit` was incorrectly passed in microseconds to `GLPK` in `Shadow()`.
* Fixes where a valid interval-based refresh policy triggered an error in `Shadow()`.
* Now prints an error message instead of crashing, when requesting adaptive assembly with set-based refresh policy on non-set-based item pools.

## Others

* Explicitly adds `obj_tol` in `config_ATA@MIP` to allow for controlling objective value tolerance.

# TestDesign 0.2.3

## Bug fixes

* Fixes where `ATA()` and `Shadow()` were returning incorrect solutions with set-based item pools in some cases.
* Fixes where `ATA()` and `Shadow()` were returning fewer than specified number of items, due to solution vectors being not strictly binary in some cases.
* `vignette('constraints')` is improved.

## Others

* Add `URL` and `BugReports` fields to `DESCRIPTION` file.

# TestDesign 0.2.2

* Initial release.
