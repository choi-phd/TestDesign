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

* `getSolution()` prints the indexes of the selected items from the results of `Static()` or `Shadow()`.
* `showConstraints()` returns the constraints table from a `constraints` object. This is a shortcut to access `@constraints` slot.

## Bug fixes

* Now recognizes `TM_TARGET_GAP_ACHIEVED` as valid status message in `lpsymphony` and `Rsymphony` solvers.
* `loadItemAttrib()` now ignores malformatted values in 'INDEX' column and regenerates correct indexes.
* `loadStAttrib()` now ignores malformatted values in 'STINDEX' column and regenerates correct indexes.
* `config_Static` object now shows objective tolerance value in slot `@MIP$obj_tol`.

# TestDesign 1.0.0

TestDesign 1.0.0 is a major release that provides structural changes to better streamline the usage of the functions, and also achieve more structured abstraction.

## User-visible structural changes

* The function for fixed-length assembly `ATA()` is now named `Static()` to match with `Shadow()` for adaptive assembly.
* `Shadow()` now uses fewer arguments to match with `Static()` and to reduce redundant information in the arguments.
* `plotMaxInfo()` is removed. The functionality is merged to `plotInfo()`, which can be used by supplying a `constriants` class object to the function.
* `Static()` now does not return the plot by itself. The plotting should be done with `plotInfo()`.
* `plotInfo()` is now a S4 method.
* * Supplying `item_pool` object gives pool-level information plot.
* * Supplying the result from `Static` gives information plot from the selected items.
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

* Fixed where `plotExposure()` was incorrectly plotting stimulus-level exposure rates along with item-level exposure rates.
* Fixed where using Bayesian methods would trigger an error in `Shadow()`.

# TestDesign 0.2.5

## Bug fixes

* Fixed where `gap_limit` was incorrectly passed onto `SYMPHONY` in `ATA()` and `Shadow()`, instead of `gap_limit_abs`.
* Fixed where `gap_limit` was not being passed onto `GUROBI` in `ATA()` and `Shadow()`.
* Fixed where `time_limit` was not being passed onto `GUROBI` in `Shadow()`.
* Fixed where `time_limit` was incorrectly passed in microseconds to `GLPK` in `Shadow()`.
* Fixed where a valid interval-based refresh policy triggered an error in `Shadow()`.
* Now prints an error message instead of crashing, when requesting adaptive assembly with set-based refresh policy on non-set-based item pools.

## Others

* `config_ATA@MIP` now has `$obj_tol` for controlling objective value tolerance.

# TestDesign 0.2.3

## Bug fixes

* Fixed where `ATA()` and `Shadow()` were returning incorrect solutions with set-based item pools in some cases.
* Fixed where `ATA()` and `Shadow()` were returning fewer than specified number of items, due to solution vectors being not strictly binary in some cases.

## Others

* Improved `vignette('constraints')`.
* Added `URL` and `BugReports` fields to `DESCRIPTION` file.

# TestDesign 0.2.2

* Initial release.
