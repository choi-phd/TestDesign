# TestDesign 0.2.4

## Bug fixes

* Fix where `gap_limit` was passed onto `SYMPHONY` in `ATA()` and `Shadow()` instead of `gap_limit_abs`. The two gap limits used the same default value, so this should not have affected the solutions.
* Fix where `gap_limit` was not passed onto `GUROBI` in `ATA()` and `Shadow()`.
* Fix where `time_limit` was not passed onto `GUROBI` in `Shadow()`.
* Fix where `time_limit` was incorrectly passed in microseconds to `GLPK` in `Shadow()`.
* Fix where a valid interval-based refresh policy triggered an error in `Shadow()`.
* Prevent the Shiny app from crashing when the user erroneously requests an adaptive assembly with a set-based refresh policy on item pools without item sets.

## Others

* Explicitly add `obj_tol` in `config_ATA@MIP` to allow for controlling objective value tolerance.

# TestDesign 0.2.3

## Bug fixes

* Fix where `ATA()` and `Shadow()` returns incorrect solutions with stimulus-based item pools in some cases.
* Fix where `ATA()` and `Shadow()` returns fewer than specified number of items due to solution vectors being not strictly binary in some cases.
* Update formatting of `vignette('constraints')`.

## Others

* Add `URL` and `BugReports` fields to `DESCRIPTION` file.

# TestDesign 0.2.2

* Initial release.
