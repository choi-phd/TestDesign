# TestDesign 0.2.4

## Bug fixes

* Fix where `gap_limit` was incorrectly being passed onto `SYMPHONY` solver in `ATA()` and `Shadow()`, instead of `gap_limit_abs`. The two gap limits use the same default values, so this should not affect the solutions.
* Fix where `gap_limit` was not being passed onto `GUROBI` solver in `ATA()` and `Shadow()`.
* Fix where `time_limit` was not being passed onto `GUROBI` solver in `Shadow()`.
* Fix where `time_limit` was incorrectly being passed in ms units to `GLPK` solver in `Shadow()`.
* Fix where a valid interval-based refresh policy was triggering an error in `Shadow()`.
* Prevent the Shiny app from crashing when running adaptive assembly with a set-based refresh policy on item pools without sets.

## Others

* Explicitly add `obj_tol` in `config_ATA@MIP` to allow for controlling objective value tolerance.

# TestDesign 0.2.3

## Bug fixes

* Fix where `ATA()` and `Shadow()` returns incorrect solutions with stimulus-based item pools in some cases.
* Fix where `ATA()` and `Shadow()` returns fewer than specified number of items due to solution vectors being not strictly binary in some cases.
* Fix a typo in `vignette('constraints')`.

## Others

* Add `URL` and `BugReports` field to `DESCRIPTION` file.

# TestDesign 0.2.2

* Initial release.
