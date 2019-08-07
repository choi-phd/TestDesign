## Resubmission of 'TestDesign' package

The following issues raised by Martina Schmirl in email correspondence have been addressed in 'TestDesign' v0.2.1:

```
Please provide a link to the gurobi website to the description field of 
your DESCRIPTION file in the form
<http:...> or <https:...>
with angle brackets for auto-linking and no space after 'http:' and
'https:'.
```
* Added a link in the description field of DESCRIPTION file.

```
Please write references in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.
```
* Added references in appropriate forms.

```
Please always write package names, software names and API names in 
single quotes in the title and the description field.
f.i.: --> 'gurobi'
```
* Added single quotes in the description field of DESCRIPTION file.

```
Please make sure that you do not change the user's options, par or 
working directory. If you really have to do so, please ensure with an 
*immediate* call of on.exit() that the settings are reset when the 
function is exited, similar to this:
...
oldpar <- par(mfrow = c(1,2))
on.exit(par(oldpar))
...
f.i.: plotShadow-methods
```
* Added `oldpar` caching and `on.exit` revert calls for all 5 cases of `par()` calls across the package: Lines 210, 367, 514, 1777, 2910 in `shadow_functions.R` (line numbers based on the revised codes).

```
Are you sure you want to set a seed to 1 within a function? manuscript.R
```
* Removed `manuscript.R` from the package. These are not necessary to be included in the package.

```
Please ensure that your functions do not write by default or in your 
examples/vignettes/tests in the user's home filespace (including the 
package directory and getwd()). That is not allowed by CRAN policies. 
Please only write/save files if the user has specified a directory in 
the function themselves. Therefore please omit any default path = 
getwd() in writing functions.
In your examples/vignettes/tests you can write to tempdir().

\dontrun{} should be only used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user. That's why wrapping examples in \dontrun{} adds the comment 
("# Not run:") as a warning for the user.
```
* Removed the write calls in `datasets.R` and moved them into dataset descriptions instead. The instructions for write calls are necessary to make the expected formats available to the user.

```
Please add small executable examples in your Rd-files to illustrate the 
use of the exported function but also enable automatic testing.
When creating the examples please keep in mind that the structure
would be desirable:
\examples{
    examples for users and checks:
    executable in < 5 sec
    \dontshow{
        examples for checks:
        executable in < 5 sec together with the examples above
        not shown to users
    }
    \donttest{
        further examples for users; not used for checks
        (f.i. data loading examples )
    }
    if(interactive()){
        functions not intended for use in scripts, or are supposed
    to only run interactively (f.i. shiny)
    not used for checks
    }
}
```
* Changed the examples in `datasets.R` to `\donttest`.
* Added an example for `ATA()` in `ATA_class.R`.
* Added examples for `loadItemPool()`, `loadItemAttrib()`, `loadStAttrib()`, `loadConstraints()`, `updateConstraints()` in `loading_functions.R`.

The following functional changes were made to improve usability.

* `createStaticTestConfig()` now fills targetWeight automatically to 1s if not explicitly supplied. Unit tests in `tests/testthat/test-ATA.R` were appropriately changed to reflect this.

The following cosmetic changes were made to improve consistency.

* Changed the examples in `item_functions.R` to use `<-` for assignments.
