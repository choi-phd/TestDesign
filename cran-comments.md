## Resubmission of 'TestDesign' package

The following issues raised by Martina Schmirl in email correspondence have been addressed in 'TestDesign' v0.2.2:

```
Please add small executable examples in your Rd-files to illustrate the 
use of the exported function but also enable automatic testing.
```

* All exported functions now provide their respective examples and tests. The example codes are intended to serve as user examples and automatic tests.
* Added examples for the following exported functions:
* * `buildConstraints()`
* * `createShadowTestConfig()`
* * `iparPosteriorSample()`
* * `lnHyperPars()`
* * `logitHyperPars()`
* * `makeItemPoolCluster()`
* * `mle()`
* * `OAT()`
* * `plotCAT()`
* * `plotExposure()`
* * `plotExposureRateFinal()`
* * `plotInfo()`
* * `plotShadow()`
* * Operators in `item_pool.operators.Rd`

* Removed all cpp functions from exports. Wrapper functions are available for the user.
* Also removed `addTrans()` and `plotRMSE()` from exports.

```
>    Removed the write calls in `datasets.R` and moved
>    them into dataset descriptions instead. The
>    instructions for write calls are necessary to make
>    the expected formats available to the
>    user.

Why? the write() functions are not part of your package and ppl are 
aware of write() functions?
Anyway, you can still keep those in the example. Just write to 
tempdir(). The user can change the direction themselves.
```

* Changed the examples in `datasets.R` to write to `tempdir()` and clean afterwards. Removed `\donttest` to make them also used in automatic testing.

The following changes were made to fix errors.

* `on.exit(par())` calls now only use relevant parameters. Only the relevant parameters are cached.
* Line 293 in `loading_functions.R`: assign empty strings to `ONOFF` column to avoid errors from trying parse `NA` values.
* Fixed segment table population in  `plotExposure()` and `plotExposureRateFinal()`.
* Operators documented in `item_pool.operators.Rd` now work correctly.

The following changes were made to improve usability.

* `subsetItemPool()` now also accepts a single numeric value for `select` argument.
* `plotInfo()` now has a default value for `theta` argument.

## Test environments

* local OS X install, R 3.6.1
* local Windows 10 install, R 3.6.1
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on R-hub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (on R-hub)
* Fedora Linux, R-devel, clang, gfortran (on R-hub)
* Debian Linux, R-devel, GCC ASAN/UBSAN (on R-hub)

## R CMD check results

```
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Seung W. Choi <schoi@austin.utexas.edu>'
  
New submission

Suggests or Enhances not in mainstream repositories:
  gurobi
```

Information on obtaining 'gurobi' is described in `DESCRIPTON`.

## Downstream dependencies

There are no downstream dependencies of the previous version of 'TestDesign' v0.2.1.
