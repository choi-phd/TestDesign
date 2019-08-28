## Test environments

* local OS X install, R 3.6.1
* local Windows 10 install, R 3.6.1
* R-hub with ``rhub::check_for_cran(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false"))``:
* * Fedora Linux, R-devel, clang, gfortran
* * Ubuntu Linux 16.04 LTS, R-release, GCC
* * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* * Debian Linux, R-devel, GCC ASAN/UBSAN


## R CMD check results

```
Status: 1 NOTE

Suggests or Enhances not in mainstream repositories:
  gurobi
```

Information on obtaining 'gurobi' is described in `DESCRIPTON`.


## Downstream dependencies

There are no downstream dependencies of the previous version of 'TestDesign' v0.2.2.
