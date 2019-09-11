## Resubmission

* This is a resubmission of TestDesign 0.2.4.
* The examples in `plotExposureRateFinal` and `plotExposure-methods` are now wrapped with `\donttest{}`. This is the only viable option for the purposes of this package, and it is not possible to further reduce the execution time. The examples are running a series of optimization solvers, and we are already using bare minimum settings for the examples.
* Other NOTEs are being raised from 'gurobi' not being available from CRAN. This cannot be fixed while making 'gurobi' available to 'TestDesign'. We tried not declaring 'gurobi' in `suggests:` which produces a WARNING, escalating the severity of the message. Other means of making 'gurobi' available seems to be against CRAN policy.

## Test environments

* Local: macOS (R 3.6.1), Windows 10 (R 3.6.1)
* Travis-CI: Ubuntu Linux 16.04 (R-release, R-devel)


## R CMD check results

```
Status: 1 NOTE

Suggests or Enhances not in mainstream repositories:
  gurobi
```

Information on obtaining 'gurobi' is described in `DESCRIPTON`.


## Downstream dependencies

There are no downstream dependencies of the previous version of 'TestDesign' v0.2.3.
