# TestDesign 1.4.0

## Test environments

* Local:
* * Windows 11 (R 4.2.2)
* GitHub Actions:
* * macOS 12 Monterey (R-release)
* * macOS 11 Big Sur (R-release)
* * Windows Server 2022 (R-release)
* * Windows Server 2019 (R-release)
* * Ubuntu 20.04 (R-devel, R-release, R-oldrel, R 3.6.3)

## R CMD check results

```
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Suggests or Enhances not in mainstream repositories:
  gurobi
```

NOTE is being raised from 'gurobi' not being available from CRAN. This cannot be fixed while making 'gurobi' available to 'TestDesign'. We tried not declaring 'gurobi' in `suggests:` which produces a WARNING, escalating the severity of the message. Other means of making 'gurobi' available seems to be against CRAN policy.

Information on obtaining 'gurobi' is described in `DESCRIPTION`.

## Downstream dependencies

The previous version 'TestDesign' 1.3.4 has 1 downstream dependency: 'maat' 1.1.0

Downstream dependency was checked using 'revdepcheck' available from https://github.com/r-lib/revdepcheck

## revdepcheck results

```
We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
```
