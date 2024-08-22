# TestDesign 1.7.0

## Test environments

* Local:
* * macOS 14 (R 4.4.1)
* * Windows 11 (R 4.4.1)
* GitHub Actions:
* * macOS 12 (R-release)
* * Windows Server 2022 (R-release)
* * Windows Server 2019 (R-release)
* * Ubuntu 22.04 (R-devel, R-release, R-oldrel)

## R CMD check results

```
Status: 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
Suggests or Enhances not in mainstream repositories:
  gurobi
* checking package dependencies ... NOTE
Package suggested but not available for checking: 'gurobi'
```

The NOTEs are being raised from 'gurobi' not being available from CRAN. This cannot be fixed while making 'gurobi' available to 'TestDesign'. We tried not declaring 'gurobi' in `suggests:` which produces a WARNING, escalating the severity of the message. Other means of making 'gurobi' available seems to be against CRAN policy.

Information on obtaining 'gurobi' is described in `DESCRIPTION`.

## Downstream dependencies

The previous version 'TestDesign' 1.6.1 has 2 downstream dependencies:

- 'maat' 1.1.0
- 'PROsetta' 0.4.1

Downstream dependencies were checked using 'revdepcheck' available from https://github.com/r-lib/revdepcheck

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
