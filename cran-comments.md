# TestDesign 1.2.4

## Test environments

* Local:
* * Windows 10 (R 4.1.0)
* * Ubuntu 20.04 (R 4.1.0)
* GitHub Actions:
* * Windows Server 2019 (R-release)
* * macOS Catalina 10.15 (R-release)
* * Ubuntu 20.04 (R-release, R-devel, R-oldrel)

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

The previous version 'TestDesign' 1.2.3 does not have downstream dependencies.
