# TestDesign v1.0.2

## Test environments

* Local: macOS (R 3.6.2), Windows 10 (R 3.6.2)
* Travis-CI: Ubuntu Linux 16.04 (R-release, R-devel)

## R CMD check results

```
Status: 1 NOTE

Suggests or Enhances not in mainstream repositories:
  gurobi
```

NOTE is being raised from 'gurobi' not being available from CRAN. This cannot be fixed while making 'gurobi' available to 'TestDesign'. We tried not declaring 'gurobi' in `suggests:` which produces a WARNING, escalating the severity of the message. Other means of making 'gurobi' available seems to be against CRAN policy.

Information on obtaining 'gurobi' is described in `DESCRIPTON`.

## Downstream dependencies

There are no downstream dependencies of the previous version of 'TestDesign' v1.0.0.
