# TestDesign 1.1.3

## Test environments

* Local: Windows 10 (R 4.0.2)
* Travis-CI:
* * Ubuntu Linux 16.04 (R-release, R-devel)
* * macOS 10.13, Xcode 9.4.1 (R-release, R-devel)

## R CMD check results

```
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Seung W. Choi <schoi@austin.utexas.edu>'

Days since last update: 4

Suggests or Enhances not in mainstream repositories:
  gurobi
```

NOTE is being raised from the update being submitted too soon. This version only contains minor updates: adding missing documentation and exposing a function to the user.

NOTE is being raised from 'gurobi' not being available from CRAN. This cannot be fixed while making 'gurobi' available to 'TestDesign'. We tried not declaring 'gurobi' in `suggests:` which produces a WARNING, escalating the severity of the message. Other means of making 'gurobi' available seems to be against CRAN policy.

Information on obtaining 'gurobi' is described in `DESCRIPTION`.

`Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)` was used to suppress the following NOTE:

```
* checking for future file timestamps ... NOTE
unable to verify current time
```

## Downstream dependencies

The previous version 'TestDesign' 1.1.2 does not have downstream dependencies.
