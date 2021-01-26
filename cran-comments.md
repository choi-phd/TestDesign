# TestDesign 1.2.2

## Test environments

* Local:
* * Windows 10 (R 4.0.3)
* * Ubuntu 20.04 (R 4.0.3 with clang-ASAN)
* GitHub Actions:
* * Windows Server 2019 (R-release)
* * macOS Catalina 10.15 (R-release)
* * Ubuntu 20.04 (R-release, R-devel, R-oldrel)

* In clang-ASAN, gcc-ASAN, the use of 'lpsymphony' in tests was causing heap-buffer-overflow:
```
  =================================================================
  ==3289600==ERROR: AddressSanitizer: heap-buffer-overflow on address 0x62500034f018 at pc 0x000000448067 bp 0x7ffd335f7ee0 sp 0x7ffd335f76a0
  READ of size 1096 at 0x62500034f018 thread T0
      #0 0x448066 in memcpy /data/gannet/ripley/Sources2/LLVM/11.0.0/llvm-project-11.0.0/compiler-rt/lib/asan/../sanitizer_common/sanitizer_common_interceptors.inc:808:5
      #1 0x7fd0a93eac31 in sym_explicit_load_problem(SYM_ENVIRONMENT*, int, int, int*, int*, double*, double*, double*, char*, double*, double*, char*, double*, double*, char) (/lib64/libSym.so.3+0x17c31)
      #2 0x7fd0a9487394 in lp_symphony_solve /tmp/RtmpHUBnog/R.INSTALL16d35062cfdc66/lpsymphony/src/lp_symphony.cc:47:4
```
This cannot be fixed on our end. As a workaround, a test in 'TestDesign' 1.2.1 that was using 'lpsymphony' is now conditionally disabled.

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

The previous version 'TestDesign' 1.2.1 does not have downstream dependencies.
