## Test environments
* local OS X install, R 3.4.4
* ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1 (2017-01-27)
* Windows (via devtools::build_win(), R-devel)

[![Build Status](https://travis-ci.org/DivadNojnarg/bs4Dash.svg?branch=master)](https://travis-ci.org/DivadNojnarg/bs4Dash)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Revdep check
Nothing to declare

## Note for CRAN maintainers
There were 2 notes on https://cran.r-project.org/web/checks/check_results_bs4Dash.html:

- r-devel-linux-x86_64-fedora-clang

checking installed package size ... NOTE
  installed size is 5.0Mb
  sub-directories of 1Mb or more:
    AdminLTE3-3.0.0 1.0Mb
    doc 1.8Mb
    help 1.3Mb
    
- r-patched-solaris-x86

checking installed package size ... NOTE
  installed size is 5.6Mb
  sub-directories of 1Mb or more:
    AdminLTE3-3.0.0 1.1Mb
    doc 2.2Mb
    help 1.4Mb
    
I significantly reduced the size of the documentation which should fix these notes!