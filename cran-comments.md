## Test environments
* local OS X install, R 4.3.1.
* RStudio Server on linux Ubuntu 20.04.3 LTS R 4.2.1.
* `rhub::check_for_cran`.
* Github actions.
* Windows (via devtools::check_win_devel()).

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Note
Fix a 3 NOTES:

> Version: 2.3.3
Check: Rd cross-references
Result: NOTE 
  Found the following Rd file(s) with Rd \link{} targets missing package
  anchors:
    insertTab.Rd: tabPanel
  Please provide package anchors for all Rd \link{} targets not in the
  package itself and the base packages.