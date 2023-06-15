## Test environments
* local OS X install, R 4.1.3.
* RStudio Server on linux Ubuntu 20.04.3 LTS R 4.2.1.
* `rhub::check_for_cran`.
* Github actions.
* Windows (via devtools::check_win_devel()).

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Note
I remove the svg file from /man and replaced it by a smaller png, which fixes the NOTES
on https://cran.r-project.org/web/checks/check_results_bs4Dash.html.
