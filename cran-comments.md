## Resubmission
This is a resubmission. In this version we have:
* Added path argument to print_graphs(), which allows user to specify where to
print the graph files.
* Changed the print_graphs() example and the 'Using_plantecowrap.Rmd' vignette
so that path = tempdir()
* Added tryCatch() to fitacis2() so that when an A-Ci curve fit fails, the
output is "Failed". Notes on this effect have been added to descriptions of
fitacis2(), acisummary(), and in 'Using_plantecowrap.Rmd' on lines 74-75. This
change avoids an error where the entire function fails when there is one
exception/poor dataset.

## Test environments
* local Windows 10, R 3.6.2
* local macOS X install, R 3.6.3
* win-builder (devel and release)
* ubuntu 16.04 (on travis-ci), R 3.6.2

## R CMD check results
0 errors | 0 warnings | 1 note
* This is a new submission.

## Downstream dependencies
There are currently no downstream dependencies for this package.