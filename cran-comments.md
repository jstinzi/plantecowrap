## Resubmission
This is a resubmission. In this version we have:
- Changed print_graphs() so that it shows the current working directory and
asks the user's permission to save the files in the working directory. When
permission is not granted (i.e. any response other than "Y"), the function
simply states "Graphs not printed"
- Added setwd(tempdir()) to the print_graphs() example and to the vignette
'Using_plantecowrap.Rmd' to avoid writing to user filespace without permission

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