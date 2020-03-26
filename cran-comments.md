## Resubmission
This is a resubmission. In this version we have:
* Reduced the 'LICENSE' file to two lines: YEAR & COPYRIGHT HOLDER.
* Added references to the 'DESCRIPTION' file describing the theoretical
background of the methods in the package.
* Added example datasets, 'example_1.csv' and 'example_2.csv', for tests,
examples, and the vignette 'Using_plantecowrap.Rmd'.
* Added vignette showing how 'example_1.csv' and 'example_2.csv' were created,
see 'Example_dataset_creation.Rmd'.
* Examples for all functions with examples were changed from \dontrun to
\donttest.
* All examples now use 'example_1.csv' or 'example_2.csv' so that they can be
run on a user's computer.
* Removed all code lines such as unlink() from tests.
* All tests now use example datasets to run.

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