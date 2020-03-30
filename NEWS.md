# plantecowrap 1.0.2

## Minor changes

- Changed print_graphs() so that it shows the current working directory and
asks the user's permission to save the files in the working directory. When
permission is not granted (i.e. any response other than "Y"), the function
simply states "Graphs not printed".
- Added setwd(tempdir()) to the print_graphs() example and to the vignette
'Using_plantecowrap.Rmd' to avoid writing to user filespace without permission

# plantecowrap 1.0.1

## Minor changes

- Reduced 'LICENSE' file to two lines: YEAR & COPYRIGHT HOLDER
- Added references to the 'DESCRIPTION' file describing the theoretical
background of the methods in the package.
- Added example datasets, 'example_1.csv' and 'example_2.csv', for tests,
examples, and the vignette 'Using_plantecowrap.Rmd'.
- Added vignette showing how 'example_1.csv' and 'example_2.csv' were created,
see 'Example_dataset_creation.Rmd'.
- Examples for all functions with examples were changed from \dontrun to
\donttest.
- All examples now use 'example_1.csv' or 'example_2.csv' so that they can be
run on a user's computer.
- Removed all code lines such as unlink() from tests.
- All tests now use example datasets to run.
- Updated 'README.md' to better reflect the 'DESCRIPTION' file.

# plantecowrap 1.0.0

## Major Changes

- Created the package