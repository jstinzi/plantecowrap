# plantecowrap 1.0.4

## Minor changes

- Removed user prompt from print_graphs() which did not make sense.
- 'path' argument must now be specified every time print_graphs() is used. The
default value was removed.
- file.path() is now used in jpeg() of print_graphs().
- Changed print_graphs() so that the 'height', 'width', 'res', and 'units'
arguments are actually passed on to jpeg().
- Added '...' to fitacis2(), print_graphs(), fit_topt_VJ(), and fit_topt_VJs()
so that arguments can be passed on to key component functions, including
plantecophys::fitaci() for fitacis2(), jpeg() for print_graphs(), and
minpack.lm::nlsLM() for fit_topt_VJ() and fit_topt_VJs().

# plantecowrap 1.0.3

## Minor changes

- Added path argument to print_graphs(), which allows user to specify where to
print the graph files.
- Changed the print_graphs() example and the 'Using_plantecowrap.Rmd' vignette
so that path = tempdir()
- Added tryCatch() to fitacis2() so that when an A-Ci curve fit fails, the
output is "Failed". Notes on this effect have been added to descriptions of
fitacis2(), acisummary(), and in 'Using_plantecowrap.Rmd' on lines 74-75. This
change avoids an error where the entire function fails when there is one
exception/poor dataset.

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