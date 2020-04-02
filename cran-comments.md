## Resubmission
This is a resubmission. In this version we have:
* Removed user prompt from print_graphs() which did not make sense.
* 'path' argument must now be specified every time print_graphs() is used. The
default value was removed.
* file.path() is now used in jpeg() of print_graphs().
* Changed print_graphs() so that the 'height', 'width', 'res', and 'units'
arguments are actually passed on to jpeg().
* Added '...' to fitacis2(), print_graphs(), fit_topt_VJ(), and fit_topt_VJs()
so that arguments can be passed on to key component functions, including
plantecophys::fitaci() for fitacis2(), jpeg() for print_graphs(), and
minpack.lm::nlsLM() for fit_topt_VJ() and fit_topt_VJs().

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