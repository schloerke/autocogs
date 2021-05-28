#### 2020-05-28

A minor update has been made to be compatible with the upcoming release of ggplot2.

## Test environments

GitHub actions: https://github.com/schloerke/autocogs/actions/runs/887052179

- macOS-latest (release)
- windows-latest (release)
- windows-latest (oldrel)
- ubuntu-18.04 (devel)
- ubuntu-18.04 (release)
- ubuntu-18.04 (oldrel)
- ubuntu-18.04 (3.5)
- ubuntu-18.04 (3.4)

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 1 reverse dependency, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

#### 2020-04-02

> Result: NOTE
>     Namespaces in Imports field not imported from:
>      ‘MASS’ ‘broom’ ‘diptest’ ‘ggplot2’ ‘hexbin’ ‘moments’
>      All declared Imports should be used.

All of these packages are used to create functions (all but `hexbin`) or are underlying packages used in testing (`hexbin`). Except for `hexbin`, all packages are accessed directly using `::`.

Remaining errors have been addressed.  Thank you for your patience!

Best,
Ryan


#### 2020-03-06

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_autocogs.html>.

Please correct before 2020-03-20 to safely retain your package on CRAN.

Best,
-k


## Test environments

* local OS X install, R 3.6.3
* win-builder (oldrelease, release, and devel)
* Travis-CI - Ubuntu (oldrelease, release, devel)
* r-hub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

#### 2020-04-02

> Result: NOTE
>     Namespaces in Imports field not imported from:
>      ‘MASS’ ‘broom’ ‘diptest’ ‘ggplot2’ ‘hexbin’ ‘moments’
>      All declared Imports should be used.

All of these packages are used to create functions (all but `hexbin`) or are underlying packages used in testing (`hexbin`). Except for `hexbin`, all packages are accessed directly using `::`.

Remaining errors have been addressed.  Thank you for your patience!

Best,
Ryan


#### 2020-03-06

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_autocogs.html>.

Please correct before 2020-03-20 to safely retain your package on CRAN.

Best,
-k


## Test environments

* local OS X install, R 3.6.3
* win-builder (oldrelease, release, and devel)
* Travis-CI - Ubuntu (oldrelease, release, devel)
* r-hub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
