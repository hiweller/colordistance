## Test environments
* local OS X install, x86_64-apple-darwin15.6.0, R version 3.5.2 (2019-01-08)
* win-builder
* Fedora Linux (via rhub)
* ubuntu 16.04 (via rhub)


## R CMD check results
* local check: there were no ERRORs, WARNINGs, or NOTEs.
* linux: there were no ERRORs, WARNINGs, or NOTEs.
* win-builder: there were no ERRORS or WARNINGS.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
	Maintainer: 'Hannah Weller <hannahiweller@gmail.com>'

	Possibly mis-spelled words in DESCRIPTION:
  	CIELAB (11:22)

This is a standard accepted spelling for the CIELAB (or CIELab or CIE L*a*b) color space.


## Downstream dependencies
R CMD check on all downstream dependencies performed using revdepcheck.

There was 1 reverse dependency (countcolors 0.9.0), which I am currently updating. The issue was redundant naming of vignettes, rather than issues with package functions, and I have just submitted a package update to fix this problem and a couple of bugs.
