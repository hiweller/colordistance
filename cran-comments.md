## Test environments
* local ubuntu 20.04 install, x86_64-pc-linux-gnu, R version 4.0.3 (2020-10-10)
* win-builder
* ubuntu 16.04.6 LTS (travis-ci)


## R CMD check results
* local check: there were no ERRORs, WARNINGs, or NOTEs.
* travis-ci: there were no ERRORs, WARNINGs, or NOTEs.
* win-builder: there were no ERRORS or WARNINGS.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
	Maintainer: 'Hannah Weller <hannahiweller@gmail.com>'

	Possibly mis-spelled words in DESCRIPTION:
  	CIELAB (11:22)

This is a standard accepted spelling for the CIELAB (or CIELab or CIE L*a*b) color space.


## Downstream dependencies
R CMD check on all downstream dependencies performed using revdepcheck.

There was 1 reverse dependency (countcolors 0.9.0), which I maintain.

