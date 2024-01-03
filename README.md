# growthstandards [![Build Status](https://travis-ci.org/ki-tools/growthstandards.svg?branch=master)](https://travis-ci.org/ki-tools/growthstandards) [![codecov.io](https://codecov.io/gh/ki-tools/growthstandards/coverage.svg?branch=master)](https://codecov.io/gh/ki-tools/growthstandards?branch=master)

A collection of utility functions for conveniently converting anthropometric measurements to z-scores or centiles (and converting z-scores / centiles to measurements) for three growth standards:

- WHO growth standard (functions prefixed with `who_`)
- INTERGROWTH newborn size standard including very preterm (functions prefixed with `igb_`)
- INTERGROWTH fetal growth standard (functions prefixed with `igfet_`)
- INTERGROWTH gestational weight gain for mothers (functions prefixed with `iggwg_`)
- INTERGROWTH postnatal growth for preterm infants (functions prefixed with `igprepost_`)

These growth standards have previously not necessarily been easy to access inside an R analysis. In some cases R code has been provided for making conversions with the growth standard but in a form that is difficult to embed and generalize (copying and pasting code that will be frequently used is messy will almost surely lead to errors). Some standards are provided only through published coefficients. The goal here is to put all the standards into a single package with a unified interface, while providing additional functionality like interpolation for regions where standard's provided tables are sparse.

The growth standard conversion methods have been painstakingly checked for accuracy through comparisons with the standards provided by the original sources. However, we advise caution and double checking against the original sources when results will impact decisions. Links to the original sources can be found in the sections for each standard below.

## Installation

```r
# install.packages("remotes") # if "remotes" is not already installed
remotes::install_github("ki-tools/growthstandards")
```

## Usage

See [here](https://github.com/ki-tools/growthstandards) for several examples of how to use this package.
