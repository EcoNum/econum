# econum

[![Linux & OSX Build Status](https://travis-ci.org/EcoNum/econum.svg )](https://travis-ci.org/EcoNum/econum)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/EcoNum/econum?branch=master&svg=true)](http://ci.appveyor.com/project/phgrosjean/econum)
[![Coverage Status](https://img.shields.io/codecov/c/github/EcoNum/econum/master.svg)
](https://codecov.io/github/EcoNum/econum?branch=master)
[![CRAN Status](http://www.r-pkg.org/badges/version/econum)](http://cran.r-project.org/package=econum)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](http://www.gnu.org/licenses/MIT)

Various routines to control scientific devices or to perform ecophysiological calculations.

## Installation

Make sure you have the **devtools** R package installed:

```r
install.packages("devtools")
```

Use `install_github()` to install the **econum** package:

```r
devtools::install_github("EcoNum/econum")
```
    
R should install all required dependencies automatically, and then it should compile and install **econum**.

Latest devel version also available in sources and Windows binaries at [appveyor](https://ci.appveyor.com/project/phgrosjean/en-test/build/artifacts) _(only available if passing last Windows build!)_.



## Usage

Make the **econum** package available in your R session:

```r
library("econum")
```
    
Get help about this package:

```r
library(help = "econum")
```
