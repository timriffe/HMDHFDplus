[![Build Status](https://travis-ci.org/timriffe/TR1.svg?branch=master)](https://travis-ci.org/timriffe/TR1)
[![CRAN status](https://www.r-pkg.org/badges/version/HMDHFDplus)](https://cran.r-project.org/package=HMDHFDplus)
[![issues](https://img.shields.io/github/issues-raw/timriffe/HMDHFDplus.svg)](https://github.com/timriffe/TR1/issues)
[![license](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://github.com/timriffe/TR1/tree/master/TR1/HMDHFDplus/LICENSE)

# HMDHFDplus
Read Human Mortality Database and Human Fertility Database Data from the Web

Install from CRAN like so:
```r
install.packages("HMDHFDplus")
```

Install the development version like so:
```r
# install.packages("remotes")

library(remotes)
install_github("timriffe/HMDHFDplus")
```

Then you can run the examples like so:

```r
library(HMDHFDplus)

?readHMDweb
?readJMDweb
?readCHMDweb
?readHFDweb
?readHFCweb
             
```
# Note
This repository replaces the previous one <https://github.com/timriffe/TR1> for purposes of maintaining the package. TR1 holds materials for my 2015 technical report on this package.
