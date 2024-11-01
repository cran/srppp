# srppp - R package to read XML dumps of the Swiss Register of Plant Protection Products

<!-- badges: start -->
[![srppp status badge](https://agroscope-ch.r-universe.dev/badges/srppp)](https://agroscope-ch.r-universe.dev/srppp)
[![R-CMD-check](https://github.com/agroscope-ch/srppp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/agroscope-ch/srppp/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/832080563.svg)](https://doi.org/10.5281/zenodo.13993340)
<!-- badges: end -->

## Description

Functions to generate data objects from XML versions of the Swiss
Register of Plant Protection Products (SRPPP). An online version of the
register can be accessed at <https://www.psm.admin.ch/de/produkte>. There is no
guarantee of correspondence of the data read in using this package with that
online version, or with the original registration documents.  Also, the
Federal Food Safety and Veterinary Office, coordinating the authorisation of
plant protection products in Switzerland, does not answer requests regarding
this package. 

## Installation

```
install.packages("srppp",
  repos = c("https://agroscope-ch.r-universe.dev", "https://cran.r-project.org"))
```

## Documentation

An online version of the documentation of the current development version of
the package is available [on github](https://agroscope-ch.github.io/srppp/).

## See also

You may also be interested in our
[agroscope-ch/srppphist](https://github.com/agroscope-ch/srppphist) package containing
historical registration data starting from 2011.
