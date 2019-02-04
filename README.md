# robis

[![CRAN robis](http://www.r-pkg.org/badges/version-last-release/robis)](https://cran.r-project.org/package=robis)
[![Travis-CI Build Status](https://travis-ci.org/iobis/robis.svg?branch=master)](https://travis-ci.org/iobis/robis)
[![Coverage Status](https://coveralls.io/repos/iobis/robis/badge.svg?branch=master&service=github)](https://coveralls.io/github/iobis/robis?branch=master)
[![DOI](https://zenodo.org/badge/47509713.svg)](https://zenodo.org/badge/latestdoi/47509713)

R client for the OBIS API

## Installation

```R
# CRAN
install.packages("robis")

# latest development version
install.packages("devtools")
devtools::install_github("iobis/robis")
```

## Occurrence

Get occurrences by scientific name:

```R
records <- occurrence("Abra alba")
```

Get occurrences by AphiaID:

```R
records <- occurrence(taxonid = 141433)
```

Get occurrences by geometry:

```R
records <- occurrence("Abra alba", geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
```

Plot occurrences on a Leaflet or ggplot2 map:

```R
map_leaflet(records)
map_ggplot(records)
```

## Checklist

Get a checklist for a taxonomic group:

```R
taxa <- checklist("Semelidae")
```

Get a checklist for a region:

```R
taxa <- checklist(geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
```
