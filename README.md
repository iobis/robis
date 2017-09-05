# robis

[![Travis-CI Build Status](https://travis-ci.org/iobis/robis.svg?branch=master)](https://travis-ci.org/iobis/robis)
[![Coverage Status](https://coveralls.io/repos/iobis/robis/badge.svg?branch=master&service=github)](https://coveralls.io/github/iobis/robis?branch=master)

R client for the OBIS API

## Installation

```R
install.packages("devtools")
devtools::install_github("iobis/robis")
```

## Occurrence

Get occurrences by scientific name:

```R
data <- occurrence("Abra alba")
```

Get occurrences by AphiaID:

```R
data <- occurrence(aphiaid = 141433)
```

Restrict fields in result set:

```R
data <- occurrence("Abra alba", fields = c("decimalLongitude", "decimalLatitude"))
```

Filter occurrences by [QC flags](http://www.ncbi.nlm.nih.gov/pubmed/25632106):

```R
data <- occurrence("Abra nitida", qc = c(22, 23))
```

Get occurrences by geometry:

```R
data <- occurrence("Abra alba", geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
```

Plot occurrences on a Leaflet map:

```R
leafletmap(data)
```

## Checklist

Get a checklist for a specific group and year:

```R
data <- checklist("Semelidae", year = 2005)
```

Get a checklist for a region:

```R
data <- checklist(geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
```

## Dataset

Get dataset information by dataset ID:

```R
datasets <- datasets(seq(2500, 2520)
```
