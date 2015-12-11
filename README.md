# obisclient

R client for the OBIS API

## Installation

```R
devtools::install_github("iobis/obisclient")
```

## Occurrence

Get occurrences by scientific name:

```R
data <- occurrence("Abra alba")
```

Get occurrences by AphiaID:

```R
data <- occurrence(aphiaid=141433)
```

Restrict fields in result set:

```R
data <- occurrence("Abra alba", fields=c("decimalLongitude", "decimalLatitude"))
```

Filter occurrences by [QC flags](http://www.ncbi.nlm.nih.gov/pubmed/25632106):

```R
data <- occurrence("Abra nitida", qc=c(22, 23))
```

Get occurrences by geometry:

```R
data <- occurrence("Abra alba", geometry="POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
```

## Taxon

Get a taxon list for a specific group and year:

```R
taxa <- taxon("Semelidae", year=2005)
```

Get a taxon list for a region:

```R
taxa <- taxon(geometry="POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
```

Get a taxon list for a region in marineregions.org:

```R
require(mregions)
taxa <- taxon(geometry=as_wkt(region_shp(name="Belgian Exclusive Economic Zone")))
```
