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

Get occurrences by geometry:

```R
data <- occurrence("Abra alba", geometry="POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
```
