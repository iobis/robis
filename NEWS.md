* Added `keyword` parameter to `dataset()` function, enabling simple keyword search of dataset metadata via the `/dataset/search` endpoint.

# robis 2.11.3

Warnings and notes fixes.

# robis 2.11.3

Warnings and notes fixes.

# robis 2.11.1

Do not download extension definitions on package load.

# robis 2.11.0

Added `generate_citation()`.

# robis 2.10.1

Added contacts to `dataset()`.

# robis 2.10.0

Added the `unnest_extension()` function for extracting extension records.

# robis 2.9.0

Added `extensions` and `hasextensions` parameters.

# robis 2.8.2

Updated vignette so the code is not evaluated during CRAN checks.

# robis 2.8.1

Documentation fixes.

# robis 2.8.0

Added support for DNADerivedData records.

# robis 2.7.2

Improved handling of API connection errors. Skip tests that fail when the API is down on CRAN.

# robis 2.7.1

Faster extraction of measurements with `measurements()`.

# robis 2.7.0

Added the `instituteid` parameter.

# robis 2.6.1

Improved handling of API connection errors.

# robis 2.6.0

Improved handling of API connection errors.

# robis 2.5.0

Performance improvement by only fetching total hits on the first request.

# robis 2.4.0

Added `taxon()`.

# robis 2.3.11

Fix for https://github.com/iobis/robis/issues/71.

# robis 2.3.10

Added the `qcfields` parameter.

# robis 2.3.9

Documentation fixes.

# robis 2.3.8

Added `get_geometry()` to construct WKT geometries.

# robis 2.3.7

Returning tibbles instead of dataframes.

# robis 2.3.6

Added option to cache HTTP requests.

# robis 2.3.5

Added the option to include pure event records in the occurrence response.

# robis 2.3.4

Added the option to filter on quality flags.

# robis 2.3.3

Added support for dropped and absence records.

# robis 2.3.1

Fixed the Leaflet base map.

# robis 2.3.0

Added quality flags to occurrence results.

# robis 2.1.12

Improved dataset usage metrics.

# robis 2.1.11

Bugfix for checklist of unknown taxon.

# robis 2.1.10

Added hab parameter for IOC-UNESCO HAB species.

# robis 2.1.9

Fix for empty checklists.

# robis 2.1.8

Critical fix for `occurrence()` bug introduced in version 2.1.7.

# robis 2.1.7

Fix for duplicate rows in `checklist()`.

# robis 2.1.6

Fix for `dataset()`.

# robis 2.1.5

Added `exclude` and `fields` parameters, added EPSG:3031 support for Leaflet maps.

# robis 2.1.0

Added `dataset()`, `area()`, and `node()`.

# robis 2.0.0

This is a major release which links robis to the new OBIS data system released in January 2019.
