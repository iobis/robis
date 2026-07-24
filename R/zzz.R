.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return(invisible())
  }
  packageStartupMessage(
    "Note: For large downloads, see alternative data\n",
    "      access options at https://obis.org/data/access/\n",
    "      (including GeoParquet on AWS Open Data)."
  )
}
