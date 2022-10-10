#' Rename one or more fields (columns) in a OBIS tibble
#'
#' @export
#' @param x tibble of data
#' @param fields named character vector with oldname = newname
#' @return renamed tibble
rename_obis <- function(x, 
                        fields = c("name" = "scientificName",
                                   "longitude" = "decimalLongitude",
                                   "latitude" = "decimalLatitude")){
  #xnames <- colnames(x)
  
  #for (nm in names(fields)){
  #  if(nm %in% xnames) x <- dplyr::rename(x, fields[[nm]] = nm)
  #}
  x <- dplyr::rename(x, !!!fields)
  x
}

#' Plot the locations of a OBIS dataset
#' 
#' @export
#' @param x tibble of OBIS data
#' @param what character, one of 'base', 'ggplot', 'leaflet', 'gist'
#' @param ... other arguments for mapr package map_* functions
plot_obis <- function(x, what = c('base', 'ggplot', 'leaflet', 'gist')[1], ...){
  
  x <- rename_obis(x)
  switch(tolower(what[1]),
         "base" = mapr::map_plot(x, ...),
         "ggplot" = mapr::map_ggplot(x, ...),
         "leaflet" = mapr::map_leaflet(x, ...),
         "ggplot" = mapr::map_gist(x, ...))
  
}
