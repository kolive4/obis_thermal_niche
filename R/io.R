#' Retrieve a data path
#' 
#' @export
#' @param ... character, one or more file path segments to be post-pended to \code{root}
#' @param root character, the root data directory path
#' @return character path specification
get_path <- function(..., root = rappdirs::user_data_dir("obis_niche")){
  file.path(root, ...)
}

#' Retrieve a listing of local files
#' 
#' @export
#' @param path the path to the datasets
#' @param full.names logical return basename or full path specs?
#' @param strip character or NA, if character then strip this pattern. Ignored
#'   if full.names is \code{TRUE}
#' @return character names (possibly filenames)
list_obis <- function(path = get_path(), 
                      full.names = FALSE,
                      strip = c(NA, ".csv.gz")[2]){
  ff <- list.files(path, pattern = "^.*\\.csv", full.names = full.names)
  if (!full.names && !is.na(strip[1])){
    ff <- sub(strip, "", ff, fixed = TRUE)
  }
  ff
}


# # fetch occurrence data for given species from GBIF
# # 
# # @export
# # @param species character, latin name of species to fetch
# # @param cache logical, if true save results to cache
# # @return data frame in the form of a tibble (or an error object if issues arise)
# fetch_gbif = function(species = "Carcharodon carcharias", cache = TRUE){
#   occurrence <- try(rgbif::occ_search(scientificName = species[1]))
#   if (!inherits(occurrence, "try-error")){
#     occurrence <- occurrence$data
#     if(cache == TRUE){
#       path <- get_path(paste0(species, ".csv.gz"))
#       occurrence <- readr::write_csv(occurrence, path)
#     }
#   }
#   return(occurrence)
# }


# species = "Squalus acanthias subsp. acanthias Linnaeus, 1758"

#' Fetch occurrence data for given species from OBIS
#' 
#' @export
#' @param species character, latin name of species to fetch
#' @param cache logical, if true save results to cache
#' @param verbose logical, if true prints counter of progress
#' @param progress logical, if true print progress bar and ignore verbose
#' @return data frame in the form of a tibble
fetch_obis = function(species = "Carcharodon carcharias", 
                      cache = TRUE, 
                      verbose = FALSE, 
                      progress = !verbose){
  
  if(progress == TRUE){
    verbose = FALSE
  }
  
  DONE = FALSE
  LIMIT = 500
  CUR = 0
  FROM = "obis"
  
  #x <- spocc::occ(query = species[1], limit = LIMIT, from = FROM, 
  #                start = CUR, throw_warnings = FALSE)
  x <- robis::occurrence(scientificName = species[1], limit = LIMIT, start = CUR)
  
  #COUNT = x[[FROM]]$meta$found
  COUNT <- x$meta$count
  NCHUNKS = ceiling(COUNT / LIMIT)
  ICHUNK = 1
  
  xx = vector(mode = "list", length = NCHUNKS)
  #xx[[ICHUNK]] <- x[[FROM]]
  xx[[ICHUNK]] <- x
  ICHUNK = ICHUNK + 1
  CUR = CUR + LIMIT
  
  if(progress) pb = txtProgressBar(min = 0, max = NCHUNKS, initial = ICHUNK, style = 3) 
  
  while(ICHUNK <= NCHUNKS){
    if(verbose){
      cat(sprintf("%i:%i", ICHUNK, CUR))
    }
    if(progress) setTxtProgressBar(pb, ICHUNK)
    #x <- spocc::occ(query = species[1], limit = LIMIT, 
    #                from = FROM, start = CUR, 
    #                throw_warnings = FALSE)
    x <- robis::occurrence(scientificName = species[1], limit = LIMIT, start = CUR)
    #xx[[ICHUNK]] <- x[[FROM]]
    xx[[ICHUNK]] <- x
    ICHUNK = ICHUNK + 1
    CUR = CUR + LIMIT
  }
  
  if(progress) close(pb)
  
  occurrence = lapply(xx, 
                      function(y){
                        return(y$data |> 
                                 dplyr::as_tibble() |> 
                                 dplyr::select(-dplyr::any_of("networkKeys")))
                      }) |>
    dplyr::bind_rows()
  
  
  if(cache == TRUE){
    path <- get_path(paste0(species[1], ".csv.gz"))
    occurrence <- readr::write_csv(occurrence, path)
  }
  return(occurrence)
}

#' function to read obis with the option to fetch
#' 
#' reads file by species name, if file doesn't exist, first try to fetch it then save it to the cache
#' @export
#' @param species character, latin name of species
#' @param refresh logical, if true fetch fresh set of data
#' @return dwc logical, if TRUE trim to the recommended Darwin Core content
#' @return data frame in the form of a tibble
read_obis = function(species = "Carcharodon carcharias", 
                     refresh = FALSE,
                     dwc = TRUE){
  filename = get_path(paste0(species[1], ".csv.gz"))
  if (!file.exists(filename) || refresh == TRUE) {
    x = fetch_obis(species = species)
  }
  else{
    x <- #readr::read_csv(filename, show_col_types = FALSE)
      tidytable::fread.(filename) |>
      dplyr::as_tibble()
  }
  if (dwc) x <- as_dwc(x)
  return(x)
}


#' Create a template with required, recommended or both (default) columns.
#' 
#' @export
#' @param what character, one or more of 'required' and 'recommended'
#' @param n numeric, the number of rows to return,  less than equal to zero
#'   yields an empty tibble
#' @return tibble with n rows
template_dwc <- function(what = c("required", "recommended"), n = 0) {
  
  if (n <= 0) {
    N <- 1
  } else {
    N = n
  }
  
  char = rep(NA_character_, N)
  num = rep(NA_real_, N)
  int = rep(NA_integer_, N)
  
  x <- dplyr::tibble(
    occurrenceID = char,
    basisOfRecord = char,
    scientificName = char,
    eventDate = rep(Sys.Date(), N))
  
  if ("recommended" %in% tolower(what)){
    
    x <- dplyr::bind_cols(x,
                          dplyr::tibble(
                            taxonRank = char,
                            kingdom = char,
                            decimalLatitude = num,
                            decimalLongitude = num,
                            geodeticDatum = char,
                            countryCode = char,
                            individualCount = int,
                            organismQuantity = int, 
                            organismQuantityType = char))
    
  }
  
  if (n <= 0) x <- dplyr::slice(x, 0)
  
  x
}

#' Modify an input OBIS dataset to include only Darwin Core required and 
#' recommended fields.
#' 
#' @seealso \href{https://ipt.gbif.org/manual/en/ipt/2.5/occurrence-data}{Darwin Core}
#' @param x tibble of raw OBIS data
#' @param template template as a tibble
#' @return tibble with defined columns
as_dwc <- function(x, template = template_dwc(n=1)){
  
  as_date <- function(x, format = "%Y-%m-%d %H:%M:%S"){
    
  }
  
  tnames <- colnames(template)
  tclass <- sapply(template, class)
  y <- dplyr::select(x, dplyr::any_of(tnames))
  ynames <- colnames(y)
  for (nm in tnames){
    if (nm %in% ynames){
      if (tclass[[nm]] == "Date"){
        y <- dplyr::mutate(y, {{ nm }} := as.Date(.data[[nm]])) 
      } else {
        class(y[[nm]]) <- tclass[[nm]]
      }
    } else {
      y <- dplyr::mutate(y, !!nm := template[[nm]])
    }
  }
  y  
}


