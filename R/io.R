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

#' Fetch a species from OBIS
#' 
#' @export
#' @param scientificname character, the Latin name for a species
#' @param ... other arguments for \code{\link[robis]{occurrence}}
#' @param save_file NA or a path specification to save the file
#' @param template data frame defining minimal fields 
#' @return tibble, possibly empty if a species is not found
fetch_obis <- function(scientificname = 'Carcharodon carcharias', 
                          save_file = file_name(scientificname),
                          template = species_template(),
                          ...){
  
  autofill <- function(x, template = species_template()){
    xnames <- colnames(x)
    tnames <- colnames(template)
    ix <- !(tnames %in% xnames)
    if (any(ix)){
      missingnames <- tnames[ix]
      for (mn in missingnames) x[[mn]] = template[[mn]][1]
    } 
    for (nm in tnames) mode(x[[nm]]) <- mode(template[[nm]])
    x |> dplyr::select(dplyr::all_of(tnames))
  }
  
  x <- try(robis::occurrence(scientificname = scientificname[1], fields = names(template), ...))
  if (!inherits(x, 'try-error') && nrow(x) > 0){
    x <- dplyr::mutate(x, across(dplyr::everything(), as.character)) |>
      autofill(template) |>
      dplyr::mutate(eventDate = format(as.Date(substring(.data$eventDate, 1, nchar("YYYY-mm-dd")), 
                                               format = "%Y-%m-%d"), 
                                       format = "%Y-%m-%d")) |>
      dplyr::filter(!grepl("void_", .data$id, fixed = TRUE))
  }
  if (!inherits(x, 'try-error') && nrow(x) > 0 && !is.na(save_file)){
    x <- readr::write_csv(x, save_file)
  }
  x
}


#' function to read obis with the option to fetch
#' 
#' reads file by species name, if file doesn't exist, first try to fetch it then save it to the cache
#' @export
#' @param species character, latin name of species
#' @param refresh logical, if true fetch fresh set of data
#' @param dwc logical, if TRUE trim to the recommended Darwin Core content
#' @return data frame in the form of a tibble
read_obis = function(species = "Carcharodon carcharias", 
                     refresh = FALSE,
                     dwc = TRUE){
  filename = get_path(paste0(species[1], ".csv.gz"))
  if (!file.exists(filename) || refresh == TRUE) {
    x = fetch_obis(species = species)
  }
  else{
    x <- readr::read_csv(filename, show_col_types = FALSE)
      tidytable::fread(filename) |>
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


#' Generate a dummy template of data
#' 
#' @export
#' @param n numeric, the number of rows to create
#' @param eventDate_type character, what class should eventDate be?
#' @return tibble
species_template <- function(n = 1, eventDate_type = c("character", "date")[1]){
  x <- dplyr::tibble(
    id                 = paste("void", seq_len(n), sep = "_"),
    scientificName     = "",
    eventDate          = "",
    decimalLongitude   = NA_real_,
    decimalLatitude    = NA_real_,
    depth              = NA_real_,
    sst                = NA_real_,
    sss                = NA_real_)
  if (tolower(eventDate_type[1]) == "date"){
    x <- dplyr::mutate(x, eventDate = Sys.Date())
  }
  x
}

#' Convert from file name to scientific_name
#' 
#' @export
#' @param x chr, name of file
#' @param ext chr, file extension to be removed
#' @param sep chr, separator of file name
#' @return character vector of scientific names
scientific_name <- function(x = 'carcharodon_carcharias.csv.gz', 
                            ext = '.csv.gz', 
                            sep = '_'){
  
  x = basename(x)
  x = sub(ext, '', x, fixed = TRUE)
  x = paste0(toupper(substring(x, 1,1)),substring(x, 2))
  x = gsub(sep," ", x, fixed = TRUE)
  x
}

#' Convert from scientific name to file name
#' 
#' @param x chr, scientific name of species
#' @param ext chr, file extension to be added
#' @param sep chr, separator of file name (between genus and species)
#' @param path chr, path of which file is stored
#' @return character vector of file names
file_name <- function(x = 'Carcharodon carcharias', 
                      ext = '.csv.gz', 
                      sep = "_", 
                      path = get_path()){
  
  x = tolower(x)
  x = gsub(" ", sep, x, fixed = TRUE)
  x = paste0(x, ext)
  x = file.path(path, x)
  x
}

