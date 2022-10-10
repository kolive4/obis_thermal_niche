.onLoad <- function(libname, pkgname){
  set_data_path = function(path = rappdirs::user_data_dir("obis_niche")){
    if(!dir.exists(path)){
      okay = dir.create(path, recursive = TRUE)
    }
    return(path)
  }
  path = set_data_path()
}
