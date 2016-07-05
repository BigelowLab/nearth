
#' Test for the existence of a Natural Earth path in R's \code{options()}
#'
#' @export
#' @param what character, either 'vector' (default) or 'raster'
#' @return named logical, TRUE of the path exists
has_nearth <- function(what = c('vector', 'raster')[1]){

   path <- switch(tolower(what),
      'raster' = 'NEARTH_RASTER_PATH',
      'NEARTH_VECTOR_PATH')
   NEARTH_PATH <- options(path)[[1]]
   if (is.null(NEARTH_PATH)){
      msg <- switch(tolower(what[1]),
         'raster' = 'please set options to include NEARTH_RASTER_PATH',
         'vector' = 'please set options to include NEARTH_VECTOR_PATH')
      cat(msg, "\n")
      x <- FALSE
      names(x) <- path
   } else {
      names(NEARTH_PATH) <- NEARTH_PATH
      x <- sapply(NEARTH_PATH, file.exists)
   }
   x
}


#' Retrieve the Natural Earth path
#' 
#' @export
#' @param what character, either 'vector' (default) or 'raster'
#' @return the Natural Earth path or ""
nearth_path <- function(what = c('vector', 'raster')[1]){

   ok <- has_nearth(what = what[1])
   if (!ok[1]){
      r = ""
   } else {
      r <- names(ok)[1]
   }
   r
}
#' Find the full path for a vector dataset
#'
#' @export
#' @param name character, one or more names to find
#' @param ext character the file extension to seek, by default '.shp'
#' @param path character the path to the Natural Earth vector datasets
#' @return a named character vector, missing files are returned as empty character
find_nearth_vectors <- function(name = 'ne_50m_coastline', 
   ext = '.shp',
   path = nearth_path(what = 'vector') ){
      
   stopifnot(has_nearth(what='vector'))
   pat <- glob2rx(paste0("*",name, ext))
   names(pat) <- name
   sapply(pat, 
      function(x, path="."){
         list.files(path, pattern = x, full.names = TRUE, recursive = TRUE)
      },
      path = path)
}

#' Find the full path for a raster dataset
#'
#' @export
#' @param name character, one or more names to find
#' @param ext character the file extension to seek, by default '.tif'
#' @param path character the path to the Natural Earth raster datasets
#' @return character, named logical, TRUE if the file exists
find_nearth_rasters <- function(name = 'GRAY_50M_SR_O', 
   ext = '.tif',
   path = nearth_path(what = 'raster')){
   
   stopifnot(has_nearth(what='raster'))
   pat <- glob2rx(paste0("*",name, ext))
   names(pat) <- name
   sapply(pat, 
      function(x, path="."){
         list.files(path, pattern = x, full.names = TRUE, recursive = TRUE)
      },
      path = path)
}

#' Strip the extension (e.g. ".ext") off one or more filenames
#'
#' @export
#' @param x character one or more filenames
#' @param sep character, separator between name of extension
#' @return named character vector 
strip_extension <- function(x, sep = "."){
   strip_one <- function(x, sep = '.'){
      ix <- gregexpr(sep, x[1], fixed = TRUE)[[1]]
      nix <- length(ix)
      iy <- attr(ix, "match.length")[nix]
      if ( iy > 0) x <- substring(x, 1, ix[nix] - 1)
      x
   }
   sapply(x, strip_one, sep = sep)
}


#' Read one vector data set using \code{rgdal::readOGR()}
#' 
#' @export
#' @param filename character, one file name
#' @param ... further arguments for \code{rgdal::readOGR()}
#' @return Spatial* class object or NULL
read_nearth_vector <- function(filename, ...){
   stopifnot(requireNamespace("rgdal", quietly = TRUE))
   stopifnot(has_nearth(what='vector'))
   stopifnot(file.exists(filename[1]))
   x <- try(rgdal::readOGR(dsn = dirname(filename[1]), 
      layer = strip_extension(basename(filename[1])), 
      ...))
   if (inherits(x, "try-error")){
      return(NULL)
   } else {
      return(x)
   }
}

#' Read one raster data set using \code{raster::raster()}, \code{raster::stack()}
#' or \code{raster::brick()}
#' 
#' @export
#' @param filename character, one file name
#' @param form character one of "raster", "stack", "brick" (default)
#' @param ... further arguments for \code{raster::raster()}, \code{raster::stack()}
#' or \code{raster::brick()}
#' @return Raster* class object or NULL
read_nearth_raster <- function(filename, 
   form = c("raster", "stack", "brick")[3],
   ...){
   stopifnot(requireNamespace("raster", quietly = TRUE))
   stopifnot(has_nearth(what='raster'))
   stopifnot(file.exists(filename[1]))
   x <- switch(tolower(form[1]),
      "raster" = try(raster::raster(filename[1], ...)),
      "stack"  = try(raster::stack(filename[1], ...)),
      "brick"  = try(raster::brick(filename[1], ...)) )
   if (inherits(x, "try-error")){
      return(NULL)
   } else {
      return(x)
   }
}



#' Read one or more Natural Earth data files.
#'
#' @export
#' @param name character, one or more names to find, must point to either
#'    vectors or rasters but not a mix of the two.  You can also provide the 
#'    full filepath but providing just the name can be easier.
#' @param what character either 'vector' (default) or 'raster'
#' @param ... further arguments for \code{nearth::read_nearth_raster()} or 
#' or \code{nearth::read_nearth_vector()}
read_nearth <- function(name = 'ne_50m_coastline',
   what = c('vector', 'raster')[1],
   ...){
   stopifnot(has_nearth(what=what[1]))
   
   name <- strip_extension(basename(name))
   
   X <- NULL
   if (tolower(what[1]) == "raster"){
      ff <- find_nearth_rasters(name)
      X <- lapply(ff, 
         function(x, ...){
            if (nchar(x) > 0){
               return(read_nearth_raster(x, ...))
            } else {
               return(NULL)
            } }, 
         ...)
   } else {
      ff <- find_nearth_vectors(name)
      X <- lapply(ff, 
         function(x, ...){
            if (nchar(x) > 0){
               return(read_nearth_vector(x, ...))
            } else {
               return(NULL)
            } }, 
         ...)
   }
   invisible(X)
}

