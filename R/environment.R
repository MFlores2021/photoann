pkg.env <- new.env()

pkg.env$organisms <- 
  c('plant',
    'flower',
    'flower dissection',
    'fruit',
    'seeds',
    'tuber',
    'root',
    'sprout',
    'herbarium',
    'habitat',
    'foliage',
    'chips')

get_organisms <- function() {
  pkg.env$organisms
}

get_my_tempdir <- function() {
  file.path(tempdir(), 'photoann')
}

#' get_package_root
#'
#' Returns the install location of the currently loaded package.
#'
#' @return string The directory of this package
#' @author Alexander Vowinkel
#' @export 
#' 
get_package_root <- function() {
  pck_data <- installed.packages()[, c("Package", "Version", "LibPath")]
  pck <- pck_data[pck_data[,1] == 'photoann']
  if(!is.null(dim(pck))) {
    pck_version <- sessionInfo()[[6]][['photoann']][['Version']]
    pck <- pck[pck[,2] == pck_version,]
    pck <- pck[1,]
  }
  file.path(pck[3], 'photoann')
}

#' get_www_dir
#'
#' Returns the location of the www ressources for shiny.
#'
#' @param file optional relative file in www dir to append
#' @return string Location of the www ressources for shiny
#' @author Alexander Vowinkel
#' @export 
#' 
get_www_dir <- function(file = NA) {
  path <- file.path(get_package_root(), 'www')
  if(!is.na(file)) path <- file.path(path, file)
  path
}

initialize <- function() {
  if(!dir.exists(get_my_tempdir())) dir.create(get_my_tempdir())
  
  if(.Platform$OS.type == "windows") {
    .Platform$file.sep = '\\'
  }
}
