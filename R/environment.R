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

#source('AnnotatorTool.R')
#source('AnnotatorTool-seds.R')
#source('add.metadata.R')

get_organisms <- function() {
  pkg.env$organisms
}

get_my_tempdir <- function() {
  file.path(tempdir(), 'photoann')
}

initialize <- function() {
  dir.create(get_my_tempdir())
}
