q_lapply <- function(x, FUN, ...){
  output <- lapply(x, FUN, ...)
}


derive_ext <- function(x){
  if(is.character(x)){
    tolower(x)
  }else{
  stopifnot(is.function(x))
  tools::file_ext(as.list(x)$filename)
  }
}
