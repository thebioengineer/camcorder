q_lapply <- function(x, FUN, ...){
  output <- lapply(x, FUN, ...)
}


derive_ext <- function(x){
  if(is.character(x)){
    c(
      "png" = "png",
      "pdf" = "pdf",
      "jpeg" = "jpg",
      "bmp" = "bmp",
      "tiff" = "tif",
      "emf" = "emf",
      "svg" = "svg",
      "eps" = "eps",
      "ps" = "ps"
    )[[tolower(x)]]
  }else{
    stopifnot(is.function(x))
    x_sub <- format(substitute(x))
    if (any(x_sub %in% c(
      "png",
      "pdf",
      "jpeg",
      "bmp",
      "tiff",
      "emf",
      "svg",
      "eps",
      "postscript"
    ))) {

      return(
        c(
          "png" = "png",
          "pdf" = "pdf",
          "jpeg" = "jpg",
          "bmp" = "bmp",
          "tiff" = "tif",
          "emf" = "emf",
          "svg" = "svg",
          "eps" = "eps",
          "postscript" = "ps"
        )[[x_sub[1]]]
      )
    }else{
      tryCatch({
        setdiff(unique(tools::file_ext(as.list(x)$filename)),"")[[1]]
      }, error = function(e){
        stop(paste(
          "Unable to determine file extention to use for `",substitute(x),"`.",
          "Please provide a file extension in the `device_ext` argument of gg_record()."
        ))
      })
    }
  }
}
