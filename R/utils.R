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

camcorder_plot_file_path <- function(){

  timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6")
  i <- camcorder_plot_count()

  file.path(
    GG_RECORDING_ENV$recording_dir,
    paste0(
      glue(GG_RECORDING_ENV$filename_pattern),
      ".",
      GG_RECORDING_ENV$device_ext
    ))
}


camcorder_reserved_patterns <- list(
  list(pattern = "{timestamp}", regex = "\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}[.]\\d+"),
  list(pattern = "{i}", regex = "\\d+")
)

verify_filename_pattern <- function(x){
  has_pattern <- FALSE
  for(reserved_pattern in camcorder_reserved_patterns){
    has_pattern <- grepl(reserved_pattern$pattern, x, fixed = TRUE)
    if(has_pattern){
      break
    }
  }

  if(!has_pattern){
    stop(paste0(
      "filename_pattern must have one of the camcorder reserved filename",
      " patterns to ensure uniqueness:\n",
      paste0("\t`",sapply(camcorder_reserved_patterns, `[[`, "pattern"),"`", collapse = "\n")
      )
    )
  }

  x
}

camcorder_plot_count <- function(){
  i <- mget("plot_count", GG_RECORDING_ENV, ifnotfound = 0)[[1]] + 1
  set_plot_count(i)
  i
}

cleanup_filename_pattern_to_regex <- function(filename_pattern){
  for(reserved_pattern in camcorder_reserved_patterns){
    filename_pattern <- gsub(reserved_pattern$pattern,reserved_pattern$regex, filename_pattern, fixed=TRUE)
  }
  filename_pattern
}

set_plot_count <- function(i){
  GG_RECORDING_ENV$plot_count <- i
}
