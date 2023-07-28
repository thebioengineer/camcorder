
#' @importFrom rlang env_bind caller_env
declare_lib_shims <- function(env = caller_env()){
  env_bind(env,
    library = shim_library,
    require = shim_require
  )
}

#' @importFrom rlang env_unbind caller_env
remove_lib_shims <- function(env = caller_env()){
  env_unbind(  env, nms = c( "library", "require") )
}

register_camcorder_shims <- function(){

  declare_lib_shims()

  if("package:ggplot2" %in% search()){
    registerS3method(
      genname = "print",
      class = "ggplot",
      method = "record_ggplot",
      envir = getNamespace("camcorder")
    )
  }

  if("package:patchwork" %in% search()){
    registerS3method(
      genname = "print",
      class = "patchwork",
      method = "record_patchwork",
      envir = getNamespace("camcorder")
    )
  }

  if("package:gt" %in% search()){
    registerS3method(
      genname = "print",
      class = "gt_tbl",
      method = "record_gt",
      envir = getNamespace("camcorder")
    )
  }

  GG_RECORDING_ENV$shims_registered <- TRUE

}

detach_camcorder_shims <- function(){

  if(!is.null(GG_RECORDING_ENV$shims_registered) &
     isTRUE(GG_RECORDING_ENV$shims_registered)){
    remove_lib_shims()
  }

  if("package:ggplot2" %in% search()){
    registerS3method(
      genname = "print",
      class = "ggplot",
      method = "print.ggplot",
      envir = getNamespace("ggplot2")
    )
  }

  if("package:patchwork" %in% search()){
    registerS3method(
      genname = "print",
      class = "patchwork",
      method = "print.patchwork",
      envir = getNamespace("patchwork")
    )
  }

  GG_RECORDING_ENV$shims_registered <- FALSE

}


shim_library <- function(package, ..., warn.conflicts = TRUE,character.only = FALSE){

  package <- as.character(substitute(package))

  detach_camcorder_shims()
  on.exit(register_camcorder_shims())

  base::library(
    package = package,
    character.only = TRUE,
    warn.conflicts = camcorder_warn_suppress(package, warn.conflicts),
    ...
  )
}

shim_require <- function(package, ..., warn.conflicts = TRUE, character.only = FALSE){

  package <- as.character(substitute(package))

  detach_camcorder_shims()
  on.exit(register_camcorder_shims())

  base::require(
    package = package,
    character.only = TRUE,
    warn.conflicts = camcorder_warn_suppress(package, warn.conflicts),
    ...
  )
}

camcorder_warn_suppress <- function(package, warn.conflicts = FALSE){

  if(package %in% c("ggplot2","patchwork")){
    return(TRUE)
  }

  return(warn.conflicts)

}




