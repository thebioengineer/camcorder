
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

    if(ggplot2_is_s7()){
      ## ggplot2 >= 4.0.0 plot objects are S7, with class vector
      ## c("ggplot2::ggplot", "ggplot", ...). S3 dispatch finds ggplot2's
      ## method for "ggplot2::ggplot" before any method for "ggplot", so the
      ## shim must be registered on the qualified class name. Keep a copy of
      ## ggplot2's own method so it can be restored on detach.
      if(is.null(GG_RECORDING_ENV$ggplot2_print)){
        GG_RECORDING_ENV$ggplot2_print <-
          utils::getS3method("print", "ggplot2::ggplot")
      }
      registerS3method(
        genname = "print",
        class = "ggplot2::ggplot",
        method = record_ggplot,
        envir = getNamespace("camcorder")
      )
    }else{
      registerS3method(
        genname = "print",
        class = "ggplot",
        method = "record_ggplot",
        envir = getNamespace("camcorder")
      )
    }
  }

  if("package:patchwork" %in% search()){
    registerS3method(
      genname = "print",
      class = "patchwork",
      method = "record_patchwork",
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
    if(ggplot2_is_s7()){
      ## print.ggplot no longer exists in the ggplot2 namespace in
      ## ggplot2 >= 4.0.0; restore the method object captured at registration
      if(!is.null(GG_RECORDING_ENV$ggplot2_print)){
        registerS3method(
          genname = "print",
          class = "ggplot2::ggplot",
          method = GG_RECORDING_ENV$ggplot2_print,
          envir = getNamespace("ggplot2")
        )
      }
    }else{
      registerS3method(
        genname = "print",
        class = "ggplot",
        method = "print.ggplot",
        envir = getNamespace("ggplot2")
      )
    }
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

## ggplot2 4.0.0 rewrote its object system from S3 to S7,
## changing the class that print dispatches on
ggplot2_is_s7 <- function(){
  utils::packageVersion("ggplot2") >= "4.0.0"
}




