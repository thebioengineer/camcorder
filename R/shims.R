#' @importFrom rlang env_bind caller_env
declare_lib_shims <- function(env = caller_env()){
  env_bind(env,
    library = shim_library,
    require = shim_require
  )
}

#' @importFrom rlang env_unbind caller_env
remove_lib_shims <- function(env = caller_env()){
  env_unbind(env, nms = c("library", "require"))
}

#' Register camcorder shims for plot recording
#'
#' This function registers S7 methods for ggplot and patchwork print methods
#' to intercept plot printing and save them automatically.
#'
#' @return Used for side effects.  Invisibly returns NULL.
#' @keywords internal
register_camcorder_shims <- function(){

  declare_lib_shims()

  # Handle ggplot2 - check if it's loaded and if it uses S7
  if ("package:ggplot2" %in% search()) {
    tryCatch({
      # Try S7 method registration first (ggplot2 >= 4.0.0)
      if (requireNamespace("S7", quietly = TRUE)) {
        # Get the ggplot class from ggplot2
        ggplot_class <- get("ggplot", envir = getNamespace("ggplot2"))
        
        # Register S7 method for print
        S7::method(print, ggplot_class) <- record_ggplot
      } else {
        # Fallback to S3 method registration for older ggplot2
        registerS3method(
          genname = "print",
          class = "ggplot",
          method = "record_ggplot",
          envir = getNamespace("camcorder")
        )
      }
    }, error = function(e) {
      # If S7 fails, try S3 as fallback
      registerS3method(
        genname = "print",
        class = "ggplot",
        method = "record_ggplot",
        envir = getNamespace("camcorder")
      )
    })
  }

  # Handle patchwork
  if ("package:patchwork" %in% search()) {
    tryCatch({
      # patchwork may also use S7 in newer versions
      if (requireNamespace("S7", quietly = TRUE)) {
        patchwork_class <- get("patchwork", envir = getNamespace("patchwork"))
        S7::method(print, patchwork_class) <- record_patchwork
      } else {
        registerS3method(
          genname = "print",
          class = "patchwork",
          method = "record_patchwork",
          envir = getNamespace("camcorder")
        )
      }
    }, error = function(e) {
      # Fallback to S3
      registerS3method(
        genname = "print",
        class = "patchwork",
        method = "record_patchwork",
        envir = getNamespace("camcorder")
      )
    })
  }

  GG_RECORDING_ENV$shims_registered <- TRUE

}

#' Detach camcorder shims
#'
#' Restores the original print methods for ggplot and patchwork objects,
#' stopping the automatic recording of plots.
#'
#' @return Used for side effects. Invisibly returns NULL.
#' @keywords internal
detach_camcorder_shims <- function(){

  if (!isTRUE(GG_RECORDING_ENV$shims_registered)) {
    return(invisible(NULL))
  }

  remove_lib_shims()

  # Restore ggplot2 print method
  if ("package:ggplot2" %in% search()) {
    tryCatch({
      # Try S7 method restoration first
      if (requireNamespace("S7", quietly = TRUE)) {
        ggplot_class <- get("ggplot", envir = getNamespace("ggplot2"))
        original_print <- get("print. ggplot", envir = getNamespace("ggplot2"))
        S7::method(print, ggplot_class) <- original_print
      } else {
        # S3 fallback
        registerS3method(
          genname = "print",
          class = "ggplot",
          method = "print.ggplot",
          envir = getNamespace("ggplot2")
        )
      }
    }, error = function(e) {
      # S3 fallback
      registerS3method(
        genname = "print",
        class = "ggplot",
        method = "print.ggplot",
        envir = getNamespace("ggplot2")
      )
    })
  }

  # Restore patchwork print method
  if ("package:patchwork" %in% search()) {
    tryCatch({
      # Try S7 method restoration
      if (requireNamespace("S7", quietly = TRUE)) {
        patchwork_class <- get("patchwork", envir = getNamespace("patchwork"))
        original_print <- get("print.patchwork", envir = getNamespace("patchwork"))
        S7::method(print, patchwork_class) <- original_print
      } else {
        # S3 fallback
        registerS3method(
          genname = "print",
          class = "patchwork",
          method = "print. patchwork",
          envir = getNamespace("patchwork")
        )
      }
    }, error = function(e) {
      # S3 fallback
      registerS3method(
        genname = "print",
        class = "patchwork",
        method = "print. patchwork",
        envir = getNamespace("patchwork")
      )
    })
  }

  GG_RECORDING_ENV$shims_registered <- FALSE

}


#' Shim library function
#'
#' Intercepts library() calls to properly manage print method registration
#' when packages are loaded/unloaded during recording.
#'
#' @param package Package name
#' @param ... Additional arguments passed to base::library()
#' @param warn.conflicts Logical, warn about conflicts
#' @param character.only Logical, package name as character
#'
#' @keywords internal
shim_library <- function(package, ..., warn.conflicts = TRUE, character.only = FALSE){

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

#' Shim require function
#'
#' Intercepts require() calls to properly manage print method registration
#' when packages are loaded/unloaded during recording.
#'
#' @param package Package name
#' @param ... Additional arguments passed to base::require()
#' @param warn.conflicts Logical, warn about conflicts
#' @param character.only Logical, package name as character
#'
#' @keywords internal
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

#' Suppress warnings for specific packages
#'
#' @param package Package name
#' @param warn.conflicts Logical, user preference for warnings
#'
#' @return Logical value for warn.conflicts parameter
#' @keywords internal
camcorder_warn_suppress <- function(package, warn.conflicts = FALSE){

  if (package %in% c("ggplot2", "patchwork")) {
    return(TRUE)
  }

  return(warn.conflicts)

}