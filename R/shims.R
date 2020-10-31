print_envs <- new.env()


declare_shims <- function(){
  assign(
    "library",
    get("shim_library",envir = asNamespace("camcorder")),
    envir = print_envs
  )
  assign(
    "require",
    get("shim_require",envir = getNamespace("camcorder")),
    envir = print_envs
  )

}

register_camcorder_shims <- function(){
  if(length(ls(print_envs)) == 0){
    declare_shims()
  }
  suppressMessages({
    attach(print_envs, name = ".camcorder")
  })

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

}

detach_camcorder_shims <- function(){
  detach(".camcorder")

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




