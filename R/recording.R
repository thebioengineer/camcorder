#' Record Plots
#'
#' @description Functions that do the "recording" ie saving and then
#' previewing output via \link{\code{preview_film()}}
#'
#' @param x plot to save
#' @param ... allow for traditionally pass arguments to printing that are ignored
#'
#' @importFrom ggplot2 set_last_plot
#'
#' @noRd
#'
record_ggplot <- function(x, ...) {

  plot_file <-
    file.path(GG_RECORDING_ENV$recording_dir, paste0(
      format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6"),
      ".",
      GG_RECORDING_ENV$device_ext
    ))

  suppressMessages({
    ggsave(
      filename = plot_file,
      plot = x,
      device = GG_RECORDING_ENV$device,
      scale = GG_RECORDING_ENV$scale,
      width = GG_RECORDING_ENV$image_width,
      height = GG_RECORDING_ENV$image_height,
      units = GG_RECORDING_ENV$image_units,
      dpi = GG_RECORDING_ENV$image_dpi,
      limitsize = GG_RECORDING_ENV$limitsize,
      bg = GG_RECORDING_ENV$bg
    )
  })

  preview_film()

  set_last_plot(x)
  GG_RECORDING_ENV$last_plot <- x

}

#' @importFrom utils capture.output
record_patchwork <- function(x,...) {

    plot_file <-
      file.path(GG_RECORDING_ENV$recording_dir, paste0(
        format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6"),
        ".",
        GG_RECORDING_ENV$device_ext
      ))

    registerS3method(
      genname = "print",
      class = "patchwork",
      method = "print.patchwork",
      envir = getNamespace("patchwork")
    )

    on.exit({
      registerS3method(
        genname = "print",
        class = "patchwork",
        method = "record_patchwork",
        envir = getNamespace("camcorder")
      )
    })

    suppressMessages({
      ggsave(
        filename = plot_file,
        plot = x,
        device = GG_RECORDING_ENV$device,
        scale = GG_RECORDING_ENV$scale,
        width = GG_RECORDING_ENV$image_width,
        height = GG_RECORDING_ENV$image_height,
        units = GG_RECORDING_ENV$image_units,
        dpi = GG_RECORDING_ENV$image_dpi,
        limitsize = GG_RECORDING_ENV$limitsize,
        bg = GG_RECORDING_ENV$bg
      )
    })

    preview_film()

    set_last_plot(x)
    GG_RECORDING_ENV$last_plot <- x

}

#' Record gt tables
#'
#' @description Record gt tables as png using webshot2.
#'
#' @param x gt table to save
#' @param ... allow for traditionally pass arguments to printing that are ignored
#'
#' @noRd
#'
record_gt <- function(x, ...) {

  rlang::check_installed("webshot2", reason = "to record gt tables")

  table_dims <- dim(x[["_data"]])
  if (GG_RECORDING_ENV$limitsize && (table_dims[1] > 100 || table_dims[2] > 30)) {
    rlang::abort(c(
      "Table dimensions exceed 100x30",
      i = "Render a subset with `gt_preview()` or use `limitsize = FALSE`"
    ))
  }

  plot_files <-
    file.path(GG_RECORDING_ENV$recording_dir, paste0(
      format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6"),
      ".",
      c("html", "png") # webshot() only supports png for raster
    ))

  # Convert to pixel for webshot()
  as_pixel <- function(x) {
    if (is.na(x)) {
      return(NULL)
    }
    ratio <- switch(
      GG_RECORDING_ENV$image_units,
      "cm" = 1/2.54,
      "mm" = 1/25.4,
      "px" = 1,
      "in" = 1
    )
    dpi_scaling <- if (GG_RECORDING_ENV$image_units == "px") {
      1
    } else {
      GG_RECORDING_ENV$image_dpi
    }
    round(x * ratio * dpi_scaling)
  }

  suppressMessages({
    gt::gtsave(data = x, filename = plot_files[1])
    # Doesn't suppress webshot() messages
    # - known issue: https://github.com/rstudio/webshot2/issues/24
    webshot2::webshot(
      url = plot_files[1],
      file = plot_files[2],
      vwidth = as_pixel(GG_RECORDING_ENV$image_width) %||% formals(webshot2::webshot)$vwidth,
      vheight = as_pixel(GG_RECORDING_ENV$image_height) %||% formals(webshot2::webshot)$vheight,
      selector = "table",
      expand = GG_RECORDING_ENV$expand,
      zoom = GG_RECORDING_ENV$zoom
    )
  })

  preview_film()

  GG_RECORDING_ENV$last_plot <- x

}

#' Record Plots - generic
#'
#' @description For plot types that don't have a special print method, use this
#' function to capture what has been printed to the current graphics device and
#' save it using the current camcorder settings
#'
#' @returns No return value. Used for the side effect of capturing the current
#'   graphics device and saving it to the set directory from gg_record.
#'
#' @examples
#'
#' library(grid)
#'
#' gg_record(device = "png", width = 10, height = 8, units = "in", dpi = 320)
#'
#' ## make a plot using grobs
#' grid.draw(rectGrob(width = 2, height = 2, gp = gpar(fill = "green")))
#' grid.draw(textGrob("Hello world"))
#'
#' record_polaroid()
#'
#' gg_stop_recording()
#'
#' @importFrom grDevices dev.cur dev.copy dev.set dev.off
#' @importFrom utils capture.output
#'
#' @export
record_polaroid <- function(){

  plot_file <-
    file.path(GG_RECORDING_ENV$recording_dir, paste0(
      format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6"),
      ".",
      GG_RECORDING_ENV$device_ext
    ))

  suppressMessages({

    dev <- plot_dev(GG_RECORDING_ENV$device, plot_file, dpi = GG_RECORDING_ENV$image_dpi)

    dim <- plot_dim(
      c(GG_RECORDING_ENV$image_width, GG_RECORDING_ENV$image_height),
      scale = GG_RECORDING_ENV$scale,
      units = GG_RECORDING_ENV$image_units,
      limitsize = GG_RECORDING_ENV$limitsize,
      dpi = GG_RECORDING_ENV$image_dpi
    )

    capture.output({

      old_dev <- dev.cur()

      dev.copy(dev,
               filename = plot_file,
               width = dim[1],
               height = dim[2],
               bg = GG_RECORDING_ENV$bg %||% "transparent"
               )

      dev.off(which = )
      if (old_dev > 1){
        dev.set(old_dev)
      }

    })
  })

  preview_film()

}

`%||%`<- function(x,y){
  if(is.null(x)){
    y
  }else{
    x
  }
}

# copied from ggplot2 internal plot_dim function
#' @importFrom grDevices dev.size dev.list

plot_dim <- function(dim = c(NA, NA),
                     scale = 1,
                     units = c("in", "cm","mm", "px"),
                     limitsize = TRUE,
                     dpi = 300) {

  units <- match.arg(units)
  to_inches <- function(x) x/c(`in` = 1, cm = 2.54, mm = 2.54 *10, px = dpi)[units]
  from_inches <- function(x) x * c(`in` = 1, cm = 2.54,mm = 2.54 * 10, px = dpi)[units]
  dim <- to_inches(dim) * scale

  if (any(is.na(dim))) {
    if (length(grDevices::dev.list()) == 0) {
      default_dim <- c(7, 7)
    } else {
      default_dim <- grDevices::dev.size() * scale
    }
    dim[is.na(dim)] <- default_dim[is.na(dim)]
    dim_f <- prettyNum(from_inches(dim), digits = 3)
    message("Saving ", dim_f[1], " x ", dim_f[2],
            " ", units, " image")
  }

  if (limitsize && any(dim >= 50)) {
    stop(paste0("\n      Dimensions exceed 50 inches (height and width are specified in '{",units,"}' not pixels).\n      If you're sure you want a plot that big, use `limitsize = FALSE`.\n    "))
  }
  dim
}

# copied from ggplot2 internal plot_dev function
#' @importFrom svglite svglite
#' @importFrom tools file_ext
#' @importFrom utils modifyList
plot_dev <- function (device, filename = NULL, dpi = 300) {

  force(filename)
  force(dpi)

  if (is.function(device)) {
    args <- formals(device)
    call_args <- list()
    if ("file" %in% names(args)) {
      call_args$file <- filename
    }
    if ("res" %in% names(args)) {
      call_args$res <- dpi
    }
    if ("units" %in% names(args)) {
      call_args$units <- "in"
    }
    dev <- function(...) do.call(device, modifyList(list(...),
                                                     call_args))
    return(dev)
  }

  eps <- function(filename, ...) {
    grDevices::postscript(file = filename, ..., onefile = FALSE,
                          horizontal = FALSE, paper = "special")
  }

  if (requireNamespace("ragg", quietly = TRUE)) {
    png_dev <-  pass_bg(ragg::agg_png)
    jpeg_dev <- pass_bg(ragg::agg_jpeg)
    tiff_dev <- pass_bg(ragg::agg_tiff)
  } else {
    png_dev <- grDevices::png
    jpeg_dev <- grDevices::jpeg
    tiff_dev <- grDevices::tiff
  }

  devices <- list(eps = eps, ps = eps,
                  tex = function(filename, ...) grDevices::pictex(file = filename, ...),
                  pdf = function(filename, ..., version = "1.4") grDevices::pdf(file = filename, ..., version = version),
                  svg = function(filename, ...) svglite::svglite(file = filename, ...),
                  emf = function(..., bg = NULL) grDevices::win.metafile(...),
                  wmf = function(..., bg = NULL) grDevices::win.metafile(...),
                  png = function(...) png_dev(..., res = dpi, units = "in"),
                  jpg = function(...) jpeg_dev(..., res = dpi, units = "in"),
                  jpeg = function(...) jpeg_dev(..., res = dpi, units = "in"),
                  bmp = function(...) grDevices::bmp(..., res = dpi, units = "in"),
                  tiff = function(...) tiff_dev(..., res = dpi, units = "in"))

  device <- tolower(tools::file_ext(filename))

  if (!is.character(device) || length(device) != 1) {
    stop("`device` must be NULL, a string or a function.")
  }
  dev <- devices[[device]]
  if (is.null(dev)) {
    stop(paste0("Unknown graphics device '",device,"'"))
  }
  dev
}

pass_bg <- function(device){
  function(...,bg){
    if(!is.null(bg)){
      device(..., bg = bg)
    }else{
      device(...)
    }
  }
}
