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
      limitsize = GG_RECORDING_ENV$limitsize
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

    z <- capture.output({

    })

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
        limitsize = GG_RECORDING_ENV$limitsize
      )
    })

    preview_film()

    set_last_plot(x)
    GG_RECORDING_ENV$last_plot <- x

}

#' Record Plots - generic
#'
#' @description For plot types that don't have a special print method, use this
#' function to capture what has been printed to the current graphics device and
#' save it using the current camcorder settings
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
      GG_RECORDING_ENV$device
    ))

  dev <- plot_dev(plot_file, dpi = GG_RECORDING_ENV$image_dpi)

  dim <- plot_dim(
    c(GG_RECORDING_ENV$image_width, GG_RECORDING_ENV$image_height),
    scale = GG_RECORDING_ENV$scale,
    units = GG_RECORDING_ENV$image_units,
    limitsize = GG_RECORDING_ENV$limitsize,
    dpi = GG_RECORDING_ENV$image_dpi
  )

  suppressMessages({
    capture.output({
      old_dev <- dev.cur()

      dev.copy(dev,
               filename = plot_file,
               width = dim[1],
               height = dim[2])

      dev.off()
      if (old_dev > 1)
        dev.set(old_dev)
    })
  })

  preview_film()

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
#' @importFrom grDevices postscript png jpeg tiff pictex pdf win.metafile bmp
#' @importFrom svglite svglite
#' @importFrom tools file_ext
plot_dev <- function (filename = NULL, dpi = 300) {
  force(filename)
  force(dpi)

  eps <- function(filename, ...) {
    grDevices::postscript(file = filename, ..., onefile = FALSE,
                          horizontal = FALSE, paper = "special")
  }

  if (requireNamespace("ragg", quietly = TRUE)) {
    png_dev <-  ragg::agg_png
    jpeg_dev <- ragg::agg_jpeg
    tiff_dev <- ragg::agg_tiff
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

