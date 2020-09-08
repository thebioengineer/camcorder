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
      GG_RECORDING_ENV$device
    ))

  suppressMessages({
    ggsave(
      filename = plot_file,
      plot = x,
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

record_patchwork <- function(x,...) {

    plot_file <-
      file.path(GG_RECORDING_ENV$recording_dir, paste0(
        format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6"),
        ".",
        GG_RECORDING_ENV$device
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

