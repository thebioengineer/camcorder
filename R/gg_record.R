#' @title Record and generate plot histories
#'
#' @description Record plots created over time and generate a gif of the plots
#'    made in the R session.
#'
#' @rdname Recording
#'
#' @param dir directory to save the intermediate plots in
#' @return Used initialize recording, nothing returned
#' @inheritParams ggplot2::ggsave
#'
#' @importFrom ggplot2 ggsave
#'
#' @examples
#'  if(require(ggplot2) & interactive()){
#'   gg_record(dir = file.path(tempdir(),"recording"))
#'   ggplot(data.frame(x = 1, y = 1), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'   ggplot(data.frame(x = 1, y = 2), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'
#'   ## resize canvas of the last plot
#'   gg_resize_film(height = 10, width = 5, dpi = 350)
#'
#'   ggplot(data.frame(x = 1, y = 3), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'
#'   gg_playback(tempfile(fileext = ".gif"))
#' }
#'
#' @export

gg_record <- function(dir = NULL,
                      device = c("png", "pdf", "jpeg", "bmp", "tiff", "emf", "svg", "eps", "ps", "bmp"),
                      scale = 1,
                      width = NA,
                      height = NA,
                      units = c("in", "cm", "mm"),
                      dpi = 300,
                      limitsize = TRUE
){


  if (is.null(dir)) {
    is_temp_dir <- TRUE
    dir <- tempdir()
  } else{
    is_temp_dir <- FALSE
  }

  device <- match.arg(device)
  units <- match.arg(units)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  } else{
    if (length(list.files(dir, pattern = paste0("[.]", device, "$"))) > 1) {
      warning(
        "Writing to a folder that already exists. gg_playback may use more files than intended!"
      )
    }
  }



  GG_RECORDING_ENV$recording_dir <- dir
  GG_RECORDING_ENV$device        <- device
  GG_RECORDING_ENV$is_temp_dir   <- is_temp_dir

  GG_RECORDING_ENV$image_width  <- width
  GG_RECORDING_ENV$image_height <- height
  GG_RECORDING_ENV$image_units  <- units
  GG_RECORDING_ENV$image_dpi    <- dpi

  ggplot2_print <- get("print.ggplot", envir = asNamespace("ggplot2"), inherits = FALSE)

  ## create shim function

  GG_RECORDING_ENV$print.view_and_save_ggplot <-
    function(x,
             newpage = is.null(vp),
             vp = NULL,
             ...) {

      plot_file <-
        file.path(dir, paste0(format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6"), ".", device))

      suppressMessages({
        ggplot2::ggsave(
          filename = plot_file,
          plot = x,
          scale = scale,
          width = GG_RECORDING_ENV$image_width,
          height = GG_RECORDING_ENV$image_height,
          units = GG_RECORDING_ENV$image_units,
          dpi = GG_RECORDING_ENV$image_dpi,
          limitsize = limitsize
        )
      })

      # View plot
      # ggplot2_print(x, newpage = newpage, vp = vp, ...)
      preview_film()

      GG_RECORDING_ENV$last_plot <- x

    }

  registerS3method(
    genname = "print",
    class = "ggplot",
    method = "print.view_and_save_ggplot",
    envir = GG_RECORDING_ENV
  )
}


#' @describeIn Recording
#'
#' @param name name of gif
#' @param first_image_duration n units of frame_duration to show the last image for
#' @param last_image_duration n units of frame_duration to show the last image for
#' @param frame_duration n seconds each plot should be shown
#' @param image_resize size to rescale images to in pixels
#' @param background color to set the background. A valid color string such as "navyblue" or
#' "#000080". Use "none" for transparancy.
#' @param playback Boolean, should the recording start playing after it is
#' turned into a gif? defaults to TRUE
#' @param stoprecording Boolean, should the plots stop being recorded?
#' defaults to TRUE.
#' @inheritParams gifski::gifski
#'
#' @return Returns nothing. Used to generate the gif.
#' @importFrom gifski gifski
#' @export

gg_playback <-
  function(name = NULL,
           first_image_duration = 16,
           last_image_duration = 20,
           frame_duration = .25,
           loop = TRUE,
           image_resize = 600,
           background = "black",
           width = NULL,
           height = NULL,
           progress = interactive(),
           playback = TRUE,
           stoprecording = TRUE) {
    records <- list.files(
      path    = GG_RECORDING_ENV$recording_dir,
      pattern = paste0("*.", GG_RECORDING_ENV$device, "$"),
      full.names = TRUE
    )

    if (length(records) == 0) {
      warning("No images recorded to playback.")
    } else{
      stopifnot(last_image_duration > 0)
      stopifnot(first_image_duration > 0)

      records <-
        scale_film(film = records,
                   size = image_resize,
                   background = background)

      records <- c(
        rep(records[1], times = first_image_duration),
        records[-c(1, length(records))],
        rep(records[length(records)], times = last_image_duration)
      )

      if (is.null(name)) {
        recording <- paste0(format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".gif")
        if (!GG_RECORDING_ENV$is_temp_dir) {
          recording <- file.path(GG_RECORDING_ENV$recording_dir, recording)
        }
      } else{
        recording <- name
      }

      ## make gif via gifski
      gifski(
        png_files = records,
        gif_file = recording,
        delay = frame_duration,
        width = ifelse(is.null(width), image_resize, width),
        height = ifelse(is.null(height), image_resize, width),
        progress = progress
      )

      viewer <- getOption("viewer", utils::browseURL)

      if (is.function(viewer) &&
          length(recording) > 0 && playback && interactive()) {
        viewer(recording)
      }
    }
    ## revert ggplot printing to standard printing
    if (stoprecording) {
      registerS3method(
        genname = "print",
        class = "ggplot",
        method = "print.ggplot",
        envir = getNamespace("ggplot2")
      )

    }

    invisible()

  }

#' @describeIn Recording
#'
#' @description resize the film for recording, reprints and saves last plot
#'
#' @inheritParams ggplot2::ggsave
#' @export
#'
#' @return Returns the last plot generated, resized to new dimensions
#'
gg_resize_film <- function(height = NA, width = NA, units = NA, dpi = NA){

  if(!is.na(height)){
    GG_RECORDING_ENV$image_height <- height
  }
  if(!is.na(width)){
    GG_RECORDING_ENV$image_width <- width
  }
  if(!is.na(units)){
    GG_RECORDING_ENV$image_units <- units
  }
  if(!is.na(dpi)){
    GG_RECORDING_ENV$image_dpi <- dpi
  }

  print(GG_RECORDING_ENV$last_plot)
  invisible()
}

GG_RECORDING_ENV <- new.env()
