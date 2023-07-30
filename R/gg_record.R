#' @title Record and generate plot histories
#'
#' @description Record plots created over time and generate a GIF of the plots
#'    made in the 'R' session. Overrides the print methods for ggplot, patchwork, and gt_tbl objects
#'    from the 'ggplot2', 'patchwork', and 'gt' packages respectively.
#'
#' @rdname Recording
#'
#' @param dir directory to save the intermediate plots in. Defaults to a temporary directory
#' @param device Device to use. Can either be a device function (e.g. png()), or
#'     one of "png", "pdf", "jpeg", "bmp", "tiff", "emf", "svg", "eps", "ps".
#' @param device_ext file extension to use for images created. Does not usually need to be populated manually.
#' @return Used initialize recording, nothing returned
#' @inheritParams ggplot2::ggsave
#' @inheritParams webshot2::webshot
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
                      device = c("png", "pdf", "jpeg", "bmp", "tiff", "emf", "svg", "eps", "ps"),
                      scale = 1,
                      width = NA,
                      height = NA,
                      units = c("in", "cm", "mm","px"),
                      dpi = 300,
                      limitsize = TRUE,
                      device_ext = NULL,
                      bg = NULL,
                      expand = 5,
                      zoom = 2
){

  if (is.null(dir)) {
    is_temp_dir <- TRUE
    dir <- tempdir()
  } else{
    is_temp_dir <- FALSE
  }

  if(!is.function(device)){
    device <- tolower(device)
    device <- match.arg(device)
  }else{
    device_alt <- substitute(device)
    device_alt <- tolower(device_alt)
    device <- if(device_alt%in% c("png", "pdf", "jpeg", "bmp", "tiff", "emf", "svg", "eps", "ps")){
      device_alt
    }else{
      device
    }
  }

  if(is.null(device_ext)){
    device_ext <- derive_ext(device)
  }

  units <- match.arg(units)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  } else{
    if (length(list.files(dir, pattern = paste0("[.]", device_ext, "$"))) > 1) {
      warning(
        "Writing to a folder that already exists. gg_playback may use more files than intended!"
      )
    }
  }

  GG_RECORDING_ENV$recording_dir <- dir
  GG_RECORDING_ENV$device        <- device
  GG_RECORDING_ENV$device_ext    <- device_ext
  GG_RECORDING_ENV$is_temp_dir   <- is_temp_dir

  GG_RECORDING_ENV$image_width  <- width
  GG_RECORDING_ENV$image_height <- height
  GG_RECORDING_ENV$image_units  <- units
  GG_RECORDING_ENV$image_dpi    <- dpi
  GG_RECORDING_ENV$scale        <- scale
  GG_RECORDING_ENV$bg           <- bg
  GG_RECORDING_ENV$limitsize    <- limitsize

  GG_RECORDING_ENV$expand       <- expand
  GG_RECORDING_ENV$zoom         <- zoom

  GG_RECORDING_ENV$shims_registered <- FALSE

  register_camcorder_shims()

  invisible()
}


#' @describeIn Recording
#'
#' @param name name of gif.
#' @param first_image_duration n units of frame_duration to show the first image for.
#' @param last_image_duration n units of frame_duration to show the last image for.
#' @param frame_duration n seconds each plot should be shown.
#' @param image_resize size to rescale images to in pixels.
#' @param background color to set the background of the gif. A valid color string such as "navyblue" or
#' "#000080". Use "none" for transparency. Does not impact the background of images.
#' @param playback Boolean, should the recording start playing after it is
#' turned into a gif? defaults to TRUE.
#' @param stoprecording Boolean, should the plots stop being recorded?
#' defaults to TRUE.
#'
#' @param last_as_first Should the last plot be displayed at the beginning too?
#'
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
           stoprecording = FALSE,
           last_as_first = TRUE,
           ...) {

    records <- get_file_records(full_path = TRUE)

    if (length(records) == 0) {
      warning("No images recorded to playback.")
      invisible()
    }

    stopifnot(last_image_duration > 0)
    stopifnot(first_image_duration > 0)

    records <-
      scale_film(film = records,
                 cassette = file.path(GG_RECORDING_ENV$recording_dir, "resized"),
                 size = image_resize,
                 background = background)

    if(last_as_first){
      records <- c(records[length(records)], records)
    }

    records <- c(
      rep(records[1], times = first_image_duration),
      records[-c(1, length(records))],
      rep(records[length(records)], times = last_image_duration)
    )

    if (is.null(name)) {
      recording <- paste0(format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".gif")
      recording <- file.path(GG_RECORDING_ENV$recording_dir, recording)
    } else{
      recording <- name
    }

    ## make gif via gifski
    gifski(
      png_files = records,
      gif_file = recording,
      delay = frame_duration,
      width = ifelse(is.null(width), image_resize, width),
      height = ifelse(is.null(height), image_resize, height),
      progress = progress
    )

    viewer <- getOption("viewer", utils::browseURL)

    if (is.function(viewer) &&
        length(recording) > 0 && playback && interactive()) {
      viewer(recording)
    }

    ## revert ggplot printing to standard printing
    if (stoprecording) {
      detach_camcorder_shims()
    }

    invisible(name)

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
gg_resize_film <- function(height = NA, width = NA, units = NA, dpi = NA, scale = NA, expand = NA, zoom = NA){

  if(!is.na(height)){
    GG_RECORDING_ENV$image_height <- height
  }
  if(!is.na(width)){
    GG_RECORDING_ENV$image_width <- width
  }
  if(!is.na(units)){
    units <- match.arg(units,choices = c("in", "cm", "mm","px"))
    GG_RECORDING_ENV$image_units <- units
  }
  if(!is.na(dpi)){
    GG_RECORDING_ENV$image_dpi <- dpi
  }
  if(!is.na(scale)){
    GG_RECORDING_ENV$scale <- scale
  }

  if(!is.na(expand)){
    GG_RECORDING_ENV$expand <- expand
  }
  if(!is.na(zoom)){
    GG_RECORDING_ENV$zoom <- zoom
  }

  print(GG_RECORDING_ENV$last_plot)
  invisible()
}

#' @describeIn Recording
#'
#' @description Stop recording images with {camcorder}.
#'
#' @export
#'
#' @return Returns nothing. used for side effect.
gg_stop_recording <- function(){
  detach_camcorder_shims()
}


GG_RECORDING_ENV <- new.env()

