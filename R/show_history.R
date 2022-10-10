#' Show previous plots as a stop motion slideshow
#'
#' Using slickjs, preview all historical plots as a image carousel using slick js via the
#' slickR package.
#'
#' slickR and htmltools must be installed for this function to operate.
#'
#' @param height Set the height of the carousel widget and images in pixels. Defaults to 500.
#' @param dir directory the saved intermediate plots are in. When able, uses the set directory from `gg_record`.
#' @param ext extension type of the saved intermediate plots to display. When able, uses the set device extention from `gg_record`.
#'
#' @returns Returns a slickR htmlwidget populated with the plots
#'
#' @examples
#'
#' if(require(ggplot2) & interactive()){
#'
#'   gg_record(dir = file.path(tempdir(),"recording"))
#'   ggplot(data.frame(x = 1, y = 1), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'   ggplot(data.frame(x = 1, y = 2), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'
#'   ## resize canvas of the last plot
#'   gg_resize_film(height = 10, width = 5, dpi = 350)
#'
#'   ggplot(data.frame(x = 1, y = 3), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'
#'   stop_motion()
#' }
#'
#'
#' @importFrom rlang check_installed
stop_motion <- function(height = 500, dir = GG_RECORDING_ENV$recording_dir, ext = GG_RECORDING_ENV$device_ext){

  if(is.null(dir)){
    stop("Set `dir` to the directory where the intermediate plots are saved in.")
  }

  check_installed("slickR", reason = "To generate the stop_motion, slickR must be installed")
  check_installed("htmltools", reason = "To generate the stop_motion, htmltools must be installed")

  stopifnot(is.numeric(height))
  stopifnot(height > 1)


  if(is.null(ext)){
    warning("No file extension set. All allowable image file extentions will be used")
  }

  records <- get_file_records(full_path = TRUE, path = dir, ext = ext %||% "(png)|(pdf)|(jpeg)|(bmp)|(tiff)|(emf)|(svg)|(eps)|(ps)")

  slick_content <- lapply(records, function(image_path, img_height){
    b64 <- read_image_b64(image_path)
    htmltools::tags$div(
      htmltools::tags$img(src = b64,
                          style = paste0(
                            "max-height:100%;",
                            "width:auto;height:auto;",
                            "margin-left: auto; margin-right: auto;",
                            "vertical-align:middle;")
                          )
    ,
    style = paste0("margin-left:auto;margin-right:auto;width:fit-content;height:",img_height,"px;")
    )
  }, img_height = height)

  slick_carousel <- slickR::slickR(slick_content, height = height, width = "95%") +
    slickR::settings(
      slidesToShow = 1,
      infinite = TRUE
  )

  return(slick_carousel)
}
