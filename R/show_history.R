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
#'   gg_record(dir = file.path(tempdir(),"recording"), device = "png" )
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
#' @importFrom slickR slickR settings %synch%
stop_motion <- function(height = 500, dir = GG_RECORDING_ENV$recording_dir, ext = GG_RECORDING_ENV$device_ext, filename = NULL){

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

  records <- get_file_records(full_path = FALSE, path = dir, ext = ext %||% "(png)|(pdf)|(jpeg)|(bmp)|(tiff)|(emf)|(svg)|(eps)|(ps)")

  slick_content <- lapply(records, function(image_path, img_height){
    img_ext <- tools::file_ext(image_path)
    if(img_ext%in% c("tif", "emf", "eps", "ps")){
      img_tag <- htmltools::tags$img(src = image_path,
                          style = paste0(
                            "max-height:100%;",
                            "max-width:100%",
                            "width:auto;height:auto;",
                            "margin-left: auto; margin-right: auto;",
                            "vertical-align:middle;"))
    }else if( img_ext %in% c("pdf")){

      img_tag <- htmltools::tags$img(src = pdf_to_png(file.path(dir, image_path)),
                                     style = paste0(
                                       "max-height:100%;",
                                       "width:auto;height:auto;",
                                       "margin-left: auto; margin-right: auto;",
                                       "vertical-align:middle;"))
    }else{

      img_tag <- htmltools::tags$img(src = image_path,
                          style = paste0(
                            "max-height:100%;",
                            "width:auto;height:auto;",
                            "margin-left: auto; margin-right: auto;",
                            "vertical-align:middle;"))
    }
    htmltools::tags$div(
      img_tag,style = paste0("margin-left:auto;margin-right:auto;width:fit-content;height:",img_height,"px;")
    )
  }, img_height = height)

  slick_carousel <- (
    slickR::slickR(
      slick_content,
      height = height,
      width = "95%") +
      slickR::settings(infinite = FALSE)
  )

  if(is.null(filename)){
    filename <- file.path(dir, "carousel.html")
  }

  htmltools::save_html(
    slick_carousel,
    file = filename
  )

  viewer <- getOption("viewer", utils::browseURL)

  if (is.function(viewer) &&
      length(filename) > 0 && interactive()) {
    viewer(filename)
  }

  invisible()


}

pdf_to_png <- function(pdf_path, path = tempdir()){

  pdf_name <- basename(pdf_path)
  pdf_name_sans_ext <- file_path_sans_ext(pdf_name)

  png_path <- normalizePath(
    file.path(path, paste0(pdf_name_sans_ext,".png")),
    winslash = "/"
  )

  if(!file.exists(png_path)){
    pdftools::pdf_convert(
      pdf = pdf_path,
      format = 'png',
      verbose = FALSE,
      filenames = png_path
      )
  }

  return(png_path)

}
