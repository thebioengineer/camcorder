
#' Resize images to a square
#'
#' Resize an image to equal height and width, maintaining the aspect ratio by filling the outside
#' with a defined color
#'
#' @param image path to image
#' @param output path to save resized image
#' @param max_size set maximum height/width of the image in pixels
#' @param background color to set the background. A valid color string such as "navyblue" or
#' "#000080". Use "none" for transparancy.
#' @return Nothing. Used to resize images on the file system.
#'
#' @importFrom magick image_read image_info image_resize image_extent image_write
#' @importFrom tools file_path_sans_ext file_ext
#' @import pdftools
#' @import rsvg
#'
#' @noRd
#'
resize_image <- function(image, path, max_size = 600, background = "black") {

  if (missing(path)) {
    base_image_name <- tools::file_path_sans_ext(basename(image))
    image_ext <- tools::file_ext(image)
    path <- file.path(dirname(image),
                      paste0(
                        paste(
                          base_image_name,
                          "resize",
                          paste0(max_size, "x", max_size),
                          sep = "_"
                        ),
                        ".",
                        "png"
                      ))
  }

  image_ext <- tools::file_ext(image)
  image_out_ext <- tools::file_ext(path)

  if (image_ext %in% c("pdf", "svg")) {
    if (image_ext == "pdf") { image <- magick::image_read_pdf(image) }
    if (image_ext == "svg") { image <- magick::image_read_svg(image) }
  } else  {
    image <- magick::image_read(image)
  }

  image_width <- magick::image_info(image)$width
  image_height <- magick::image_info(image)$height

  # image_resize should give better results(?) but slower than image_scale
  # http://www.imagemagick.org/Usage/resize/

  if (image_width >= image_height) {
    resized_image <- magick::image_resize(image, max_size)
  } else {
    resized_image <- magick::image_resize(image, paste0("x", max_size))
  }

  resized_image <- magick::image_extent(resized_image, paste0(max_size, "x", max_size), color = background)

  if(image_ext != image_out_ext){
    resized_image <- magick::image_convert(resized_image,format = image_out_ext)
  }

  magick::image_write(resized_image, path = path)

}



