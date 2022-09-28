#' Vectorized rescaling images for gif
#'
#' @param film paths of images to rescale
#' @param cassette path of output folder that contains resized images
#' @param size size to rescale images to in pixels
#' @param background color to set the background. A valid color string such as "navyblue" or
#' "#000080". Use "none" for transparancy.
#' @return vector of paths to resized images
#' @noRd
scale_film <- function(film, cassette, size = 600, background = "black") {

  if (missing(cassette)) {
    cassette <- file.path(GG_RECORDING_ENV$recording_dir, "resized")

  }

  if(!dir.exists(cassette)){
      dir.create(cassette)
  }

  image_ext <- file_ext(film[1])

  image_pattern <- paste0("_", size, "x", size, "_",background)

  ## what resized film already exists, as to not do-over work
  existing_resized_film <-
    list.files(cassette,
               pattern = paste0(image_pattern,"[.]",ifelse(image_ext == "pdf","pdf","png")),
               full.names = TRUE)

  existing_resized_film <-
    gsub(image_pattern,
         "",
         existing_resized_film)

  film_to_resize <- film[!basename(file_path_sans_ext(film)) %in% basename(file_path_sans_ext(existing_resized_film))]

  q_lapply(film_to_resize, function(film_path) {

    base_image_name <- file_path_sans_ext(basename(film_path))
    image_ext <- tools::file_ext(film_path)
    path <- file.path(cassette,
                      paste0(
                        paste0(
                          base_image_name,
                          "_resize",
                          image_pattern
                        ),
                        ".",
                        ifelse(image_ext == "pdf","pdf","png")
                      ))

    resize_image(film_path,
                 path,
                 max_size = size,
                 background = background)

  })

  list.files(cassette,
             pattern = paste0(image_pattern, "[.]"),
             full.names = TRUE)

}
