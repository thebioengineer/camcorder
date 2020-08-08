library(magick)

square <- image_read("https://github.com/gkaramanis/tidytuesday/blob/master/2020-week32/plots/european-energy.png") # square

tall <- image_read("https://github.com/gkaramanis/tidytuesday/blob/master/2020-week29/plots/astronauts.png") # tall

wide <- image_read("https://github.com/gkaramanis/tidytuesday/blob/master/2020-week28/plots/coffee-ratings.png") # wide

image_info(square)
image_info(wide)
image_info(tall)

resize <- function(image, max_size = 1024, background = "black") {
  image_width <- image_info(image)$width
  image_height <- image_info(image)$height

  # image_resize should give better results(?) but slower than image_scale
  # http://www.imagemagick.org/Usage/resize/

  if (image_width >= image_height) {
    resized_image <- image_resize(image, max_size)
  } else {
    resized_image <- image_resize(image, paste0("x", max_size))
  }

  image_extent(resized_image, paste0(max_size, "x", max_size), color = background)
}

resize(tall) # with defaults
resize(wide, max_size = 800, background = "pink")
resize(wide, 2048, "green")
resize(square, 1200)
