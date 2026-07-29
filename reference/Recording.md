# Record and generate plot histories

Record plots created over time and generate a GIF of the plots made in
the 'R' session. Overrides the print methods for ggplot and patchwork
objects from the 'ggplot2' and 'patchwork' packages respectively.

resize the film for recording, reprints and saves last plot

Stop recording images with camcorder.

## Usage

``` r
gg_record(
  dir = NULL,
  device = c("png", "pdf", "jpeg", "bmp", "tiff", "emf", "svg", "eps", "ps"),
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  device_ext = NULL,
  bg = NULL
)

gg_playback(
  name = NULL,
  first_image_duration = 16,
  last_image_duration = 20,
  frame_duration = 0.25,
  loop = TRUE,
  image_resize = 600,
  background = "black",
  width = NULL,
  height = NULL,
  progress = interactive(),
  playback = TRUE,
  stoprecording = FALSE,
  last_as_first = TRUE,
  ...
)

gg_resize_film(height = NA, width = NA, units = NA, dpi = NA)

gg_stop_recording()
```

## Arguments

- dir:

  directory to save the intermediate plots in. Defaults to a temporary
  directory

- device:

  Device to use. Can either be a device function (e.g. png()), or one of
  "png", "pdf", "jpeg", "bmp", "tiff", "emf", "svg", "eps", "ps".

- scale:

  Multiplicative scaling factor.

- width, height, units:

  Plot size in `units` ("in", "cm", "mm", or "px"). If not supplied,
  uses the size of current graphics device.

- dpi:

  Plot resolution. Also accepts a string input: "retina" (320), "print"
  (300), or "screen" (72). Applies only to raster output types.

- limitsize:

  When `TRUE` (the default),
  [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) will
  not save images larger than 50x50 inches, to prevent the common error
  of specifying dimensions in pixels.

- device_ext:

  file extension to use for images created. Does not usually need to be
  populated manually.

- bg:

  Background colour. If `NULL`, uses the `plot.background` fill value
  from the plot theme.

- name:

  name of gif.

- first_image_duration:

  n units of frame_duration to show the first image for.

- last_image_duration:

  n units of frame_duration to show the last image for.

- frame_duration:

  n seconds each plot should be shown.

- loop:

  if the gif should be repeated. Set to FALSE to only play once, or a
  number to indicate how many times to repeat after the first.

- image_resize:

  size to rescale images to in pixels.

- background:

  color to set the background of the gif. A valid color string such as
  "navyblue" or "#000080". Use "none" for transparency. Does not impact
  the background of images.

- progress:

  print some verbose status output

- playback:

  Boolean, should the recording start playing after it is turned into a
  gif? defaults to TRUE.

- stoprecording:

  Boolean, should the plots stop being recorded? defaults to TRUE.

- last_as_first:

  Should the last plot be displayed at the beginning too?

- ...:

  Other arguments passed on to the graphics device function, as
  specified by `device`.

## Value

Used initialize recording, nothing returned

Returns nothing. Used to generate the gif.

Returns the last plot generated, resized to new dimensions

Returns nothing. used for side effect.

## Functions

- `gg_playback()`:

- `gg_resize_film()`:

- `gg_stop_recording()`:

## Examples

``` r
 if(require(ggplot2) & interactive()){
  gg_record(dir = file.path(tempdir(),"recording"))
  ggplot(data.frame(x = 1, y = 1), aes(x=x, y=y)) + geom_point() + ylim(0,4)
  ggplot(data.frame(x = 1, y = 2), aes(x=x, y=y)) + geom_point() + ylim(0,4)

  ## resize canvas of the last plot
  gg_resize_film(height = 10, width = 5, dpi = 350)

  ggplot(data.frame(x = 1, y = 3), aes(x=x, y=y)) + geom_point() + ylim(0,4)

  gg_playback(tempfile(fileext = ".gif"))
}
#> Loading required package: ggplot2
```
