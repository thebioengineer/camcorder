test_that("recording a basic ggplot works", {

  rec_dir <- file.path(tempdir(),"camcorder_tests_ggplot")

  if(dir.exists(rec_dir)){
    unlink(rec_dir,recursive = TRUE)
  }

  gg_record(dir = rec_dir)
  on.exit(gg_stop_recording())

  mtcars_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cyl), y = mpg))

  record_ggplot(mtcars_plot)


  expect_equal(
    GG_RECORDING_ENV$last_plot,
    mtcars_plot
  )

  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 1)

  skip_on_ci()

  file.rename(list.files(rec_dir,full.names = TRUE),file.path(tempdir(),"camcorder_preview_ggplot2.png"))
  expect_snapshot_file(
    path = file.path(tempdir(),"camcorder_preview_ggplot2.png")
  )

})

test_that("recording a basic patchwork works", {

  withr::with_package("patchwork",code = {

  rec_dir <- file.path(tempdir(),"camcorder_tests_patchwork")

  if(dir.exists(rec_dir)){
    unlink(rec_dir,recursive = TRUE)
  }

  gg_record(dir = rec_dir)
  on.exit(gg_stop_recording())

  mtcars_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cyl), y = mpg))

  mtcars_patchwork <- wrap_plots(mtcars_plot,mtcars_plot,ncol = 2)

  record_patchwork(mtcars_patchwork)

  expect_equal(
    GG_RECORDING_ENV$last_plot,
    mtcars_patchwork
  )
  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 1)

  skip_on_ci()

  file.rename(list.files(rec_dir,full.names = TRUE),file.path(tempdir(),"camcorder_preview_patchwork.png"))
  expect_snapshot_file(
    path = file.path(tempdir(),"camcorder_preview_patchwork.png")
  )

  })
})

test_that("recording a basic graphic devices works", {

  rec_dir <- file.path(tempdir(),"camcorder_tests_polaroid")

  if(dir.exists(rec_dir)){
    unlink(rec_dir,recursive = TRUE)
  }

  gg_record(dir = rec_dir)
  on.exit(gg_stop_recording())

  png(filename = tempfile(fileext = ".png"))

  plot(1:5)

  record_polaroid()

  dev.off()

  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 1)

  skip_on_ci()

  file.rename(list.files(rec_dir,full.names = TRUE),file.path(tempdir(),"camcorder_preview_polaroid.png"))
  expect_snapshot_file(
    path = file.path(tempdir(),"camcorder_preview_polaroid.png")
  )


})

test_that("recording works - gif output", {

  skip_on_ci()

  rec_dir <- file.path(tempdir(),"camcorder_tests_playback")

  if(dir.exists(rec_dir)){
    unlink(rec_dir)
  }

  gg_record(dir = rec_dir)
  on.exit(gg_stop_recording())

  mtcars_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cyl), y = mpg))

  record_ggplot(mtcars_plot)
  record_ggplot(mtcars_plot +
                  ggplot2::scale_x_discrete(labels = c("4 CYL","6 CYL","8 CYL"))
  )



  playback_file <- file.path(tempdir(),"camcorder_playback.gif")
  gg_playback(playback_file)

  expect_true(file.exists(playback_file))

})

test_that("resizing a plot works", {

  rec_dir <- file.path(tempdir(),"camcorder_tests_resizing")

  if(dir.exists(rec_dir)){
    unlink(rec_dir,recursive = TRUE)
  }

  gg_record(dir = rec_dir,width = 5, height = 5, units = "in", dpi = 100)
  on.exit(gg_stop_recording())

  mtcars_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cyl), y = mpg))

  record_ggplot(mtcars_plot)

  mtcars_plot <- gg_resize_film(height = 10, width = 5, units = "in")

  ##re-record with larger size. gg_resize_film prints and thus would record
  ##automatically normally
  record_ggplot(mtcars_plot)

  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 2)

  image_sizes <- list.files(rec_dir,full.names = TRUE) %>%
    lapply(magick::image_read) %>%
    lapply(magick::image_info) %>%
    sapply(function(x){paste(x$width,"x",x$height)})

  expect_equal(
    image_sizes,
    c("500 x 500", "500 x 1000")
  )

})
