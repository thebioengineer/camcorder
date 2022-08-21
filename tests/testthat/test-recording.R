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

  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 1)

  ## preview film shows the file
  preview_html <-  preview_film()

  file.rename(preview_html,file.path(tempdir(),"camcorder_preview_ggplot.html"))

  expect_snapshot_file(
    path = file.path(tempdir(),"camcorder_preview_ggplot.html")
  )

  expect_equal(
    GG_RECORDING_ENV$last_plot,
    mtcars_plot
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

  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 1)

  ## preview film shows the file
  preview_html <-  preview_film()

  file.rename(preview_html,file.path(tempdir(),"camcorder_preview_patchwork.html"))

  expect_snapshot_file(
    path = file.path(tempdir(),"camcorder_preview_patchwork.html")
  )

  expect_equal(
    GG_RECORDING_ENV$last_plot,
    mtcars_patchwork
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

  plot(1:5)

  record_polaroid()

  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 1)

  ## preview film shows the file
  preview_html <-  preview_film()

  file.rename(preview_html,file.path(tempdir(),"camcorder_preview_polaroid.html"))

  expect_snapshot_file(
    path = file.path(tempdir(),"camcorder_preview_polaroid.html")
  )

})

test_that("recording works - gif output", {

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

  expect_snapshot_file(
    path = playback_file
  )

})

test_that("resizing a plot works", {

  rec_dir <- file.path(tempdir(),"camcorder_tests_ggplot")

  if(dir.exists(rec_dir)){
    unlink(rec_dir,recursive = TRUE)
  }

  gg_record(dir = rec_dir)
  on.exit(gg_stop_recording())

  mtcars_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cyl), y = mpg))

  record_ggplot(mtcars_plot)

  ## Recording created directory
  expect_true(dir.exists(rec_dir))

  ## Recording added a single file
  expect_equal(length(list.files(rec_dir)), 1)

  ## preview film shows the file
  preview_html <-  preview_film()

  file.rename(preview_html,file.path(tempdir(),"camcorder_preview_ggplot.html"))

  expect_snapshot_file(
    path = file.path(tempdir(),"camcorder_preview_ggplot.html")
  )

  expect_equal(
    GG_RECORDING_ENV$last_plot,
    mtcars_plot
  )

})
