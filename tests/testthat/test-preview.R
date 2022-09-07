test_that("records of files works", {

  rec_dir <- file.path(tempdir(),"list_recorded_files")

  if(dir.exists(rec_dir)){
    unlink(rec_dir,recursive = TRUE)
  }

  gg_record(dir = rec_dir)
  on.exit(gg_stop_recording())


  mtcars_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cyl), y = mpg))

  record_ggplot(mtcars_plot)
  Sys.sleep(.1)
  record_ggplot(mtcars_plot)
  Sys.sleep(.1)
  record_ggplot(mtcars_plot)

  records <- get_file_record()

  expect_equal(length(records), 3)


})

test_that("records of files ignores extra files", {

  rec_dir <- file.path(tempdir(),"list_recorded_extra_files")

  if(dir.exists(rec_dir)){
    unlink(rec_dir,recursive = TRUE)
  }

  gg_record(dir = rec_dir)
  on.exit(gg_stop_recording())


  mtcars_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cyl), y = mpg))

  record_ggplot(mtcars_plot)
  Sys.sleep(.1)
  record_ggplot(mtcars_plot)
  Sys.sleep(.1)
  record_ggplot(mtcars_plot)

  ## add random file to be ignored
  suppressMessages({
  ggsave(
    filename = file.path(rec_dir,"random_file.png"),
    plot = mtcars_plot,
    device = png
  )})

  records <- get_file_record()

  expect_equal(length(records), 3)


})

