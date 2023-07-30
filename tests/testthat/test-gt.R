test_that("recording a basic gt works", {

  withr::with_package("gt",code = {

    rec_dir <- file.path(tempdir(),"camcorder_tests_gt")

    if(dir.exists(rec_dir)){
      unlink(rec_dir,recursive = TRUE)
    }

    gg_record(dir = rec_dir)
    on.exit(gg_stop_recording())

    exibble_gt <- gt::gt(exibble)
    record_gt(exibble_gt)

    expect_equal(
      GG_RECORDING_ENV$last_plot,
      exibble_gt
    )

    ## Recording created directory
    expect_true(dir.exists(rec_dir))

    ## Recording adds html and png files
    expect_equal(file_ext(list.files(rec_dir)), c("html", "png"))

    skip_on_ci()

    file.rename(
      list.files(rec_dir,full.names = TRUE,pattern = "\\.png$"),
      file.path(tempdir(),"camcorder_preview_gt.png")
    )
    expect_snapshot_file(
      path = file.path(tempdir(),"camcorder_preview_gt.png")
    )

  })
})

test_that("recording a gt with webshot options works", {

  withr::with_package("gt",code = {

    rec_dir <- file.path(tempdir(),"camcorder_tests_gt")

    if(dir.exists(rec_dir)){
      unlink(rec_dir,recursive = TRUE)
    }

    gg_record(dir = rec_dir, expand = 20, zoom = .5)
    on.exit(gg_stop_recording())

    exibble_gt <- gt::gt(exibble)
    record_gt(exibble_gt)

    skip_on_ci()

    file.rename(
      list.files(rec_dir,full.names = TRUE,pattern = "\\.png$"),
      file.path(tempdir(),"camcorder_preview_gt_resize.png")
    )
    expect_snapshot_file(
      path = file.path(tempdir(),"camcorder_preview_gt_resize.png")
    )

  })
})

test_that("recording gt works - gif output", {

  skip_on_ci()

  withr::with_package("gt",code = {

    rec_dir <- file.path(tempdir(),"camcorder_tests_gt")

    if(dir.exists(rec_dir)){
      unlink(rec_dir,recursive = TRUE)
    }

    gg_record(
      dir = rec_dir,
      device = "png",
      width = 600,
      height = 800,
      units = "px"
    )
    on.exit(gg_stop_recording())

    # Examples from: https://gt.rstudio.com/articles/creating-summary-lines.html
    # 1)
    exibble_gt <- gt::gt(exibble)
    record_gt(exibble_gt)
    # 2)
    exibble_a <-
      exibble[, c("num", "char", "currency", "row", "group")] |>
      gt::gt(rowname_col = "row", groupname_col = "group") |>
      gt::sub_missing()
    record_gt(exibble_a)
    # 3)
    gg_resize_film(width = 400)
    exibble_b <-
      exibble_a |>
      summary_rows(
        groups = everything(),
        columns = num,
        fns = list(
          average = "mean",
          total = "sum",
          SD = "sd"
        )
      )
    record_gt(exibble_b)

    playback_file <- file.path(tempdir(),"camcorder_playback_gt.gif")

    gg_playback(
      name = playback_file,
      first_image_duration = .5,
      last_image_duration = 3,
      frame_duration = .5,
      image_resize = 800
    )

    expect_true(file.exists(playback_file))

    skip_on_ci()

    expect_snapshot_file(
      path = file.path(tempdir(),"camcorder_playback_gt.gif")
    )

  })
})
