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

  skip_on_cran()

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
      units = "px",
      zoom = 1
    )
    on.exit(gg_stop_recording())

    # Examples from: https://gt.rstudio.com/articles/creating-summary-lines.html
    # 1)
    exibble_gt <- gt::gt(exibble)
    record_gt(exibble_gt)
    # 2)
    gg_resize_film(width = 800)
    # 3)
    exibble_a <-
      exibble[, c("num", "char", "currency", "row", "group")] |>
      gt::gt(rowname_col = "row", groupname_col = "group") |>
      gt::sub_missing()
    record_gt(exibble_a)
    # 4)
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
    # 5)
    gg_resize_film(expand = 20, zoom = 2)

    playback_file <- file.path(tempdir(),"camcorder_playback_gt.gif")

    gg_playback(
      name = playback_file,
      first_image_duration = 1,
      last_image_duration = 3,
      frame_duration = 1,
      image_resize = 800
    )

    expect_true(file.exists(playback_file))

    expect_snapshot_file(
      path = file.path(tempdir(),"camcorder_playback_gt.gif")
    )

  })
})

test_that("recording gt and ggplot together works - gif output", {

  skip_on_cran()

  skip_on_ci()

  withr::with_package("gt",code = {

    rec_dir <- file.path(tempdir(),"camcorder_tests_gt_ggplot")

    if(dir.exists(rec_dir)){
      unlink(rec_dir,recursive = TRUE)
    }

    gg_record(dir = rec_dir)
    on.exit(gg_stop_recording())

    # safety check for rendering large tables
    gt_big <- gt::gt(ggplot2::diamonds)
    expect_error(
      expect_message(
        record_gt(gt_big),
        "Table dimensions exceed"
      ),
      "use `limitsize = FALSE`"
    )

    # 1)
    gt_1 <- gt::gt(ggplot2::diamonds) |>
      gt_preview()
    record_gt(gt_1)
    # 2)
    gt_2 <- gt::gt(ggplot2::diamonds) |>
      gt::gt_preview() |>
      gt::cols_hide(-c(cut, price))
    record_gt(gt_2)
    # 3)
    gt_3 <- aggregate(ggplot2::diamonds, price ~ cut, "mean") |>
      gt::gt() |>
      gt::cols_label(price = "average price")
    record_gt(gt_3)
    # 4)
    ggplot_1 <- aggregate(ggplot2::diamonds, price ~ cut, "mean") |>
      ggplot2::ggplot(ggplot2::aes(cut, price)) +
      ggplot2::geom_col(
        ggplot2::aes(fill = ifelse(grepl("Good", cut), "salmon", "grey35")),
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_identity()
    record_ggplot(ggplot_1)
    # 5)
    gt_4 <- aggregate(ggplot2::diamonds, price ~ cut, "mean") |>
      gt::gt() |>
      gt::cols_label(price = "average price") |>
      gt::tab_style(
        style = list(gt::cell_fill(color = "salmon")),
        locations = gt::cells_body(rows = 2:3)
      )
    record_gt(gt_4)

    playback_file <- file.path(tempdir(),"camcorder_tests_gt_ggplot.gif")

    gg_playback(
      name = playback_file,
      first_image_duration = 1,
      last_image_duration = 3,
      frame_duration = 1
    )

    expect_true(file.exists(playback_file))

    expect_snapshot_file(
      path = file.path(tempdir(),"camcorder_tests_gt_ggplot.gif")
    )

  })
})
