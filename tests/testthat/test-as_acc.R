test_that("Test as_acc eobs", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      d <- as.Date("2008-7-27")
      expect_no_warning(x<-move2::movebank_download_study(2911040, # "Galapagos Albatrosses"
                                              sensor_type_id = "acceleration",
                                              timestamp_start = as.POSIXct(d),
                                              timestamp_end = as.POSIXct(d) + 3600
      ))
      skip_if(class(x) != "move2")
      expect_s3_class(as_acc(x), "acc")
      expect_equal(colnames(field(as_acc(x),"bursts")[[3]]), strsplit(as.character(x$eobs_acceleration_axes[3]),'')[[1]])
      expect_length(as_acc(x), nrow(x))
      expect_equal((is.na(as_acc(x))), (is.na(x$eobs_accelerations_raw)))
      expect_true(is_uniform(as_acc(x)))

    })
  )
})


test_that("Test as_acc ornitella", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      d <- as.Date("2021-3-3")
      expect_no_warning(
      x <- move2::movebank_download_study(985143423, # LBBG_ZEEBRUGGE - Lesser black-backed gulls
                                              sensor_type_id = "acceleration",
                                              timestamp_start = as.POSIXct(d),
                                              timestamp_end = as.POSIXct(d + 1)
      ))
      skip_if(class(x) != "move2")
      expect_s3_class(as_acc(x), "acc")

      expect_length(as_acc(x), nrow(x))
      expect_equal(sum(!is.na(as_acc(x))), length(unique(x$start_timestamp)))
      expect_equal((is.na(as_acc(x))), (duplicated(x$start_timestamp)))
      expect_true(is_uniform(as_acc(x)))
      expect_equal(colnames(field(as_acc(x)[!is.na(as_acc(x))],"bursts")[[3]]), c("tilt_x", "tilt_y","tilt_z"))
    })
  )
})

