test_that("create zero length", {
  expect_identical(acc(), new_acc())
  expect_length(new_acc(), 0)
  expect_length(acc(), 0)
})
test_that("manipulation", {
  x <- acc(list(
    matrix(1:12, ncol = 3, dimnames = list(NULL, letters[1:3])),
    matrix(1:8, ncol = 2, dimnames = list(NULL, letters[4:5]))
  ), frequency = 2:3)
  expect_identical(head(x, 1), x[1])
  expect_length(x[1], 1)
  expect_length(x[rep(1, 3)], 3)
})



test_that("properties are correctly calculated",{

  x <- acc(
    bursts = list(
      cbind(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
      cbind(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
    ),
    frequency = units::as_units(c(20, 30), "Hz")
  )
  x <- c(x, NA)
  expect_length(x,3)
  expect_identical(is.na(x),c(F,F,T))
  expect_identical(n_axis(x), c(3L,2L,NA))
  expect_identical(n_samples(x), c(30L,20L,NA))
  expect_false(is_uniform(x))
  expect_true(is_uniform(x[c(1,3)]))

})

test_that("multiplication works", {
  skip("To work on")
  acc(list(
    matrix(1:12, ncol = 3, dimnames = list(NULL, letters[1:3])),
    matrix(1:8, ncol = 2, dimnames = list(NULL, letters[4:5]))
  ), frequency = 2:3)
  new_acc(list(
    matrix(units::set_units(1:12, "g"), ncol = 3),
    matrix(units::set_units(1:8, "g"), ncol = 2)
  ))
  new_acc(list(
    structure(units::set_units(1:10, "g"), dim = c(5L, 2L)),
    structure(units::set_units(1:15, "g"), dim = c(5L, 3L))
  ), frequency = c(20, 30), axes = list(c("x", "y"), c("x", "y", "z")))
  new_acc(list(
    structure(units::set_units(1:10, "m/s"), dim = c(5L, 2L)),
    structure(units::set_units(1:15, "m/s"), dim = c(5L, 3L))
  ))
})
test_that("size", {
  skip()
  # aa <- move2::movebank_retrieve("study_attribute", study_id = 4502577, sensor_type_id = 2365683) |>
  #   dplyr::pull("short_name") |>
  #   grep(pat = "eobs_acceler", value = T)
  # a <- move2::movebank_download_study(4502577, attributes = aa)
  # matrixList <- a$eobs_accelerations_raw |>
  #   tail() |>
  #   strsplit(" ") |>
  #   lapply(as.integer) |>
  #   lapply(matrix, ncol = 3, byrow = T)
  # new_acc(matrixList)
  # aaa <- a$eobs_accelerations_raw[100000 + 1:1000]
  # aac <- as_acc(aaa)
  # pryr::object_size(aaa)
  # pryr::object_size(aac)
  # pryr::object_size(unlist(aac))
  # pryr::object_size(packBits(lapply(lapply(unlist(aac), intToBits), head, 12) |> unlist(), "integer"))
  # l <- array(unlist(aac), c(68, 3, 1000))
  # c(apply(l, 2:3, mean))
  # bench::mark(
  #   unlist(lapply(aac, apply, 2, mean)),
  #   c(apply(l, 2:3, mean))
  # )
})
