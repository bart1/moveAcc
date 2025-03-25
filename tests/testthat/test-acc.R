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

test_that("logical",{
  expect_false(is_acc(NA))
  expect_false(is_acc(cbind(1,1:3)))
  expect_true(is_acc(acc()))
  expect_true(is_acc(acc(list(
    matrix(1:12, ncol = 3, dimnames = list(NULL, letters[1:3])),
    matrix(1:8, ncol = 2, dimnames = list(NULL, letters[4:5]))
  ), frequency = 2:3)))
  expect_true(is_acc(acc(list(NULL
  ), frequency = NA)))
})

test_that("properties are correctly calculated",{

  xa <- acc(
    bursts = list(
      cbind(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
      cbind(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
    ),
    frequency = units::as_units(c(20, 30), "Hz")
  )
  x <- c(xa, NA)
  expect_true(is_acc(x))
  expect_length(x,3)
  expect_identical(is.na(x),c(F,F,T))
  expect_identical(n_axis(x), c(3L,2L,NA))
  expect_identical(n_samples(x), c(30L,20L,NA))
  expect_false(is_uniform(x))
  expect_true(is_uniform(x[c(1,3)]))
  x2 <- vec_c(NA,xa)
  expect_true(is_acc(x2))
  expect_length(x2,3)
  expect_identical(is.na(x2),c(T,F,F))
  expect_identical(n_axis(x2), c(NA,3L,2L))
  expect_identical(n_samples(x2), c(NA,30L,20L))
  expect_false(is_uniform(x2))
  expect_true(is_uniform(x2[c(1,3)]))

})

