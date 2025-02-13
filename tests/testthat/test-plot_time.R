test_that("plot_time", {
  x <- acc(list(
    matrix(1:12, ncol = 3, dimnames = list(NULL, letters[1:3])),
    matrix(1:8, ncol = 2, dimnames = list(NULL, letters[4:5]))
  ), frequency = units::set_units(2:3,'Hz'))
  expect_silent(graph<-plot_time(x, Sys.time()+c(0,10)))
  expect_s3_class(graph,"dygraphs")

})
