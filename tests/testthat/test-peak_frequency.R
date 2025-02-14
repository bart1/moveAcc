test_that("Correct frequency found", {
  a<-new_acc(list(matrix(sin(1:200/(50/(pi*2))))), units::set_units(200,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(4,'Hz')))
})

test_that("Multiple axis peak freq and changing freq", {
  a<-new_acc(list(cbind(z=cos(1:200/(100/(pi*2))),
                        x=sin(1:200/(5/(pi*2))))), units::set_units(200,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(c(z=2,x=40),'Hz')))

  a<-new_acc(list(cbind(z=cos(1:200/(100/(pi*2))),
                        x=sin(1:200/(5/(pi*2))))), units::set_units(100,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(c(z=1,x=20),'Hz')))
  a<-new_acc(list(cbind(z=cos(1:200/(100/(pi*2))),
                        x=sin(1:200/(5/(pi*2))))), units::set_units(400,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(c(z=4,x=80),'Hz')))
})

test_that("length does not influnce result", {
  a<-new_acc(list(cbind(z=cos(1:199/(100/(pi*2))),
                        x=sin(1:199/(5/(pi*2))))), units::set_units(200,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(c(z=2,x=40),'Hz')))
  a<-new_acc(list(cbind(z=cos(1:199/(100/(pi*2))),
                        x=sin(1:199/(5/(pi*2))))), units::set_units(100,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(c(z=1,x=20),'Hz')))
  a<-new_acc(list(cbind(z=cos(1:199/(100/(pi*2))),
                        x=sin(1:199/(5/(pi*2))))), units::set_units(400,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(c(z=4,x=80),'Hz')))
})

test_that("Multiple axis peak freq intercept does not matter", {
  a<-new_acc(list(cbind(z=-3+.1*cos(1:200/(100/(pi*2))),x=3*(2+sin(1:200/(50/(pi*2)))))), units::set_units(200,'Hz'))
  expect_equal(peak_frequency(a), list(units::set_units(c(z=2,x=4),'Hz')))
})

test_that("NA returns empty",{
  expect_equal(peak_frequency(new_acc(list(NULL), frequency = NA)), list(NULL))
  expect_equal(peak_frequency(new_acc(list(NULL, NULL), frequency = c(NA, NA))), list(NULL, NULL))
  expect_equal(peak_frequency(new_acc(list(NULL,matrix(sin(1:200/(50/(pi*2)))),  NULL),
                                      frequency = units::set_units(c(NA,200, NA),'Hz'))),
               list(NULL,units::set_units(4,"Hz"), NULL))

})

test_that("Resolution alows to identify partial frequencies", {
  a<-new_acc(list(cbind(z=cos(1:200/(80/(pi*2))),
                        x=sin(1:200/(5/(pi*2))))), units::set_units(200,'Hz'))
  expect_equal(peak_frequency(a),
               list(units::set_units(c(z=3,x=40),'Hz')))
  expect_equal(peak_frequency(a, resolution = units::set_units(.5,'Hz')),
               list(units::set_units(c(z=2.5,x=40),'Hz')))
  expect_equal(peak_frequency(a, resolution = units::set_units(.25,'Hz')),
               list(units::set_units(c(z=2.5,x=40),'Hz')))
})


test_that("Resolution alows to identify partial frequencies", {
  a<-new_acc(list(matrix(runif(100), ncol=10)), units::set_units(23,'Hz'))
  expect_equal((((unlist(peak_frequency(a, resolution = units::set_units(.005,'Hz')))/.005)+.5)%%1)-.5,
               rep(0,10) )
  expect_equal((((unlist(peak_frequency(a, resolution = units::set_units(.025,'Hz')))/.025)+.5)%%1)-.5,
               rep(0,10) )
#  expect_equal((((unlist(peak_frequency(a, resolution = units::set_units(.035,'Hz')))/.035)+.5)%%1)-.5,
#               rep(0,10) )
  a<-new_acc(list(matrix(runif(1000), ncol=10)), units::set_units(23,'Hz'))
  expect_equal((((unlist(peak_frequency(a, resolution = units::set_units(.005,'Hz')))/.005)+.5)%%1)-.5,
               rep(0,10) )
  expect_equal((((unlist(peak_frequency(a, resolution = units::set_units(.025,'Hz')))/.025)+.5)%%1)-.5,
               rep(0,10) )

  })

test_that("works with and without units", {
  a<-new_acc(list(cbind(c(1:5,5:1, 1:5), c(4,3,4))), units::set_units(23,'Hz'))
  b<-new_acc(list(units::set_units(cbind(c(1:5,5:1, 1:5), c(4,3,4)),"m/s")), units::set_units(23,'Hz'))

  expect_equal(peak_frequency(a), peak_frequency(b))
})
