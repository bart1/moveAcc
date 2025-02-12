new_acc_list <- function(x) {
  assertthat::assert_that(all(unlist(lapply(x, \(y) is.null(y) || !is.null(colnames(y))))))
  new_list_of(x, ptype = matrix(numeric()), class = "acc_list")
}

acc_list <- function(x) {
  new_acc_list(x)
}

#'  Create a `acc` vector
#'
#' @param bursts a list of matrices
#'
#' @param frequency The frequency of the acceleration recordings. Either the same length of `bursts` or it will be recycled
#'
#' @export
acc <- function(bursts = list(), frequency = units::set_units(double(), "Hz")) {
  bursts <- new_acc_list(bursts)
  frequency <- vec_recycle(frequency, vec_size(bursts))
  new_acc(bursts, frequency)
}

new_acc <- function(bursts = new_acc_list(list()), frequency = units::set_units(double(), "Hz")) {
  # if(!all(unlist(lapply(bursts, \(y) is.null(y)||!is.null(colnames(y)))))){
  #   abort("not all burst have names")
  # }
  new_rcrd(list(bursts = bursts, frequency = frequency), class = "acc")
}
#' #' @export
#'
#' new_acc <- function(x, frequency) {
#'   vec_check_size(frequency, length(x))
#'   assertthat::assert_that(all(unlist(lapply(x, \(y) is.null(y)||!is.null(colnames(y))))))
#'   vctrs::new_list_of(x, ptype = matrix(numeric()),
#'                      frequency=frequency, class = "acc")
#' }
#' #' @export
#' acc<-function(...){
#'   new_acc(...)
#' }
#'
# new_acc_regular<-function(x, frequency, axes){}
