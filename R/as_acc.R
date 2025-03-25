#' Convert to acc
#'
#' In many cases the `as_acc` function will directly create an acceleration vector from input data
#'
#' @param x A `move2` containing acceleration data as collected by EOBS or ornitella tracking devices. Most of the time
#'   this will be either loaded from disk using [move2::mt_read] or downloaded using [move2::movebank_download_study].
#'
#' @param ... currently not used
#'
#' @details The resulting vector will be as long as the input. This means it can, for example, be added as a column to a
#' `data.frame`. For some tags this means `NA` values are inserted when one burst is stored over multiple rows of a
#' `data.frame`.
#'
#'
#' @export

as_acc <- function(x, ...) {
  UseMethod("as_acc")
}
#' @export
as_acc.default <- function(x, ...) {
  vctrs::vec_cast(x, new_acc())
}

#' @export
as_acc.move2 <- function(x, ...) {
  if (all(c("tilt_x", "tilt_y", "tilt_z", "start_timestamp") %in% colnames(x))) {
    return(as_acc_move2_ornitella(x, ...))
  }
  if (all(c("eobs_acceleration_axes", "eobs_acceleration_sampling_frequency_per_axis", "eobs_accelerations_raw") %in% colnames(x))) {
    return(as_acc_move2_eobs(x, ...))
  }
  stop("No acc conversion implemented")
}
as_acc_move2_ornitella <- function(x, ...) {
  assertthat::assert_that(units(x$tilt_x)==units(x$tilt_y))
  assertthat::assert_that(units(x$tilt_x)==units(x$tilt_z))
  assertthat::assert_that(!any(unlist(lapply(lapply(split( move2::mt_track_id(x),x$start_timestamp),unique), length))!=1))
  m<-as.matrix(data.frame(x)[, c("tilt_x", "tilt_y", "tilt_z")])* units::as_units(units::deparse_unit(x$tilt_x))
  lst<-purrr::map(split(seq_len(nrow(m)), x$start_timestamp), ~m[.x,])

  frq <- do.call(c, lapply(
      lapply(
       diffs<- lapply(
          split(
            move2::mt_time(x),
            x$start_timestamp
          ), diff
        ), units::as_units
      ),
      \(x) mean(1 / x)
  ))
  assertthat::assert_that( all(unlist(diffs)>0))
  acc <- vec_rep(new_acc(list(NULL), units::set_units(NA, "Hz")), nrow(x))
  s <- !duplicated(x$start_timestamp) & !is.na(x$start_timestamp)
  acc[s] <- new_acc(lst, frq)
  acc
}
as_acc_move2_eobs <- function(x, ...) {
  colnms <- strsplit(as.character(x$eobs_acceleration_axes), "")
  n_axis <- nchar(as.character(x$eobs_acceleration_axes))
  mlist <- strsplit(x$eobs_accelerations_raw, " ") |> lapply(as.integer)
  i <- !is.na(n_axis)
  mlist[!i] <- list(NULL)
  mlist[i] <- mapply(matrix, mlist[i], ncol = n_axis[i], MoreArgs = list(byrow = TRUE), SIMPLIFY = FALSE)
  mlist[i] <- mapply("colnames<-", mlist[i], colnms[i], SIMPLIFY = FALSE)
  new_acc(mlist, frequency = x$eobs_acceleration_sampling_frequency_per_axis)
}
