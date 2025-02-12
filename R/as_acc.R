#' Convert to acc
#'
#' @param x A `move2` containing acceleration data as collected by EOBS or ornitella tracking devices.
#'
#' @param ... currently not used
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
  lst <- lapply(
    lapply(split(data.frame(x)[, c("tilt_x", "tilt_y", "tilt_z")], x$start_timestamp), matrix),
    \(x) {
      v <- do.call("c", x)
      m <- matrix(v, ncol = 3) * units::as_units(units::deparse_unit(v))
      colnames(m) <- colnames(x)
      m
    }
  )
  frq <- do.call(c, lapply(
    lapply(
      lapply(
        lapply(
          split(
            move2::mt_time(x),
            x$start_timestamp
          ), diff
        ), units::as_units
      ),
      \(x) 1 / x
    ), mean
  ))
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
