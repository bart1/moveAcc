#' Ploting the acceleration bursts over time
#'
#' @param x an acc vector
#'
#' @param time a `POSIXct` with the start time of acc bursts
#' @param ylab A character with the y axis label
#'
#' @export
plot_time <- function(x, time, ylab = "Acceleration") {
  vec_check_size(time, vec_size(x))
  rlang::check_installed("dygraphs","dplyr")
  dt <- mapply(function(x, n) c(units::drop_units((c(0, seq_len(n))) / x)),
               x = field(x, "frequency")[!is.na(x)],
               n = n_samples(x)[!is.na(x)], SIMPLIFY = F
  )
  df <- dplyr::bind_cols(
    time = do.call("c", mapply("+", time[!is.na(x)], dt, SIMPLIFY = F)),
    dplyr::bind_rows(lapply(field(x, "bursts")[!is.na(x)], function(x) rbind(data.frame(x), NA)))
  )
  dygraphs::dygraph(df) |>
    dygraphs::dyRibbon() |>
    dygraphs::dyAxis("y", ylab)
}
