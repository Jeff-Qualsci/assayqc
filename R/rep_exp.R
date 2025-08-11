make_md_data <- function(smpl = NULL, exp1, exp2, pot = TRUE) {
  if (!is.numeric(exp1) | !is.numeric(exp2)) {
    stop("Both exp1 and exp2 must be numeric vectors")
  }
  if (is.null(smpl)) {
    smpl <- as.character(seq_along(exp1))
    message("Sequential sample identifiers generated for each data pair")
  }

  if (!is.character(smpl)) {
    stop("smpl must be a character vector")
  }

  if (!(length(smpl) == length(exp1) & (length(exp2) == length(exp1)))) {
    stop("smpl, exp1, and exp2 must all be the same length")
  }

  if (pot) {
    exp1 <- log10(exp1)
    exp2 <- log10(exp2)
  }

  md_data <- tibble::tibble(smpl, meas1 = exp1, meas2 = exp2) |>
    dplyr::mutate(
      meas_mean = (meas1 + meas2) / 2,
      meas_diff = meas2 - meas1
    )
}
