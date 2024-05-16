# repexp() and helper functions

# abort_bad_argument() -----------------------------
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  rlang::abort("error_bad_argument",
        message = msg,
        arg = arg,
        must = must,
        not = not
  )
}


# repexp() -------------------------------

repexp <- function(id = NULL,run1, run2, potency = TRUE, plots = TRUE) {

  if (!is.numeric(run1)) {
    abort_bad_argument("x", must = "be numeric", not = run1)
  }
  if (!is.numeric(run2)) {
    abort_bad_argument("base", must = "be numeric", not = run2)
  }
  if (is.null(id)) {
    id <- as.character(c(1:length(run1)))
    message("Sequential id values were assigned.")
  }
  if (length(run1) !== length(run2) || length(run1) !== length(id)) {
    message("All input vectors must have the same length")
  }
}
