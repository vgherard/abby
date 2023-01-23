assert_is_number <- function(x, name = deparse(substitute(x))) {
	valid <- is.numeric(x) &&
		length(x) == 1 &&
		!is.na(x)

	if (valid)
		return(TRUE)

	msg <- paste0("'", name, "' is not a number (a numeric of length 1, not NA).")
	rlang::abort(msg, class = "abby_nan_error")
}

assert_is_probability <- function(x, name = deparse(substitute(x))) {
	valid <- assert_is_number(x, name) &&
		x >= 0 &&
		x <= 1

	if (valid)
		return(TRUE)

	msg <- paste0("'", name, "' is not a probability (a number between 0 and 1).")
	rlang::abort(msg, class = "abby_nap_error")
}
