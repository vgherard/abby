assert_is_number <- function(x, name = deparse(substitute(x)))
{
	cnd <- is.numeric(x) &&
		length(x) == 1 &&
		!is.na(x)

	check_assertion(
		cnd, name, "a number", "abby_nan_error"
		)
}

assert_is_probability <- function(x, name = deparse(substitute(x)))
{
	cnd <- assert_is_number(x, name) &&
		x >= 0 &&
		x <= 1
	check_assertion(
		cnd, name, "a probability (a number between 0 and 1)", "abby_naprob_error"
		)
}

assert_is_positive_integer <- function(x, name = deparse(substitute(x)))
{
	cnd <- assert_is_number(x, name) &&
		x >= 0 &&
		(x == as.integer(x))

	check_assertion(cnd, name, "a positive integer", "abby_napi_error")
}

assert_is_split <- function(x, name = deparse(substitute(x))) {
	cnd <- is.numeric(x) &&
		length(x) == 2 &&
		assert_is_probability(x[[1]], name) &&
		assert_is_probability(x[[2]], name) &&
		(x[[1]] + x[[2]] <= 1)

	check_assertion(cnd, name, "a valid split", "abby_split_error")
}

assert <- function(
	condition,
	object_name,
	object_description,
	error_class
	)
{
	if (condition)
		return(TRUE)
	msg <- paste0("'", name, "' is not ", object_description, ".")
	rlang::abort(msg, class = error_class)
}


