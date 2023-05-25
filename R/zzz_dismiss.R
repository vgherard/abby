check_redundant_arguments <- function(l, args) {
	s <- 0

	for (arg in args)
		s <- s + exists(arg, l)

	if (s <= 1)
		return(TRUE)

	msg <- paste0(
		"The following arguments cannot be specified simultaneously: ",
		paste0(args, collapse = ", "),
		"."
	)
	rlang::abort(msg, class = "abby_arg_conflict_error")
}
