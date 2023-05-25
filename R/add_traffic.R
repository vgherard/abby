#' @export
add_traffic <- function(experiment, ...) {
	args <- list(...)

	validate_traffic(args)

	for (arg_name in names(args)) {
		experiment$traffic[[arg_name]] <- args[[arg_name]]
	}

	return(experiment)
}

validate_traffic <- function(args) {

	validators <- list(
		n = Vectorize(assert_is_positive_integer),
		f = assert_is_split
		)


	valid_arg_names <- names(validators)

	for (arg_name in names(args)) {
		if (!(arg_name %in% valid_arg_names)) {
			msg <- paste0("Unrecognized prospect argument '",
										arg_name, "' for experiment of type '", experiment$type,
										"'.")
			rlang::abort(msg, class = "abby_unrec_arg_error")
		}
		arg_value <- args[[arg_name]]
		validators[[arg_name]](arg_value, name = arg_name)
	}
}
