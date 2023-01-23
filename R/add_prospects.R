#' @export
add_prospects <- function(experiment, ...) {
	args <- list(...)

	validate_prospects(experiment, args)

	for (arg_name in names(args)) {
		experiment$prospects[[arg_name]] <- args[[arg_name]]
	}

	return(experiment)
}

validate_prospects <- function(experiment, args) {

	validators <- switch(experiment$type,
		binomial = list(
			value = assert_is_probability,
			pct_change = assert_is_number
		)
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
