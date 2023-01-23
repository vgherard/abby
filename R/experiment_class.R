#' @export
experiment <- function(type, alternative, prospects = NULL, traffic = NULL)
	new_experiment(type, alternative, prospects, traffic)

new_experiment <- function(type, alternative, prospects, traffic) {
	type <- match.arg(type, c("binomial"))
	alternative <- match.arg(alternative, c("greater", "less", "two.sided"))

	obj_raw <- list(
		type = type,
		alternative = alternative,
		prospects = list(),
		traffic = list()
	)

	res <- structure(obj_raw, class = experiment_cls())
	if (!is.null(prospects))
		res <- do.call(\(...) add_prospects(res, ...), prospects)
	if (!is.null(traffic))
		res <- do.call(\(...) add_traffic(res, ...), traffic)

	return(res)
}

experiment_cls <- function() "abby_experiment"

is_experiment <- function(object) {
	identical(class(object), experiment_cls())
}





