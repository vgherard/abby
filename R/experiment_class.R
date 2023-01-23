#' @export
experiment <- function(type, alternative, prospects = NULL, traffic = NULL)
	new_experiment(type, alternative, prospects, traffic)

new_experiment <- function(type, alternative, prospects, traffic) {
	match.arg(type, choices = c("binomial"))
	match.arg(alternative, choices = c("greater", "less", "two.sided"))

	obj_raw <- list(
		type = type,
		alternative = alternative,
		prospects = NULL,
		traffic = NULL
	)

	structure(obj_raw, class = experiment_cls()) |>
		add_prospects(prospects) |>
		add_traffic(traffic)
}

experiment_cls <- function() "abby_experiment"

is_experiment <- function(object) {
	identical(class(object), experiment_cls())
}





