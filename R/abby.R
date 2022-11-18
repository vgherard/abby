#' @title abby
#'
#' @author Valerio Gherardi
#'
#' @description Opens abby
#'
#' @return returns \code{NULL}, invisibly. Called for side-effects.
#'
#' @details No details
#'
#' @examples
#' \dontrun{
#' abby()
#' }
#'
#' @export
abby <- function() {

	#------------------------------------------------------------ User Interface
	ui <- fluidPage(
		"Hello, world!"
	)
	server <- function(input, output, session) {
	}


	#------------------------------------------------------------------- Run App
	viewer <- dialogViewer(dialogName = "abby", width = 800, height = 600)
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}
