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

	# Define the user interface
	ui <- fluidPage(
		sliderInput("slope", "Slope", min = 1, max = 10, value = 1),
		plotOutput("line_plot")
	)

	# Define the server logic
	server <- function(input, output) {

		# Create a reactive variable for the line data
		line_data <- reactive({
			data.frame(x = c(0, 10), y = input$slope * c(0, 10))
		})

		# Render the line plot
		output$line_plot <- renderPlot({
			ggplot(line_data(), aes(x, y)) +
				geom_line()
		})
	}

	# Run the Shiny app
	viewer <- dialogViewer(dialogName = "abby", width = 800, height = 600)
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}
