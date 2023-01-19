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

	ui <- fluidPage(
		titlePanel("Abby"),

		sidebarLayout(

			sidebarPanel(
				actionButton("compute", "Compute"),
				selectInput(
					"test_type", label = "AB Test type",
					choices = list(
						"Binomial test" = "p"
						),
					selected = "p"),
				selectInput(
					"alternative", label = "Alternative",
					choices = list(
						"Greater" = "greater",
						"Less" = "less",
						"Two-Sided" = "two.sided"
					),
					selected = "p"),
				numericInput(
					"baseline",
					label = "Baseline",
					value = 0.1,
					step = 0.01,
					min = 0,
					max = 1
					),
				textInput(
					"pct_change",
					label = "% Change from Baseline",
					value = "0.05, 0.1, 0.15, 0.2"
					),

				numericInput("users_batch", label = "Users / Batch", value = 1000),
				shinyWidgets::numericRangeInput(
					"batches",
					"Batches",
					value = c(1, 2),
					min = 1),
				sliderInput("pct_traffic_a",
										label = "% Traffic (A)", min = 0, max = 1, value = 0.5,
										step = 0.01
										),
				sliderInput("pct_traffic_b",
										label = "% Traffic (B)", min = 0, max = 1, value = 0.5,
										step = 0.01
										)
				),

			mainPanel(esquisse::ggplot_output("plot"))

		)

	)

	# Define the server logic
	server <- function(input, output)
	{
			data <- reactive(compute_power_wrap(input)) |> bindEvent(input$compute)
			esquisse::render_ggplot("plot", {
				plot_fnr_fpr_by_pct_change(data())
			})
	}

	# Run the Shiny app
	viewer <- dialogViewer(dialogName = "abby", width = 800, height = 600)
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}
