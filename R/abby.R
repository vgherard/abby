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


	vis_panel <- miniContentPanel(esquisse::ggplot_output("plot"))
	data_panel <- miniContentPanel()
	config_panel <- miniContentPanel(
		fillCol(
			fillRow(
				flex = c(2,5,5),
				h5("Experiment"),
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
					selected = "p")
			),

			fillRow(
				flex = c(2,5,5),
				h5("Prospects"),
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
				 	label = "% Change",
				 	value = "0.05, 0.1, 0.15, 0.2"
				)
			),

			fillRow(
				flex = c(2,5,5),
				h5("Traffic"),
				fillCol(
					numericInput("users_batch", label = "Users / Batch", value = 1000),
					shinyWidgets::numericRangeInput(
						"batches",
						"Batches",
						value = c(1, 2),
						min = 1),
				),

				fillCol(
					sliderInput("pct_traffic_a",
						label = "% Traffic (A)", min = 0, max = 1, value = 0.5,
						step = 0.01
					),
					sliderInput("pct_traffic_b",
						label = "% Traffic (B)", min = 0, max = 1, value = 0.5,
						step = 0.01
					)
				)
			)

		),





	)

	ui <- miniPage(
		miniTitleBar(
			"Abby",
			left = miniTitleBarCancelButton("exitButton", label = "Exit"),
			right = miniTitleBarButton("compute", "Compute", primary = TRUE)
			),

		miniTabstripPanel(
			miniTabPanel("Configure",	config_panel, icon = icon("sliders")),
			miniTabPanel("Visualize", vis_panel, icon = icon("area-chart")),
			miniTabPanel("Data", data_panel, icon = icon("table")),
			selected = "Visualize"
		)
	)

	# Define the server logic
	server <- function(input, output)
	{
			data <- reactive(compute_power_wrap(input)) |>
				bindEvent(input$compute)
			esquisse::render_ggplot("plot", plot_fnr_fpr_by_pct_change(data()))
			observeEvent(input$exitButton, stopApp(), ignoreInit = TRUE)
	}

	# Run the Shiny app
	viewer <- dialogViewer(dialogName = "abby", width = 800, height = 600)
	#viewer <- paneViewer()
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}
