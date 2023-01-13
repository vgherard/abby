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
						"Binomial test" = "p",
						"t-test" = "t"
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
				numericInput("min_batches", label = "Min. Batches", value = 1),
				numericInput("max_batches", label = "Max. Batches", value = 2),
				sliderInput("pct_traffic_a",
										label = "% Traffic (A)", min = 0, max = 1, value = 0.5,
										step = 0.01
										),
				sliderInput("pct_traffic_b",
										label = "% Traffic (B)", min = 0, max = 1, value = 0.5,
										step = 0.01
										)
				),

			mainPanel(plotOutput("plot"))

		)

	)

	# Define the server logic
	server <- function(input, output) {

			output$plot <- renderPlot({
				.data <- tidyr::expand_grid(
					test_type = input$test_type,
					alternative = input$alternative,
					baseline = input$baseline,
					pct_change = parse_cs_numeric_input(input$pct_change),
					users_batch = input$users_batch,
					batches = input$batches[1]:input$batches[2],
					pct_traffic_a = input$pct_traffic_a,
					pct_traffic_b = input$pct_traffic_b,
					fpr = seq(from = 1, to = 99, by = 1) / 100
				)
				.data$fnr <- NA_real_

				with(.data, for(i in 1:nrow(.data)) {
					p1 <- baseline[i] * (1 + pct_change[i])
					p2 <- baseline[i]
					h <- pwr::ES.h(p1, p2)
					users <- users_batch[i] * batches[i]
					n1 <- pct_traffic_b[i] * users
					n2 <- pct_traffic_a[i] * users
					sig.level <-

						.data$fnr[i] <<- 1 - pwr::pwr.2p2n.test(
							h = h,
							n1 = pct_traffic_b[i] * users,
							n2 = pct_traffic_a[i] * users,
							sig.level = fpr[i],
							power = NULL,
							alternative = alternative[i]
						)$power

				})

				ggplot(.data, aes(x = fpr, y = fnr, color = as.factor(pct_change))) +
					geom_line() +
					facet_grid(. ~ batches) +
					geom_hline(yintercept = 0.2, linetype = "dashed") +
					geom_vline(xintercept = 0.2, linetype = "dashed") +
					scale_y_continuous(
						name = "False Negative Rate",
						breaks = seq(from = 0, to = 1, by = 0.2),
						labels = scales::label_percent(),
					) +
					scale_x_continuous(
						name = "False Positive Rate",
						breaks = seq(from = 0, to = 1, by = 0.2),
						labels = scales::label_percent(),
					) +
					guides(color = guide_legend("% Change from Baseline")) +
					NULL

			}) |>
				bindEvent(input$compute)

	}

	# Run the Shiny app
	viewer <- dialogViewer(dialogName = "abby", width = 800, height = 600)
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}
