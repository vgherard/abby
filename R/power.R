compute_power_wrap <- function(input) {
	compute_power(
		input$test_type,
		input$alternative,
		input$baseline,
		parse_cs_numeric_input(input$pct_change),
		input$users_batch,
		input$batches,
		input$pct_traffic_a,
		input$pct_traffic_b
	)
}

compute_power <- function(
		test_type,
		alternative,
		baseline,
		pct_change,
		users_batch,
		batches,
		pct_traffic_a,
		pct_traffic_b
		)
{

	res <- tidyr::expand_grid(
		test_type = test_type,
		alternative = alternative,
		baseline = baseline,
		pct_change = pct_change,
		users_batch = users_batch,
		batches = batches[1]:batches[2],
		pct_traffic_a = pct_traffic_a,
		pct_traffic_b = pct_traffic_b,
		fpr = seq(from = 1, to = 99, by = 1) / 100
	)
	res$fnr <- NA_real_

	with(res, for(i in 1:nrow(res)) {
		p1 <- baseline[i] * (1 + pct_change[i])
		p2 <- baseline[i]
		h <- pwr::ES.h(p1, p2)
		users <- users_batch[i] * batches[i]
		n1 <- pct_traffic_b[i] * users
		n2 <- pct_traffic_a[i] * users
		sig.level <-

			res$fnr[i] <<- 1 - pwr::pwr.2p2n.test(
				h = h,
				n1 = pct_traffic_b[i] * users,
				n2 = pct_traffic_a[i] * users,
				sig.level = fpr[i],
				power = NULL,
				alternative = alternative[i]
			)$power

	})

	return(res)
}

plot_power_grid <- function(data) {
	ggplot(data, aes(x = fpr, y = fnr, color = as.factor(pct_change))) +
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
}

