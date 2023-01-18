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
		test_type,
		alternative,
		baseline,
		pct_change,
		users_batch,
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

compute_power_wrap <- function(input) {
	compute_power(
		input$test_type,
		input$alternative,
		input$baseline,
		input$pct_change,
		input$users_batch,
		input$batches,
		input$pct_traffic_a,
		input$pct_traffic_b
	)
}
