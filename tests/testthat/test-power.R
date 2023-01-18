test_that("compute_power throws no error with basic arguments", {
  expect_no_error(compute_power(
  	test_type = "p",
  	alternative = "greater",
  	baseline = 0.1,
  	pct_change = 0.1,
  	users_batch = 1000,
  	batches = c(2, 3),
  	pct_traffic_a = 0.5,
  	pct_traffic_b = 0.5
  ))
})

test_that("compute_power returns a tibble", {
	res <- compute_power(
		test_type = "p",
		alternative = "greater",
		baseline = 0.1,
		pct_change = 0.1,
		users_batch = 1000,
		batches = c(2, 3),
		pct_traffic_a = 0.5,
		pct_traffic_b = 0.5
	)

	expect_s3_class(res, class(dplyr::tibble()))
})
