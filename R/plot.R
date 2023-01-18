plot_fnr_fpr_by_pct_change <- function(
		data,
		xintercept = 0.2,
		yintercept = 0.2,
		xbreaks = seq(from = 0, to = 1, by = 0.2),
		ybreaks = seq(from = 0, to = 1, by = 0.2),
		xlabels = scales::label_percent(),
		ylabels = scales::label_percent(),
		ytitle = "False Negative Rate",
		xtitle = "False Positive Rate",
		colortitle = "% Change from Baseline"
		)
{
	data %>%
		mutate(
			color = as.factor(pct_change),
			grid_label = ifelse(batches == 1, "1 batch", paste(batches, "batches"))
			) %>%
		ggplot(aes(x = fpr, y = fnr, color = color)) +
			geom_line() +
			facet_grid(. ~ grid_label) +
			geom_vline(xintercept = xintercept, linetype = "dashed") +
			geom_hline(yintercept = yintercept, linetype = "dashed") +
			scale_x_continuous(name = xtitle, breaks = xbreaks, labels = xlabels) +
			scale_y_continuous(name = ytitle, breaks = ybreaks, labels = ylabels) +
			guides(color = guide_legend("% Change from Baseline")) +
			NULL
}

