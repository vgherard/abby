plot_fnr_fpr_by_batches <- function(
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
	ggplot(data, aes(x = fpr, y = fnr, color = as.factor(pct_change))) +
		geom_line() +
		facet_grid(. ~ batches) +
		geom_vline(xintercept = xintercept, linetype = "dashed") +
		geom_hline(yintercept = yintercept, linetype = "dashed") +
		scale_x_continuous(name = xtitle, breaks = xbreaks, labels = xlabels) +
		scale_y_continuous(name = ytitle, breaks = ybreaks, labels = ylabels) +
		guides(color = guide_legend("% Change from Baseline")) +
		NULL
}

