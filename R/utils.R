parse_cs_numeric_input <- function(txt) {
	res <- strsplit(txt, ",")
	res <- unlist(res)
	as.numeric(res)
}
