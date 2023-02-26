test_input <- function(input_node_count) {
	return(floor(runif(input_node_count, max = 2)))
}

test_output <- function(test_input) {
	inc <- length(test_input)
	onc <- packBits(as.integer(c(rep(1, inc), rep(0, 32 - inc))), "integer") + 1
	tar_int <- packBits(as.integer(c(test_input, rep(0, 32 - inc))), "integer")
	tar_out <- rep(0, onc)
	tar_out[tar_int + 1] <- 1
	return(tar_out)
}

read_input <- function(test_input) {
	inc <- length(test_input)
	tar_int <- packBits(as.integer(c(test_input, rep(0, 32 - inc))), "integer")
	return(tar_int)
}

read_output <- function(output_signal) {
	tar_int <- which(output_signal == 1) - 1
	return(tar_int)
}

clean_output <- function(output_signal) {
	mo <- max(output_signal)
	tar_int <- which(output_signal == mo) - 1
	return(tar_int)
}