source(paste(getwd(), "Layer_refCls.r", sep = "/"))

attr_list <- list(
	node_counts = "numeric",
	l_rate = "numeric",
	layers = "list"
)

func_i <- function(node_counts, l_rate, act_func_names) {
	node_counts <<- node_counts
	l_rate <<- l_rate
	layers <<- list()
	for (i in 1:(length(node_counts) - 1)) {
		layers <<- c(
			layers,
			Layer(
				node_counts[i + 1],
				node_counts[i],
				act_func_names[i + 1]
			)
		)
	}
}

# forward propagation caller as passable variable
func_1 <- function(act_in) {
    act_i <- act_in
	for (i in 1:length(layers)) {
		act_i <- layers[[i]]$act_out(act_i)
	}
	return(act_i)
}

# back propagation caller as passable variable
func_2 <- function(act_in, tar_out) {
	act_out <- fw_prop(act_in)
	cost <- sum((act_out - tar_out) ^ 2)
	dc_da <- 2 * (act_out - tar_out)
	for (i in length(layers):1) {
		dc_da <- layers[[i]]$back_prop(dc_da)
	}
	return(cost)
}

# gradient descent as passable variable
func_3 <- function() {
	for (i in length(layers):1) {
		layers[[i]]$grad_desc(l_rate)
	}
}

# these are class methods, so not adjustable instance-to-instance
meth_list <- list(
	initialize = func_i,
	fw_prop = func_1,
	back_prop = func_2,
	grad_desc = func_3
)

NNetwork <- setRefClass("NNetwork", fields = attr_list, methods = meth_list)
