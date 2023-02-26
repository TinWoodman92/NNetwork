source(paste(getwd(), "lib_act_func.r", sep = "/"))

attr_list <- list(
	node_count = "numeric",
	inputs = "numeric",
	act_func = "function",
	d_act_func = "function",
	weights = "matrix",
	biases = "numeric",
	act_out_val = "numeric",
	z_in_val = "numeric",
	act_in_val = "numeric",
	dc_dw_arr = "array",
	dc_db_arr = "array"
)

# init method as passable variable
func_i <- function(node_count, inputs, act_func_name) {
	node_count <<- node_count
	inputs <<- inputs
	init_w()
	biases <<- rep(0, node_count)
	act_func <<- sel_g(act_func_name)
	d_act_func <<-  sel_d(act_func_name)
	dc_dw_arr <<- array(0, dim = 0)
	dc_db_arr <<- array(0, dim = 0)
}

# activation function caller as passable variable
func_1 <- function(act_in) {
	act_in_val <<- act_in
	z_in_val <<- z_input(act_in)
	act_out_val <<- act_func(z_in_val)
	return(act_out_val)
}

# z expression caller as passable variable
func_2 <- function(act_in) {
	res <- weights %*% act_in
	res <- res + biases
	return(as.vector(res))
}

# weight initialization as passable function
# Xavier Initialization
func_3 <- function() {
	weights <<- matrix(
		rnorm(
			inputs * node_count,
			sd = sqrt(2 / (inputs + node_count))
		),
		ncol = inputs,
		nrow = node_count
	)
}

# back propogation method as passable variable
func_4 <- function(dc_dao) {
	dao_dz <- d_act_func(z_in_val)
	dz_dw <- matrix(
		act_in_val,
		ncol = inputs,
		nrow = node_count,
		byrow = TRUE
	)
	dc_dw <- dc_dao * dao_dz * dz_dw
	dc_db <- dc_dao * dao_dz
	dz_dai <- weights
	dc_dai <- colSums(dc_dao * dao_dz * dz_dai)

	if (dim(dc_dw_arr)[1] == 0) {
		dc_dw_arr <<- array(dc_dw, dim = c(dim(weights), 1))
		dc_db_arr <<- array(dc_db, dim = c(1, length(biases)))
	} else {
		dc_dw_arr <<- array(append(dc_dw_arr, dc_dw), dim = c(dim(weights), dim(dc_dw_arr)[3] + 1))
		dc_db_arr <<- rbind(dc_db_arr, dc_db)
	}
	
	return(dc_dai)
}

# gradient descent implementer method as passable variable
func_5 <- function(l_rate) {
	weights <<- weights - l_rate * apply(dc_dw_arr, MARGIN = c(1, 2), FUN = mean)
	biases <<- biases - l_rate * colMeans(dc_db_arr)

	dc_dw_arr <<- array(0, dim = 0)
	dc_db_arr <<- array(0, dim = 0)
}

meth_list <- list(
	initialize = func_i,
	act_out = func_1,
	z_input = func_2,
	init_w = func_3,
	back_prop = func_4,
	grad_desc = func_5
)

Layer <- setRefClass("Layer", fields = attr_list, methods = meth_list)
