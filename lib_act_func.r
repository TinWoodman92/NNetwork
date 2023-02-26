# Activation function library

relu <- function(x) return(x * (x > 0))
Drelu <- function(x) {
	lx <- as.numeric(x > 0)
	attributes(lx) <- attributes(x)
	return(lx)
}

sigmoid <- function(x) 1 / (1 + exp(-x))
Dsigmoid <- function(x) eval(D(expression(1 / (1 + exp(-x))), "x"))

#tanh
Dtanh <- function(x) return(eval(D(expression(tanh(x)), "x")))

no_func <- function(x) return(x)
Dno_func <- function(x) {
	lx <- rep(1, length(x))
	attributes(lx) <- attributes(x)
	return(lx)
}

sel_g <- function(func_name) {
	if (func_name == "relu") return(relu)
	if (func_name == "sigmoid") return(sigmoid)
	if (func_name == "tanh") return(tanh)
	if (func_name == "no_func") return(no_func)
}

sel_d <- function(func_name) {
	if (func_name == "relu") return(Drelu)
	if (func_name == "sigmoid") return(Dsigmoid)
	if (func_name == "tanh") return(Dtanh)
	if (func_name == "no_func") return(Dno_func)
}