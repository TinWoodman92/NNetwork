source(paste(getwd(), "NNetwork_refCls.r", sep = "/"))
source(paste(getwd(), "test_gen.r", sep = "/"))

ni <- 5
no <- packBits(as.integer(c(rep(1, ni), rep(0, 32 - ni))), "integer") + 1

nnetwork_1 <- NNetwork(
	node_counts = c(ni, 32, 32, no),
	l_rate = 0.01,
	act_func_names = c("no_func", "relu", "relu", "no_func")
)

cost_chk <- rep(0, no)
res_chk <- rep(0, no)

for (i in 0:10000) {

	ti <- test_input(ni)
	tar_out <- test_output(ti)

	ao <- nnetwork_1$fw_prop(ti)
	cost <- nnetwork_1$back_prop(ti, tar_out)

	tar_int <- read_input(ti)
	act_int <- clean_output(ao)

	if (tar_int == 3) {print(c(cost, ao[4], act_int))}

	cost_chk[tar_int + 1] <- cost
	res_chk[tar_int + 1] <- act_int

	if (i %% 1 == 0) {
		nnetwork_1$grad_desc()
	}
}

cost_chk
res_chk
