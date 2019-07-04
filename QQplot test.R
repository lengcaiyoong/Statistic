#qqplot test
	#although we cannot prove if data comes from normal distribution
	#we can disprove it

#Parameter
	#ori_data 	: original data
	#nsims    	: number of simulations
	#cor_oridata: correlation of original data in qqplot


	cat("\n","Name your data as ori_data!", "\n","For example: ", "\n","\n", "ori_data <- c(12,3,5,5,9,5)", "\n", "\n")

	ANS <- readline(noquote("Have you named it? [Y/N]: "))	

	Proceed = function(ANS){
	if (ANS == "N" | ANS == "n" | ANS == "no" |  ANS == "NO" |  ANS == "No"){
	 stop("Please name it!!")
					     } 
				   }

	Proceed(ANS)

	nsims = 100000

	cor_stor = rep(NA,nsims)
	cor_oridata <- round(cor(qqnorm(ori_data,plot=F)$x,qqnorm(ori_data,plot=F)$y),4)
	for (i in 1:nsims){
		stor <- rnorm( length(ori_data), mean(ori_data) , sd(ori_data))
		cor_stor[i] <- cor(qqnorm(stor,plot=F)$x,qqnorm(stor,plot=F)$y)
			}

	cor_stor <- round(cor_stor,4)
	
	qq_pv = length(which(cor_stor <= cor_oridata)) / nsims
	
	if (qq_pv <.05) cat("\n",  "since", qq_pv, "< .05, disprove NORMALITY assumption", "\n","\n" )	else cat("\n", "since", qq_pv,">.05, MAYBE generated from normal distribution", "\n","\n")


