
SRS = function(s_out, variable_lvl){
	if(any(is.na(s_out$IDENTITY.STATUS))){
	   stop("Non-response detected! Please proceed to DOUBLE PHASE SAMPLING")
	    }
	else if( (sum(as.integer(table(s_out$IDENTITY.STATUS))) / length(unique(s_out$IDENTITY.STATUS))) < 5){
		cat("\n", "It may take some time, BE PATIENT", "\n", "\n", "\n")
		original_data <- s_out$IDENTITY.STATUS
		nsims = 100000
		domain_ident <- variable_lvl
		g_squared <- rep(NA,nsims)
		observed_value<- GTest(table(original_data))$statistic
		for (i in 1:nsims){
			shuffled <- table(sample(domain_ident,nrow(s_out), replace=TRUE))
			g_squared[i] <- GTest(shuffled)$statistic
					}
			p_value = length(g_squared[g_squared >= observed_value])/nsims
		clc()
		cat(suppressWarnings(chisq.test(table(s_out$IDENTITY.STATUS))$p.value), "(CHISQUARE P-VALUE)","\n", p_value, "(EXACT TEST P-VALUE)","\n")
					}
	else{
	cat(chisq.test(table(s_out$IDENTITY.STATUS))$p.value, "(CHISQUARE P-VALUE)", "\n")
	    }
	}	



SRS(s_out, variable_lvl)

