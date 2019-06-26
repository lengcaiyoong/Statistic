

#RGVAR_STR(s_out_rgstr,s_frame, strata, variable_lvl,s_out_rgsrs)


RGVAR_STR = function(s_out,s_frame, strata, variable_lvl,s_out_rgsrs){
	#Decide Group numbers	
		#prime number function
	    is.prime <- function(num) {				#check for prime number
  			 if (num == 2) {
			      TRUE
		   } else if (any(num %% 2:(num-1) == 0)) {
			      FALSE
			   } else { 
			      TRUE
				   }
				}
		u_strata	= as.numeric(table(s_out$DEPT_CODE)) #number of sampled units per strata
	   if(is.prime(min(u_strata)) ){		 #suggest imputation if primed number
		  stop("Imputation recommended")
		}else{
			for(i in 1:min(u_strata)) {				#if not prime number, choose n_rdgrp
				if( (min(u_strata) %% i) == 0) {
					print(i)
						}
							}
		     }
	    n_rdgrp = as.numeric(readline(noquote("choose NUMBER OF RANDOM GROUP:	")))		#ask user to select number of random group
	    u_rdgrp = (min(u_strata) / n_rdgrp) * length(strata)
	#Create Random Group and Base Weight
		nonreplaced_strata = s_out
		nonreplaced_DEPTCODE <- sort(unique(s_out$DEPT_CODE)) 
		Nhmh_nonreplaced <- 
			( (as.numeric(table(s_frame$DEPT_CODE)) [which(sort(unique(s_frame$DEPT_CODE)) ==   nonreplaced_DEPTCODE)]) / ( min(u_strata)/n_rdgrp )  )						
		Bweight_nonreplaced <- list()
			for (i in 1:length(Nhmh_nonreplaced) ){
		Bweight_nonreplaced[[i]] <- rep( Nhmh_nonreplaced[i] , min(u_strata))
				}
		nonreplaced_strata$B_WEIGHT <- unlist(Bweight_nonreplaced , use.names=FALSE)			#assign B_weight to nonreplaced_strata
		nonreplaced_strata$B0_WEIGHT <- unlist(Bweight_nonreplaced , use.names=FALSE)			#assign B0_weight to nonreplaced_strata
		nonreplaced_strata$B0_WEIGHT[which(is.na(nonreplaced_strata$IDENTITY.STATUS))] <- 0		#assign zero for NA row
		nonreplaced_strata$rdgrp_code <- sample(sort(	rep(1:n_rdgrp, (nrow(nonreplaced_strata)/n_rdgrp))	))
		rdgrp <- split(nonreplaced_strata, nonreplaced_strata$rdgrp_code)
	#Check Weight Stability
		stability_list2 <- list()
		stability_list1 <- list()
			for (i in 1:length(rdgrp) ) {
	  			stability2 <- split(rdgrp[[i]], rdgrp[[i]]$NEW.VARIABLE)
				for (j in 1:length(unique(s_out$NEW.VARIABLE) ) ){
							stability_list2[[j]] <- 
						length(which (is.na(stability2[[j]]$IDENTITY.STATUS ))) / nrow(stability2[[j]]) >= 0.5
												}
				stability_list1[[i]] <- stability_list2
							    }
		if(any(unlist(stability_list1))){			#check if any class has more than half nonrespondents
			stop("Imputation recommended")
		}else{
		     }
	#Weighting Class Adjustment
		#reorder NEW.VARIABLE
			wei_classvar	<- factor(unique(s_out$NEW.VARIABLE) , levels = unique(s_out$NEW.VARIABLE)  )
		#check if zero elements within class
			#decide_weiclass <- list()	
			#	for (i in 1: length(rdgrp)  ){
			#		  decide_weiclass[[i]]  <- (!(sort(unique(s_frame$YEAR))   %in% 	sort(unique(rdgrp[[i]]$NEW.VARIABLE)) ))
			#						}
			#if (any(unlist(decide_weiclass, use.names=F ))){			#is there any class which has zero element?
			#	stop("No respondents in certain class; Imputation Recommended")
			#}else{
			#     }
		#create F_WEIGHT column
			#Find which random group has NA
			detect_NA <- rep(NA,length(rdgrp))		
			for (i in 1:length(rdgrp)){
				  detect_NA[i] <- any(is.na(rdgrp[[i]]$IDENTITY.STATUS))
								}
			detect_NA <- which(detect_NA == TRUE)		#detect which random group needs weigthing class adjustment
			non_NA <- which(!(1:length(rdgrp) %in% detect_NA))	#find which random group has no NA		
				for (i in non_NA){						#create weight2 for non-NA random group
					rdgrp[[i]]$B_WEIGHT2 <- rep(1,u_rdgrp)
							}
			#(SUM of TOTAL WEIGHTS per random group)
		by_nwvar1 <- list()
		sum_BW1 <- list()
			for (i in detect_NA){				#calculate sum of weights of all units for each random group
				by_nwvar1[[i]] <- group_by(rdgrp[[i]], NEW.VARIABLE)       
				sum_BW1[[i]] <- summarize(by_nwvar1[[i]], sum(B_WEIGHT))	
						}
			#(SUM of RESPONDENTS per random group)
		by_nwvar <- list()
		sum_BW <- list()
			for (i in detect_NA){				#calculate sum of weights of respondent for each random group
				by_nwvar[[i]] <- group_by(rdgrp[[i]], NEW.VARIABLE)       
				sum_BW[[i]] <- summarize(by_nwvar[[i]], sum(B0_WEIGHT))	
						   }
			#Check if Zero Sum in Respondents
		decide_weiclass2 <- list()
			for (i in detect_NA){			#detect zero sum of weights of respondents
			decide_weiclass2[[i]] <- sum_BW[[i]][[2]]
							}
		if(	any(unlist(decide_weiclass2, use.names=F) == 0)	){	 #is there any sum of weights of respondents equal to zero
			stop("IMPUTATION RECOMMENDED")
				}else{
					}
		F_WEIGHT <- list()
			for (i in detect_NA){							#calculate final weight for each random group
				F_WEIGHT[[i]]	<-
				sum_BW1[[i]][[2]] / sum_BW[[i]][[2]]
						}
		B_WEIGHT2 <- list()
		F_WEIGHT2 <- list()
			for (i in detect_NA){
				for(j in 1:u_rdgrp){
				   F_WEIGHT2[[j]] <- (F_WEIGHT[[i]])[rdgrp[[i]]$NEW.VARIABLE[j] == wei_classvar]
							}
			   B_WEIGHT2[[i]]<- unlist(F_WEIGHT2, use.names=F )		
				rdgrp[[i]]$B_WEIGHT2 <-  B_WEIGHT2[[i]]				#assign adjusted B_WEIGHT2 to each random group
						 }
			for (i in 1:length(rdgrp)){						#create F_WEIGHT column
					rdgrp[[i]]$F_WEIGHT <- rdgrp[[i]]$B_WEIGHT * rdgrp[[i]]$B_WEIGHT2
							}
		#calculate Variance STR
	 		storage_varlvl <- list()
	 		   for (i in 1:length(rdgrp) ){				#change Identity Status to 1 & 0
	    			 for (j in 1:length(variable_lvl) ){
					storage_varlvl[[j]]  <- as.numeric(rdgrp[[i]]$IDENTITY.STATUS == variable_lvl[j])
					storage_varlvl[[j]][ which( is.na(storage_varlvl[[j]]) ) ] <- 0	#assign 0 to NA rows in 1&0 identity status
							 		     }
					names(storage_varlvl) <- variable_lvl
					rdgrp[[i]] <- cbind(rdgrp[[i]],as.data.frame(storage_varlvl))
							      }		
			storage_varlvl2 <- list()
				for (i in 1:length(rdgrp)){				#calculate F_WEIGHT * yi for each random group
						storage_varlvl2[[i]] <-
							select(rdgrp[[i]], as.character(variable_lvl)) * rdgrp[[i]]$F_WEIGHT
					   			   }
			tht_r <- list()
			for (i in 1:length(storage_varlvl2)){		#calculate estimator F_WEIGHT*yi/sum(F_WEIGHT) for each random group (each random groups have different variable levels estimator)
					tht_r[[i]] <-
					as.numeric(	apply(  storage_varlvl2[[i]],2,sum)) / sum(	rdgrp[[i]]$F_WEIGHT)
									}
			tht_r2 <- do.call(rbind.data.frame, tht_r)
			colnames(tht_r2) <- as.character(variable_lvl)		#parameter of interest from each random group
			tht = as.numeric(apply(tht_r2,2,sum)) / n_rdgrp		#average of tht_r for each variable level
				##Estimated Variance
			strVAR = rep(NA, length(variable_lvl))
			for (i in 1:length(variable_lvl)){
					strVAR[i] <- (1/(n_rdgrp*(n_rdgrp-1))) * sum((tht_r2[,i] - tht[i])^2)
								    }
		srsVAR <- RGVAR_SRS(s_out_rgsrs,s_frame, strata, variable_lvl, u_rdgrp)
		strVAR / srsVAR
									    }



