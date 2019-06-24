
#RGVAR_SRS(s_out,s_frame, strata, variable_lvl)

RGVAR_SRS = function(s_out,s_frame, strata, variable_lvl){
	##Eliminate row to make sure equal size random group
		while( nrow(s_out) %% n_rdgrp != 0 ){		#eliminate row until no remainder
			s_out <- s_out[-sample(1:nrow(s_out),1) , ]
				}
	##Create random group and BASE WEIGHT
 		s_out$B_WEIGHT <- rep( (nrow(s_frame) / u_rdgrp), nrow(s_out)	) 
  		s_out$B0_WEIGHT <- rep( (nrow(s_frame) / u_rdgrp), nrow(s_out)	) 
 			 for (i in 1:nrow(s_out)){					#assign zero for NA row
				s_out$B0_WEIGHT[	which(is.na(s_out$IDENTITY.STATUS))		] <- 0
								}
		s_out$rdgrp_code <- sample(rep(1:n_rdgrp , u_rdgrp))   #create rdgrp_code column
	      rdgrp 	 <- split(s_out, s_out$rdgrp_code)	   #split data into different random group
 		n_rdgrp = length(rdgrp)
	##Check weight stability
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
	##Weighting Class Adjustment
				#reorder NEW.VARIABLE
			wei_classvar	<- as.factor(unique(NEW.VARIABLE) , levels = c(unique(NEW.VARIABLE)))
					#check if zero elements within class
				#decide_weiclass <- list()	
				#for (i in 1: length(rdgrp)  ){
					 # decide_weiclass[[i]]  <- (!(sort(unique(s_frame$YEAR))   %in% 	sort(unique(rdgrp[[i]]$NEW.VARIABLE)) ))
					 #					}
				#if (any(unlist(decide_weiclass, use.names=F ))){			#is there any class which has zero element?
				#stop("No respondents in certain class; Imputation Recommended")
				#}else{
			 	#}
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
			for (i in 1:length(rdgrp)){			#detect zero sum of weights of respondents
			decide_weiclass2[[i]] <- sum_BW[[i]][[2]]
							}
		if(	any(unlist(decide_weiclass2, use.names=F) == 0)	){	 #is there any sum of weights of respondents equal to zero
			stop("IMPUTATION RECOMMENDED")
				#}else{
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
			   B_WEIGHT2[[i]]<- unlist(F_WEIGHT2, use.names=F )		#assign adjusted B_WEIGHT2 to each random group
						   }
			for (i in 1:length(rdgrp)){						#create F_WEIGHT column
					rdgrp[[i]]$F_WEIGHT <- rdgrp[[i]]$B_WEIGHT * rdgrp[[i]]$B_WEIGHT2
							}
	##Calculate Variance
	  storage_varlvl <- list()
	  for (i in 1:length(rdgrp) ){				#change Identity Status to 1 & 0
	     for (j in 1:length(variable_lvl) ){
			storage_varlvl[[j]]  <- as.numeric(rdgrp[[i]]$IDENTITY.STATUS == variable_lvl[j])
					}
			names(storage_varlvl) <- variable_lvl
			rdgrp[[i]] <- cbind(rdgrp[[i]],as.data.frame(storage_varlvl))
					}
	  for (i in 1:length(rdgrp) ){				#assign 0 to NA rows in 1&0 identity status
		for (j in 1:length(variable_lvl)){
  	 		(select(  rdgrp[[i]], as.character(variable_lvl)  )[,j])[which(is.na(select(  rdgrp[[i]], as.character(variable_lvl)  ) [,j]	))] <- 0
						         }
					   }
	   storage_varlvl2 <- list()
	  for (i in 1:length(rdgrp)){				#calculate F_WEIGHT * yi for each random group
			storage_varlvl2[[i]] <-
		select(rdgrp[[i]], as.character(variable_lvl)) * rdgrp[[i]]$F_WEIGHT
			}
	  tht_r <- list()
	     for (i in 1:length(storage_varlvl2)){
			tht_r[[i]] <-
			as.numeric(apply(storage_varlvl2[[i]],2,sum)) / sum(rdgrp[[i]]$F_WEIGHT)
							}
	  tht_r2 <- do.call(rbind.data.frame, tht_r)
	  colnames(tht_r2) <- as.character(variable_lvl)		#parameter of interest from each random group
	  tht = as.numeric(apply(tht_r2,2,sum)) / n_rdgrp		#average of tht_r for each variable level
		##Estimated Variance
	   srsVAR = rep(NA, length(variable_lvl))
	   for (i in 1:length(variable_lvl)){
		   srsVAR[i] <- (1/(n_rdgrp*(n_rdgrp-1))) * sum((tht_r2[,i] - tht[i])^2)
							}
	   srsVAR
										}