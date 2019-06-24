#RGVAR_CLS(s_out_clswr,s_frame, strata, variable_lvl,n)




RGVAR_CLS = function(s_out,s_frame, strata, variable_lvl,n){
	#Define Group Numbers n_rdgrp
		is.prime <- function(num) {				#check for prime number
  			 if (num == 2) {
			      TRUE
		   } else if (any(num %% 2:(num-1) == 0)) {
			      FALSE
			   } else { 
			      TRUE
				   }
				}
		if (is.prime(n)){
			n_rdgrp <- n
			}else{
		print(noquote("SELECT number of random groups you wanna create"))
		for(i in 1:n){
			if((n %% i) == 0) {
			print(i)
				}
				}
		n_rdgrp = readline(noquote("NUMBER OF RANDOM GROUP:	"))
				}
	#Split
		n_cluster = n / n_rdgrp					#number of cluster in each random group
		cluster_strata = sort(unique(s_out$DEPT_CODE))  #cluster in samples
		s_out$clscode <- sort(rep(1:n, nrow(s_out) / n ))
		clscode_storage <- split(s_out, s_out$clscode )
	#Create BASE WEIGHT	
		for (i in 1: length(clscode_storage)){	#create base weights column
				clscode_storage[[i]]$B_WEIGHT <-
				rep(	nrow(s_frame)/ ( n_cluster * nrow(clscode_storage[[i]]) )	, nrow(clscode_storage[[i]]) )
									 }
		for (i in 1: length(clscode_storage)){		#create base0 weights column
			clscode_storage[[i]]$B0_WEIGHT <-
				rep(	nrow(s_frame)/ ( n_cluster * nrow(clscode_storage[[i]]) )	, nrow(clscode_storage[[i]]) )
								}
		for (i in 1:length(clscode_storage)){					#assign zero for NA row
				clscode_storage[[i]]$B0_WEIGHT[	which(is.na(clscode_storage[[i]]$IDENTITY.STATUS))		] <- 0
						 }
		#Create Random Group
			cls_stor = list()				 
			n1 <- 1:n					
		  for (i in 1:n_rdgrp){			#randomly sample n_rdgrp times by n_cluster
		     cls_stor[[i]] <- sample(n1, replace=F, n_cluster)
				n1 <- n1[! n1 %in% cls_stor[[i]]]
				  	     }
		 	rdgrp = list()
		  for (i in 1:n_rdgrp){
		      rdgrp[[i]] <- clscode_storage[n1[[i]]]
						}
		#Check weight stability
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
			wei_classvar	<- as.factor(unique(NEW.VARIABLE) , levels = c(unique(NEW.VARIABLE)))
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
			for (i in 1:length(rdgrp)){			#detect zero sum of weights of respondents
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
			   B_WEIGHT2[[i]]<- unlist(F_WEIGHT2, use.names=F )		#assign adjusted B_WEIGHT2 to each random group
						   }
			for (i in 1:length(rdgrp)){						#create F_WEIGHT column
					rdgrp[[i]]$F_WEIGHT <- rdgrp[[i]]$B_WEIGHT * rdgrp[[i]]$B_WEIGHT2
							}
		#Calculate Variance CLS
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
		clsVAR = rep(NA, length(variable_lvl))
		for (i in 1:length(variable_lvl)){
				clsVAR[i] <- (1/(n_rdgrp*(n_rdgrp-1))) * sum((tht_r2[,i] - tht[i])^2)
							   }
		clsVAR
			   						   }

