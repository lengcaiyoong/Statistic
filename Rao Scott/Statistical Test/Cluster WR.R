

CLSWR = function(s_out,s_frame, strata, variable_lvl, pn, s_out_rgsrs,s_out_clswr){
	#Print Warning Message
		print(noquote("This is valid only if sample sizes are equal amongst cluster"))
		readline(prompt="Press [enter] to continue")
	#Ask for n
		#print(noquote("How many primary sampling units you've sampled?"))
		n <- pn
	#Any Non-response? 
	  	if(  any(is.na(s_out$IDENTITY.STATUS))  ){
			print(noquote("SELECT NONRESPONSE ADJUSTMENT"))
			ANS1 <- readline(noquote("[WEIGHTING CLASS / IMPUTATION] : "))
				} else {
		      		 }
	if (ANS1 == "WEIGHTING CLASS" | ANS1 == "Weighting Class" | ANS1 == "weighting class" ){
		##Create BASE WEIGHT
			s_out$BASE_WEIGHT <- NA					#create B_WEIGHT column
			s_out$BASE0_WEIGHT <- NA				#create BASE0_WEIGHT column (with NA row equals to zero)
			n_cluster = n 						#number of psus selected
			cluster_strata = sort(unique(s_out$DEPT_CODE))  #reorder department code
				for (i in 1:nrow(s_out) ){				#assign BASE_WEIGHT for each row
					   s_out$BASE_WEIGHT[i] <- 
						nrow(s_frame) /
						  (n_cluster*
						   as.numeric(	table(s_out$DEPT_CODE)	)	[	which(s_out$DEPT_CODE[i] == cluster_strata)	]
									   )
								  }
				for (i in 1:nrow(s_out) ){				#assign B0_WEIGHT for each row
					   s_out$BASE0_WEIGHT[i] <- 
						nrow(s_frame) /
							  (n_cluster*
						   as.numeric(	table(s_out$DEPT_CODE)	)	[	which(s_out$DEPT_CODE[i] == cluster_strata)	]
				   					)
								  }
		 		 for (i in 1:nrow(s_out)){				#assign zero for NA row
						s_out$BASE0_WEIGHT[	which(is.na(s_out$IDENTITY.STATUS))		] <- 0
								 }
		  ##Check Stability of WEIGHT
			stability_list1 <- rep(NA, length(unique(s_out$NEW.VARIABLE))  )
			stability2 <- split(s_out, s_out$NEW.VARIABLE)
				for (i in 1:length(unique(s_out$NEW.VARIABLE) ) ){
						stability_list1[[i]] <- 
						length(which (is.na(stability2[[i]]$IDENTITY.STATUS ))) / nrow(stability2[[i]]) >= 0.5
								}
				if ( any(stability_list1) ){
					stop("IMPUTATION RECOMMENDED")
						}else{
						     }
		   ##Weighting Class Adjustment
						#reorder strata
				wei_classvar	<- factor(unique(s_out$NEW.VARIABLE) , levels = unique(s_out$NEW.VARIABLE)  )
						#create F_WEIGHT column
					 for (i in 1:nrow(s_out)){			#assign zero to NA row						#assign zero for NA row
						s_out$BASE0_WEIGHT[	which(is.na(s_out$IDENTITY.STATUS))		] <- 0
								 	 }
						#Calculate Final Weight
			 by_nwvar <- group_by(s_out, NEW.VARIABLE)       			#group s_out by New variable
			 sum_BW <- summarize(by_nwvar, sum(BASE_WEIGHT))	 		#sum all base weight in each class
			 sum_BW0 <- summarize(by_nwvar, sum(BASE0_WEIGHT))	 		#sum all base weight in each class
			 FINAL_WEIGHT = (sum_BW[[2]]) / (sum_BW0[[2]])				#final weight stored in vector in NEW.VARIABLE order
			 cls_nrsp_storage = rep(NA,nrow(s_out))
				for (i in 1:nrow(s_out)){						#for loop assign each row with Fweight
					cls_nrsp_storage[i] <- FINAL_WEIGHT [ which( wei_classvar == s_out$NEW.VARIABLE[i] )	 ]
								}
			 s_out$FINAL_WEIGHT <- cls_nrsp_storage* s_out$BASE0_WEIGHT
						#check if sum of weight of respondents is zero
				if(any(sum_BW0[[2]] == 0) ){
					stop("Try Different Variable for Weighting Class Adjustment!")
								   }else{
									  }
		    ##Change Identity Status To 1 & 0
			  cls_storage_varlvl <- list()
				for (i in 1:length(variable_lvl) ){			#assign 1 & 0 to Identity Status
					cls_storage_varlvl[[i]] <- as.numeric(s_out$IDENTITY.STATUS == variable_lvl[i])
					cls_storage_varlvl[[i]] [which(is.na(cls_storage_varlvl[[i]])) ] <- 0	#assign 0 to NA rows in 1&0 identity status
										}
			  s_out <- cbind(	s_out , as.data.frame(cls_storage_varlvl, col.names=variable_lvl))
		     ##Calculate Nc
			   Nc <- as.numeric(	apply(   (select(s_out, as.character(variable_lvl)) 	* s_out$FINAL_WEIGHT) ,2 , sum)	)	#Nc in Rao adjustment
		     ##Random Group Variance
			  deff <- RGVAR_CLS(s_out_clswr,s_frame, strata, variable_lvl,n,s_out_rgsrs)
			  #srsVAR <- RGVAR_SRS(s_out,s_frame, strata, variable_lvl)
	}else{
			##IMPUTATION##
		if(  ANS1 == "IMPUTATION" | ANS1 == "Imputation" | ANS1 == "imputation"  ){
	 		 	 	#Select variable needed
			 print(noquote("SELECT VARIABLE for IMPUTATION"))
			 ANS2 <- readline(noquote( "[YEAR / NEW.VARIABLE / NEW.VARIABLE + YEAR] :  " ))
			 data_wimp <- subset(s_out, is.na(s_out$IDENTITY.STATUS)) #NA rows in s_out 
				if(ANS2 == "YEAR" | ANS2 == "Year" | ANS2 == "year" ){	#Select YEAR as NEW VARIABLE
 						for (i in 1:nrow(data_wimp) ){
							data_wimp[i,]$IDENTITY.STATUS <- 
   							sample(as.character(	subset(  na.omit(s_out), na.omit(s_out)$YEAR == data_wimp$YEAR[i]	)$IDENTITY.STATUS  )	)[1]
											}
				}else if(ANS2 == "NEW.VARIABLE" | ANS2 == "New.Variable" | ANS2 == "new.variable" | ANS2 == "new variable" | ANS2 == "New Variable" | ANS2 == "NEW VARIABLE" ){
					 for (i in 1:nrow(data_wimp) ){
						data_wimp[i,]$IDENTITY.STATUS <- 
	  					sample(as.character(	subset(  na.omit(s_out), na.omit(s_out)$NEW.VARIABLE == data_wimp$NEW.VARIABLE[i]	)$IDENTITY.STATUS  )	)[1]
									}
 				}else{
				 		 for (i in 1:nrow(data_wimp) ){
						 	data_wimp[i,]$IDENTITY.STATUS <- 
						      sample(	as.character(	subset(na.omit(s_out), na.omit(s_out)$YEAR == data_wimp[i,]$YEAR & na.omit(s_out)$NEW.VARIABLE ==data_wimp[i,]$NEW.VARIABLE)$IDENTITY.STATUS	)	)[1]
											}
					}
			s_out <- rbind(na.omit(s_out),data_wimp)		#combine data_wimp and s_out
						#check if IMPUTATION VALID
					if ( any(is.na(s_out$IDENTITY.STATUS)) ){
						stop("Try DIFFERENT variable for IMPUTATION!")
						}else{
						     }
		}else{		
			}			#IMPUTATION & MCAR END
						###Cont. Exact Formula###
			##Create BASE_WEIGHT column
			  s_out$BASE_WEIGHT <- NA					#create B_WEIGHT column
			  n_cluster <- n 						#number of psus selected
			  cluster_strata = sort(unique(s_out$DEPT_CODE))  #reorder department code
 		 		 for (i in 1:nrow(s_out) ){				#assign BASE_WEIGHT for each row
			 		  s_out$BASE_WEIGHT[i] <- 
						nrow(s_frame) /
				 		 (n_cluster*
				 		  as.numeric(	table(s_out$DEPT_CODE)	)	[	which(s_out$DEPT_CODE[i] == cluster_strata)	]
				 				  )
				 				   }
			##Create Final_WEIGHT
		 	 s_out$FINAL_WEIGHT <- s_out$BASE_WEIGHT
			##Nc for Rao Adjustment
				cls_storage_varlvl <- list()
					for (i in 1:length(variable_lvl) ){			#assign 1 & 0 to Identity Status
						cls_storage_varlvl[[i]] <- as.numeric(s_out$IDENTITY.STATUS == variable_lvl[i])
						cls_storage_varlvl[[i]] [which(is.na(cls_storage_varlvl[[i]])) ] <- 0	#assign 0 to NA rows in 1&0 identity status
											}
				s_out <- cbind(	s_out , as.data.frame(cls_storage_varlvl, col.names=variable_lvl))
				Nc <- as.numeric(	apply(   (select(s_out, as.character(variable_lvl)) 	* s_out$FINAL_WEIGHT) ,2 , sum)	)	#Nc in Rao adjustment
			##Calculate Variance
				cls_mi <- as.numeric(table(s_out$DEPT_CODE))	
				cls_Mi <- as.numeric(table(s_frame$DEPT_CODE)) [which( sort(unique(s_frame$DEPT_CODE)) %in% strata)]
				cls_Mo <- nrow(s_frame)	
				cls_pie <- cls_mi / cls_Mo			
			##Calculate cls_tij
				cls_tij_storage1 <- na.omit(select(s_out, c(as.character(variable_lvl), DEPT_CODE))		)
				cls_tij_storage1 <- cls_tij_storage1[order(sort(cls_tij_storage1$DEPT_CODE)),]	#reorder 
				cls_tij_storage2 <- split (
					select(cls_tij_storage1, c(as.character(variable_lvl), DEPT_CODE)) , cls_tij_storage1$DEPT_CODE
									)
				cls_tij <- list()
		 			 for (i in 1:length(cls_tij_storage2)) {
						cls_tij[[i]] <-
						apply(select(cls_tij_storage2[[i]], as.character(variable_lvl)), 2 , sum) *
						(cls_Mi / cls_mi)[i]
											}
				cls_tij <- do.call(rbind.data.frame, cls_tij )		#convert list to data frame, with length(variable_lvl) column
			##calculate variance (with replacement)
				v_clswr <- function(){
					result = rep(NA , length(variable_lvl) )
					for (i in 1:length(variable_lvl) ){
						result[i] <-
						(1/(( ((1/n)  * sum(cls_Mi/cls_pie))^2 )*  n*  (n-1))) * 
						sum(		( (1/cls_pie)* ( cls_tij[,i] - cls_Mi* (  ((1/n) * sum(cls_tij[,i]/cls_pie)) /  ((1/n)* sum(cls_Mi/cls_pie))     ) )  )		^2)
											}
							return(result)
								}
				clsVAR <- v_clswr()
			##Calculate srsVAR
				n_srs = nrow(s_out)
				N_srs = nrow(s_frame)
				pro_srs <- as.numeric(apply(select(s_out,  as.character(variable_lvl)	) , 2 , sum)) / nrow(s_out)
				srsVAR1 <- function(n_srs,N_srs,pro_srs){
				result <- (1-n_srs/N_srs)*(pro_srs*(1-pro_srs))/(n_srs-1)
					return(result)
						}
				srsVAR <- srsVAR1(n_srs,N_srs,pro_srs)
			##Calculate deff
				deff = clsVAR/srsVAR
  	      }	#end of Nc & Variance calculation
	##calculate RAO SCOTT
		Ec = nrow(s_frame) / length(variable_lvl)
		Pc = Nc/nrow(s_frame)
		Qp = (nrow(s_out)/nrow(s_frame)) * sum( ( (Nc - Ec)^2 ) / Ec )	
		D_rao <- sum( 	( 1- Pc )  *    (deff  /    (length(variable_lvl)-1) )		 )
		X_RAO <- Qp / (D_rao)
		print(X_RAO)
									}




CLSWR(s_out,s_frame, strata, variable_lvl, pn, s_out_rgsrs,s_out_clswr)
