


STR = function(s_out,s_frame, strata, variable_lvl,s_out_rgsrs,s_out_rgstr){
	print(noquote("This is valid only if equal sample size amongst strata"))
	readline(prompt="Press [enter] to continue")
	###Any Non-response? 
	  	if(  any(is.na(s_out$IDENTITY.STATUS))  ){
			print(noquote("SELECT NONRESPONSE ADJUSTMENT"))
			ANS1 <- readline(noquote("[WEIGHTING CLASS / IMPUTATION ] : "))
				} else {
		      		 }
	  if (ANS1 == "WEIGHTING CLASS" | ANS1 == "Weighting Class" | ANS1 == "weighting class") {
			#Create BASE WEIGHT
						#as.numeric(table(s_out$DEPT_CODE))			#n_h
						#as.numeric(table(s_frame$DEPT_CODE))		#N_h
			BASE_WEIGHT 	=   ( as.numeric(table(s_frame$DEPT_CODE))) / (	as.numeric(table(s_out$DEPT_CODE))	)
			s_out$BASE_WEIGHT <-  rep(BASE_WEIGHT, as.numeric(table(s_out$DEPT_CODE)))	
			s_out$BASE0_WEIGHT <-  rep(BASE_WEIGHT, as.numeric(table(s_out$DEPT_CODE))	)	
						#Check Stability of WEIGHT
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
			#Weighting Class Adjustment
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
			 str_nrsp_storage = rep(NA,nrow(s_out))
				for (i in 1:nrow(s_out)){						#for loop assign each row with Fweight
					str_nrsp_storage[i] <- FINAL_WEIGHT [ which( wei_classvar == s_out$NEW.VARIABLE[i] )	 ]
								}
			 s_out$FINAL_WEIGHT <- str_nrsp_storage* s_out$BASE0_WEIGHT
						#check if sum of weight of respondents is zero
				if(any(sum_BW0[[2]] == 0) ){
					stop("Try Different Variable for Weighting Class Adjustment!")
								   }
		    ##Change Identity Status To 1 & 0
			  str_storage_varlvl <- list()
				for (i in 1:length(variable_lvl) ){			#assign 1 & 0 to Identity Status
					str_storage_varlvl[[i]] <- as.numeric(s_out$IDENTITY.STATUS == variable_lvl[i])
					str_storage_varlvl[[i]] [which(is.na(str_storage_varlvl[[i]])) ] <- 0	#assign 0 to NA rows in 1&0 identity status
										}
			  s_out <- cbind(	s_out , as.data.frame(str_storage_varlvl, col.names=variable_lvl))
			#Calculate Nc
			   Nc <- as.numeric(	apply(   (select(s_out, as.character(variable_lvl)) 	* s_out$FINAL_WEIGHT) ,2 , sum)	)	#Nc in Rao adjustment
			#Random Group Variance
			   deff <- RGVAR_STR(s_out_rgstr,s_frame, strata, variable_lvl,s_out_rgsrs)
			  		#strVAR <- RGVAR_STR(s_out,s_frame, strata, variable_lvl)
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
		#}else if( ANS1 == "MCAR" | ANS1 == "mcar" | ANS1 == "Mcar"){
			#s_out <- na.omit(s_out)		
		     }else{
			     }	#IMPUTATION & MCAR END
		##continue EXACT FORMULA
				###Create BASE weight and Final Weight##
			  		 #as.numeric(table(s_out$DEPT_CODE))		#n_h
			   		 #as.numeric(table(s_frame$DEPT_CODE))		#N_h
		BASE_WEIGHT 	=   ( as.numeric(table(s_frame$DEPT_CODE))) / (	as.numeric(table(s_out$DEPT_CODE))	)
		str_bw_storage = list()
			for (i in 1:length(strata)){
				str_bw_storage[[i]] <- rep(BASE_WEIGHT[i], as.numeric(table(s_out$DEPT_CODE))[i])
							   }
		s_out$BASE_WEIGHT <-   unlist(str_bw_storage, use.names=FALSE)
		s_out$FINAL_WEIGHT <- s_out$BASE_WEIGHT
			###Convert IDENTITY STATUS to 1 and 0
		str_storage_varlvl <- list()
		for (i in 1:length(variable_lvl) ){			#assign 1 & 0 to Identity Status
			str_storage_varlvl[[i]] <- as.numeric(s_out$IDENTITY.STATUS == variable_lvl[i])
			str_storage_varlvl[[i]] [which(is.na(str_storage_varlvl[[i]])) ] <- 0	#assign 0 to NA rows in 1&0 identity status
			}
		s_out <- cbind(	s_out , as.data.frame(str_storage_varlvl, col.names=variable_lvl))
			###Variance calculation
		varstr_Nh = as.numeric(table(s_frame$DEPT_CODE))				#N_h
		varstr_nh = as.numeric( table(s_out$DEPT_CODE) ) 				#n_h
		varstr_ph = as.numeric(table(s_out$IDENTITY.STATUS)) / nrow(s_out)	#p
		strVAR = rep(NA, length(variable_lvl))
			for (i in 1:length(variable_lvl)){						#calculate variance for each variable level
				strVAR[i]	<- sum((1-varstr_nh/varstr_Nh)*((varstr_Nh/sum(varstr_Nh))^2)*(varstr_ph[i]*(1-varstr_ph[i]))/(varstr_nh -1))
								   }
			###Calculate EXACT srsVAR
		n_srs = nrow(s_out)
		N_srs = nrow(s_frame)
		pro_srs <- as.numeric(apply(select(s_out,  as.character(variable_lvl)	) , 2 , sum)) / nrow(s_out)
			srsVAR1 <- function(n_srs,N_srs,pro_srs){
			result <- (1-n_srs/N_srs)*(pro_srs*(1-pro_srs))/(n_srs-1)
				return(result)
					}
		srsVAR <- srsVAR1(n_srs,N_srs,pro_srs)
			###Calculate Nc
		Nc <- as.numeric(	apply(   (select(s_out, as.character(variable_lvl)) 	* s_out$FINAL_WEIGHT) ,2 , sum)	)	#Nc in Rao adjustment
		    }
	###Calculate Rao Scott
		#deff = strVAR/srsVAR	#imputation-exact variance OR random group method
		Ec = nrow(s_frame) / length(variable_lvl)
		Pc = Nc/nrow(s_frame)
		Qp = (nrow(s_out)/nrow(s_frame)) * sum( ( (Nc - Ec)^2 ) / Ec )	
		D_rao <- sum( 	( 1- Pc )  *    (deff  /    (length(variable_lvl)-1) )		 )
		X_RAO <- Qp / (D_rao)
		X_RAO
	  		         				  }


STR(s_out,s_frame, strata, variable_lvl,s_out_rgsrs,s_out_rgstr)

