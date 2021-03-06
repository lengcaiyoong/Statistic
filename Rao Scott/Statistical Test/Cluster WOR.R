



CLSWOR = function(s_out,s_frame, strata, variable_lvl, pn){
	#Print Warning Message
		print(noquote("This is valid only if sample sizes are equal amongst cluster"))
		readline(prompt="Press [enter] to continue")
	#Ask for n	
		#print(noquote("How many primary sampling units you've sampled?"))
		n <- pn
	#Any Nonresponse? (IMPUTATION)
		if(  any(is.na(s_out$IDENTITY.STATUS))  ){
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
		}else{		#Do nothing
		     }		#IMPUTATION END
	#create BASE_WEIGHT column and Final WEIGHT
		cls_mi <- as.numeric(table(s_out$DEPT_CODE))	
		cls_Mi <- as.numeric(table(s_frame$DEPT_CODE)) [which( sort(unique(s_frame$DEPT_CODE)) %in% strata)]
		cls_Mo <- nrow(s_frame)	
		clsmi_storage = list()
		clsmi_storage2 <- split(s_out, s_out$DEPT_CODE)
		cls_wij <- rep(NA, length(clsmi_storage2)) 
		  cls_pie <- cls_Mi / (cls_Mo/n)			
		     for (i in 1:length(clsmi_storage2)){				#calculate Base Weight 
	  			clsmi_storage2[[i]]$BASE0_WEIGHT <- rep(cls_Mo / (n*nrow(clsmi_storage2[[i]])) , nrow(clsmi_storage2[[i]]))
				  cls_wij[i] <- cls_Mo / (n*nrow(clsmi_storage2[[i]]))
					}
		s_out <- do.call(rbind.data.frame, clsmi_storage2)	#create BASE WEIGHT column
		s_out$FINAL_WEIGHT <- s_out$BASE0_WEIGHT			#create FINAL WEIGHT
	#Nc for Rao Adjustment
		cls_storage_varlvl <- list()
			for (i in 1:length(variable_lvl) ){			#assign 1 & 0 to Identity Status
				cls_storage_varlvl[[i]] <- as.numeric(s_out$IDENTITY.STATUS == variable_lvl[i])
				cls_storage_varlvl[[i]] [which(is.na(cls_storage_varlvl[[i]])) ] <- 0	#assign 0 to NA rows in 1&0 identity status
			}
		s_out <- cbind(	s_out , as.data.frame(cls_storage_varlvl, col.names=variable_lvl))
		Nc <- as.numeric(	apply(   (select(s_out, as.character(variable_lvl)) 	* s_out$FINAL_WEIGHT) ,2 , sum)	)	#Nc in Rao adjustment
	#Caculate variance
		clsmi_storage3 <- split(s_out, s_out$DEPT_CODE)
			for (i in 1:length(clsmi_storage3)){			#calculate cls_yi 
				clsmi_storage[[i]] <- 
				apply(  select( clsmi_storage3[[i]] , as.character(variable_lvl)  )  , 2, sum)
							}
		cls_yi <- do.call(rbind.data.frame, clsmi_storage)			#cls_yi for each variable level 
		cls_yht <-list()
		cls_yht2 <- list()
			for (i in 1:length(variable_lvl) ){ 				#calculate y_ht for each variable level
				for (j in 1:length(unique(s_out$DEPT_CODE))){
					cls_yht2[[j]]	<- cls_wij[j] * cls_yi[,i][j]
						  					  }
					cls_yht[[i]] <- do.call(sum, cls_yht2) / sum(s_out$BASE0_WEIGHT)
								    }
		cls_yht <- unlist(cls_yht , use.names=F)
		v_clswor <- function(){
			result = rep(NA , length(variable_lvl) )
				for (i in 1:length(variable_lvl) ){
					result[i] <-(n/(n-1))* sum(  (    cls_wij* (cls_yi[,i] - cls_mi*cls_yht[i])     /  (cls_mi*n*cls_wij) )^2  )
									    }
					return(result)
					    }
		clsVAR <- v_clswor()
	#Calculate srsVAR
		n_srs = nrow(s_out)
		N_srs = nrow(s_frame)
		pro_srs <- as.numeric(apply(select(s_out,  as.character(variable_lvl)	) , 2 , sum)) / nrow(s_out)
			srsVAR1 <- function(n_srs,N_srs,pro_srs){
			result <- (1-n_srs/N_srs)*(pro_srs*(1-pro_srs))/(n_srs-1)
				return(result)
					}
		srsVAR <- srsVAR1(n_srs,N_srs,pro_srs)
	#calculate RAO SCOTT
		deff = clsVAR/srsVAR	#imputation-exact variance OR random group method
		Ec = nrow(s_frame) / length(variable_lvl)
		Pc = Nc/nrow(s_frame)
		Qp = (nrow(s_out)/nrow(s_frame)) * sum( ( (Nc - Ec)^2 ) / Ec )	
		D_rao <- sum( 	( 1- Pc )  *    (deff  /    (length(variable_lvl)-1) )		 )
		X_RAO <- Qp / (D_rao)
  ###check if zero variance calculated (applied RANDOM GROUP METHOD)
	if(any(clsVAR == 0)){
			stop("Found ZERO variance! Secondary sampling units are too small!")
	}else{
	     }
		print(X_RAO)
									}




CLSWOR(s_out,s_frame, strata, variable_lvl, pn)

