


DOUBLE = function(s_out,s_frame, strata, variable_lvl){
	#Ask for subsamples
	  cat( "Have you subsample from nonrespondent strata?", "\n" )
	  ANS1 <- readline(noquote("[Yes / No] : "))
	#Enter neccesary Data
	  nonresp_str = subset(s_out, is.na(s_out$IDENTITY.STATUS)) 	#nonresponse strata
	  resp_str = subset(s_out, !is.na(s_out$IDENTITY.STATUS))		#response strata
  if( ANS1 == "No" | ANS1 == "NO" | ANS1 == "no" | ANS1 == "N" | ANS1 == "n"){
		print(noquote("How many samples you wanna select from nonresponse strata?"))
		nsubs_out <- as.numeric(readline(noquote("n = ")))
		x <- t(t(nonresp_str$PSEUDO.ID[sample(nrow(nonresp_str),nsubs_out, replace=F)]))
		m <- x[,-1]
		row.names(m) <- x[, 1]
		print(m)
  }else{											#Proceed to Double Phase Calculation
	#Enter subssample and create neccesary column
		print(noquote("Select your subsample"))
		readline(prompt="Press [enter] to continue")			
		subs_out <- read.csv(file.choose(),head=T, na.strings=c("","NA"))	#read 2nd phase data
		subs_out <- subs_out[, !apply(is.na(subs_out), 2, all)]		#delete NA column
		subs_out <- mutate( subs_out, DEPT_CODE =  as.integer(substr(subs_out$PSEUDO.ID,4,6))   ) 	#create column of DEPT CODE
		subs_out <- mutate( subs_out , YEAR = as.integer(substr(subs_out$PSEUDO.ID,2,3))  ) 		#create YEARS column 
		subs_out <- subs_out[order(subs_out$DEPT_CODE),]
	#Create WEIGHTS COLUMN
		s_out$WEIGHT_1 <- rep(length(s_frame$PSEUDO.ID)/nrow(s_out), nrow(s_out))  		#create weight 1(w1) for 1st phase sample
		resp_str$WEIGHT_1 <- rep(length(s_frame$PSEUDO.ID)/nrow(s_out), nrow(resp_str))	#create weight 1(w1) for 2nd phase sample
		subs_out$WEIGHT_1 <- rep(length(s_frame$PSEUDO.ID)/nrow(s_out),nrow(subs_out)) 	#create weight 1(w1) for 2nd phase subsample
			for (i in 1:nrow(subs_out)){						#combine subs_out and nonresp_str identity status
				nonresp_str$IDENTITY.STATUS[subs_out$PSEUDO.ID[i] == nonresp_str$PSEUDO.ID] <- subs_out$IDENTITY.STATUS[i]
							   }
			for (i in 1:nrow(subs_out)){						#combine subs_out and nonresp_str NEW Variable
				nonresp_str$NEW.VARIABLE[subs_out$PSEUDO.ID[i] == nonresp_str$PSEUDO.ID] <- subs_out$NEW.VARIABLE[i]
							   }
		resp_str$WEIGHT_2 <- rep(nrow(resp_str)/nrow(resp_str), nrow(resp_str))				#create weight 2 for response strata
		subs_out$WEIGHT_2 <- rep(nrow(nonresp_str) / nrow(subs_out), nrow(subs_out))			#create wieght 2 for subsample nonresponse strata
		resp_str$F_WEIGHT <- resp_str$WEIGHT_2 * resp_str$WEIGHT_1
		subs_out$F_WEIGHT <- subs_out$WEIGHT_2 * subs_out$WEIGHT_1
		phase2_data <- rbind(resp_str, subs_out)									    #combine 2nd phase sampling data
		phase2_data <- mutate(phase2_data, YEAR =  as.integer(substr(phase2_data$PSEUDO.ID,2,3))   ) #create column of YEAR
		subs_out <- mutate( subs_out, YEAR =  as.integer(substr(subs_out$PSEUDO.ID,2,3))  )
	#IMPUTATION
	   if ( any(is.na(subs_out$IDENTITY.STATUS))	){			    #Check if imputation is needed
		 print(noquote("NA DETECTED! Imputation is needed"))
		 data_wimp <- subset(subs_out, is.na(subs_out$IDENTITY.STATUS)) #data wait for imputation
	   #Select variable needed
		 print(noquote("SELECT VARIABLE for IMPUTATION"))
		 ANS2 <- readline(noquote( "[YEAR / NEW.VARIABLE / NEW.VARIABLE + YEAR] :  " ))
			if(ANS2 == "YEAR" | ANS2 == "Year" | ANS2 == "year" ){	#Select YEAR as NEW VARIABLE
 				 for (i in 1:nrow(data_wimp) ){
					data_wimp[i,]$IDENTITY.STATUS <- 
  					sample(as.character(	subset(  na.omit(phase2_data), na.omit(phase2_data)$YEAR == data_wimp$YEAR[i]	)$IDENTITY.STATUS  )	)[1]
									}
			}else if(ANS2 == "NEW.VARIABLE" | ANS2 == "New.Variable" | ANS2 == "new.variable" | ANS2 == "new variable" | ANS2 == "New Variable" | ANS2 == "NEW VARIABLE" ){
				 for (i in 1:nrow(data_wimp) ){
					data_wimp[i,]$IDENTITY.STATUS <- 
  					sample(as.character(	subset(  na.omit(phase2_data), na.omit(phase2_data)$NEW.VARIABLE == data_wimp$NEW.VARIABLE[i]	)$IDENTITY.STATUS  )	)[1]
									}																			 
			}else{
				 for (i in 1:nrow(data_wimp) ){
					data_wimp[i,]$IDENTITY.STATUS <- 
					sample(	as.character(	subset(na.omit(phase2_data), na.omit(phase2_data)$YEAR == data_wimp[i,]$YEAR & na.omit(phase2_data)$NEW.VARIABLE ==data_wimp[i,]$NEW.VARIABLE)$IDENTITY.STATUS	)	)[1]
					                        }
			     }
	    #combine data_wimp with phase2_data and subs_out
			phase2_data <- rbind(na.omit(phase2_data),data_wimp)
			subs_out <- rbind(na.omit(subs_out),data_wimp)  #subsample after imputation
	    #Check if IMPUTATION VALID
			if ( any(is.na(phase2_data$IDENTITY.STATUS)) ){
			stop("Try DIFFERENT variable for IMPUTATION!")
			}else{
				}
	   }else{										    #Proceed to direct Calculation without Imputation
		   }
	    #Variance Calculation
		phase2_data$Ach01 <- as.integer(phase2_data$IDENTITY.STATUS == "A"  | phase2_data$IDENTITY.STATUS == "Achievement" | phase2_data$IDENTITY.STATUS == "achievement" | phase2_data$IDENTITY.STATUS == "a" ) 
		phase2_data$Dif01 <- as.integer(phase2_data$IDENTITY.STATUS == "D"  |	phase2_data$IDENTITY.STATUS == "Diffusion" | phase2_data$IDENTITY.STATUS == "diffusion" | phase2_data$IDENTITY.STATUS == "d" ) 
		phase2_data$For01 <- as.integer(phase2_data$IDENTITY.STATUS == "F"  |	phase2_data$IDENTITY.STATUS == "Forclosure" | phase2_data$IDENTITY.STATUS == "forclosure" | phase2_data$IDENTITY.STATUS == "f" ) 
		phase2_data$Mor01 <- as.integer(phase2_data$IDENTITY.STATUS == "M"  |	phase2_data$IDENTITY.STATUS == "Moratorium" | phase2_data$IDENTITY.STATUS == "moratorium" | phase2_data$IDENTITY.STATUS == "m" ) 
		phase2_data$FW_Ach <- phase2_data$Ach01 *phase2_data$F_WEIGHT
		phase2_data$FW_Dif <- phase2_data$Dif01 *phase2_data$F_WEIGHT
		phase2_data$FW_For <- phase2_data$For01 *phase2_data$F_WEIGHT
		phase2_data$FW_Mor <- phase2_data$Mor01 *phase2_data$F_WEIGHT
			#estimated proportion
			 finaldp_p = (1/nrow(s_frame))* c( sum(phase2_data$FW_Ach),	sum(phase2_data$FW_Dif)	,	sum(phase2_data$FW_For)	,	sum(phase2_data$FW_Mor)		)
			 resp_str2 <- phase2_data[which(phase2_data$PSEUDO.ID  %in% resp_str$PSEUDO.ID == TRUE),]	#update resp_str
			 subs_out2 <- phase2_data[which(phase2_data$PSEUDO.ID  %in% subs_out$PSEUDO.ID == TRUE),]	#update subs_out
			 resp_dp = c(sum(resp_str2$Ach01),	sum(resp_str2$Dif01),	sum(resp_str2$For01),sum(resp_str2$Mor01)) / nrow(resp_str2)
			 nonresp_dp = c(sum(subs_out2 $Ach01),sum(subs_out2$Dif01),	sum(subs_out2$For01), sum(subs_out2$Mor01)) / nrow(subs_out2 )
			 dpvar_resp = nrow(resp_str2)*resp_dp*(1-resp_dp)/(nrow(resp_str2)-1)
			 dpvar_nonresp = nrow(subs_out2)*nonresp_dp*(1-nonresp_dp)/(nrow(subs_out2)-1)
			 variance_dpp= ((nrow(resp_str2)-1) / (nrow(s_out) -1)) *
						(dpvar_resp / nrow(s_out))	+
						((nrow(nonresp_str) - 1) / (nrow(s_out)-1)) *
						(dpvar_nonresp/((nrow(subs_out2)/nrow(nonresp_str) )*nrow(s_out)))	+
						(1/(nrow(s_out)-1))*
						((nrow(resp_str2)/nrow(s_out)) * ((resp_dp - finaldp_p)^2)	+
						(nrow(nonresp_str)/nrow(s_out)) * ((nonresp_dp - finaldp_p	)^2))
	    #RAO-SCOTT
		ident_domain <- variable_lvl
		Ec <- nrow(s_frame) / length(ident_domain)			#expected weighted frequency
		Nc <- as.numeric(apply(select(phase2_data, c(FW_Ach,FW_Dif,FW_For,FW_Mor)),2,sum))  #sum F_WEIGHT for cell c
		Pc <- Nc / nrow(s_frame)
		Qp <- (nrow(phase2_data)/nrow(s_frame)) * sum( ( (Nc - Ec)^2 ) / Ec )
		deffsrs_p = (as.numeric(apply(select(phase2_data, c(Ach01,Dif01,For01,Mor01)),2,sum)))/ nrow(phase2_data)
		deffsrs_var = (1- nrow(phase2_data)/nrow(s_frame))  *  deffsrs_p  *  (1-deffsrs_p)  / (  nrow(phase2_data)-1  )
		deff= variance_dpp / deffsrs_var
		D_rao <- sum( 	( 1- Pc )  *    (deff  /    (  length(ident_domain) - 1 ) )		 )
		X_RAO <- Qp / (D_rao)
		print(X_RAO)
			}
									}




DOUBLE(s_out,s_frame, strata, variable_lvl)
