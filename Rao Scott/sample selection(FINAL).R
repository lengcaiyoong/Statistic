


print(noquote("Select your sampling frame"))
readline(prompt="Press [enter] to continue")
s_frame <- read.csv(file.choose(),head=T)



suppressWarnings(install.packages("dplyr"))
suppressWarnings(library(dplyr))
suppressWarnings(install.packages("sampling"))
suppressWarnings(library(sampling))




	# as.integer(substr(s_frame$PSEUDO.ID,4,6)) #extract only department code
s_frame <- mutate(s_frame, DEPT_CODE =  as.integer(substr(s_frame$PSEUDO.ID,4,6))   ) #create a new list of DEPT CODE
uni <- sort(unique(s_frame$DEPT_CODE))




S_SRS <- function() {
	print(noquote("How many samples you wanna select?"))
	n <- as.numeric(readline(noquote("n = ")))
	x <- t(t(s_frame$PSEUDO.ID[sample(nrow(s_frame),n, replace=F)]))
	m <- x[,-1]
	row.names(m) <- x[, 1]
	print(m)
			  }




S_STR <- function() {
	print(noquote("How many samples (ie. observations in total) you wanna have?"))
	n <- as.numeric(readline(noquote("n = ")))
	storage <- list()
		for(i in 1:length(uni)){
			storage[[i]] <- sample(filter(s_frame, DEPT_CODE == uni[i]) $PSEUDO.ID, n/length(uni),replace=F)
				}
		x1 = t(t(unlist(storage)))
		m1 <- x1[,-1]
		row.names(m1) <- x1[, 1]
		print(m1)
	if(nrow(m1)!= n){
		cat("\n", "WARNING: TOTAL NUMBER of observations have been deducted to" , nrow(m1) ,"\n","\n")
			    }
			}




CLS_SAMP <- function(){
		print(noquote("This is two-stage cluster sampling!"))
			readline(prompt="Press [enter] to continue")
		print(noquote("How many primary units you wanna select?"))
				n <- as.numeric(readline(noquote("n = ")))
		print(noquote("How many secondary units IN EACH CLUSTER you wanna have?"))
				mi <- as.numeric(readline(noquote("mi = ")))
		if ( n>2 | max(table(s_frame$DEPT_CODE)) >= nrow(s_frame)/n  )
		{					#LAHIRI METHOD
		  cls_storage <- rep(NA,n)
		  j=1
		  while( anyNA(cls_storage) ){						#first stage sampling
		  	pri_Lah <- sample(1:length(uni),1)
		  	sec_Lah <- sample(1:max(table(s_frame$DEPT_CODE)),1)
		     if (  sec_Lah <= table( s_frame$DEPT_CODE)[[pri_Lah]]  ){
	  			cls_storage[j] <- pri_Lah
				j <- j+1
					} else {
			cls_storage[j] <- NA
			    }
				}
		  cls_storage2 <- list()
		  for(i in 1:length(cls_storage)){						#second stage random sampling
		     cls_storage2 [[i]] <- sample(filter(s_frame, DEPT_CODE == uni[cls_storage[i]]) $PSEUDO.ID, mi,replace=F)	
			}
			x2 = t(t(unlist(cls_storage2)))
			m2 <- x2[,-1]
			row.names(m2) <- x2[, 1]
			print(m2)
			cat( "REMINDER:", "\n", "Please enter these ID into template in the SAME ORDER!", "\n", "Please REMEMBER how many primary units(n) have selected!",  "\n")
		}else if ( n <= 1) {
			stop("at least 2 CLUSTERS")
		}else{
							#BREWER METHOD
		  	pik = as.numeric(table(s_frame$DEPT_CODE)) / (nrow(s_frame)/n)
			s=UPbrewer(pik)
			bre_storage <- (1:length(pik))[s==1] 		#first stage sampling
			bre_storage2 <- list()
			for(i in 1:length(bre_storage)){				#second stage random sampling
			bre_storage2 [[i]] <- sample(filter(s_frame, DEPT_CODE == uni[bre_storage[i]]) $PSEUDO.ID, mi,replace=F)	
					}
			x3 = t(t(unlist(bre_storage2)))
			m3 <- x3[,-1]
			row.names(m3) <- x3[, 1]
			print(m3)
			}
				}





Samp_slct <- function() {
	S_METHOD <- readline(prompt= " Select a sampling method [SRS, STRATIFIED, CLUSTER]:  "  )  
	if (S_METHOD == "SRS" | S_METHOD == "srs" | S_METHOD == "Srs")
	S_SRS()
	else if (S_METHOD == "STRATIFIED" | S_METHOD == "Stratified" | S_METHOD == "stratified")
	S_STR()
	else if (S_METHOD == "CLUSTER" | S_METHOD == "Cluster" | S_METHOD == "cluster")
	CLS_SAMP()
	else
	stop("Only 3 options available!")
		}


Samp_slct()



