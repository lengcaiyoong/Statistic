	#insert data
print(noquote("Select your sampling frame"))
readline(prompt="Press [enter] to continue")	
s_frame <- read.csv(file.choose(),head=T, na.strings=c("","NA"))	#read sampling frame


print(noquote("Select your sample data"))
readline(prompt="Press [enter] to continue")	
s_out <- read.csv(file.choose(),head=T, na.strings=c("","NA"))	#read sample data


	#install and activate packages
suppressWarnings(install.packages("dplyr"))
suppressWarnings(library(dplyr))
suppressWarnings(install.packages("DescTools"))
suppressWarnings(library(DescTools))
suppressWarnings(install.packages("sampling"))
suppressWarnings(library(sampling))
clc <- function() cat(rep("\n", 50))
clc()



	#create neccesary column
s_frame <- mutate(s_frame, DEPT_CODE =  as.integer(substr(s_frame$PSEUDO.ID,4,6))   ) #create column of DEPT CODE
s_frame   <- mutate( s_frame , YEAR = as.integer(substr(s_frame$PSEUDO.ID,2,3))  ) #create YEARS column in s_frame
s_out <- mutate(s_out, DEPT_CODE =  as.integer(substr(s_out$PSEUDO.ID,4,6))   ) #create column of DEPT CODE
s_out   <- mutate( s_out , YEAR = as.integer(substr(s_out$PSEUDO.ID,2,3))  ) #create YEARS column 
strata <- sort(unique(s_out$DEPT_CODE))



	#sort s_out according to DEPT_CODE
s_out_clswr <- s_out
s_out <- s_out[order(s_out$DEPT_CODE),]



	#ask to insert group variable
print(noquote("How many levels of variable are there?"))
n_variablelvl <- as.numeric(readline(noquote("Number of levels: ")))

print(noquote("Enter them one by one"))

variablelvl <- rep(NA,n_variablelvl)

for (i in 1:n_variablelvl){
	variablelvl[i] <- readline(noquote("Enter Variable Levels:"))
				}

variable_lvl <- factor(variablelvl, levels=c(variablelvl))




	#set factor order
s_out$IDENTITY.STATUS <- factor(s_out$IDENTITY.STATUS, levels = variable_lvl)




