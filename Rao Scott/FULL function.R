
cat( "\n", "Choose the appropriate function" , "\n")
ANS_S <- readline(noquote(("[SAMPLE SELECTION / STATISTICAL TEST] : ")))


	#source all random group function
		source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Random%20Group/SRS%20RANDOM%20GROUP%20ESTIMATION.R ")		#srs
		source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Random%20Group/STRATIFIED%20RANDOM%20GROUP%20ESTIMATION.R")		#stratified
		source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Random%20Group/CLUSTER%20RANDOM%20GROUP%20ESTIMATION.R")		#cluster


full = function(){
	if(ANS_S == "Sample Selection" |ANS_S == "SAMPLE SELECTION" | ANS_S == "Sample selection"  | ANS_S == "sample selection"  ){
								#source sample selection raw code
			source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/sample%20selection(FINAL).R") 				
	}else if ( ANS_S == "STATISTICAL TEST" |ANS_S == "statistical test"  | ANS_S == "Statistical Test"| ANS_S == "Statistical test" ){
								#enter necessary data , columns
			source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Insert%20Neccesary%20information.R")
	 	cat( "\n", "Select your sampling design" , "\n")
		ANS_D <- readline(noquote(("[SRS / STRATIFIED / CLUSTER] : ")))
			if(ANS_D == "SRS" | ANS_D == "srs" | ANS_D == "Srs"){			#ask for the relevant test
				if (  any(is.na(s_out$IDENTITY.STATUS))  ){
					 			#source double phase sampling					
			source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Statistical%20Test/DOUBLE%20PHASE%20EXACT.R ")
				}else{
								#source SRS sampling
			source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Statistical%20Test/SRS%20EXACT.R ")
				     }
			}else if(ANS_D == "STRATIFIED" | ANS_D == "Stratified" | ANS_D == "stratified"){
								#source Stratified sampling
			source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Statistical%20Test/STRATIFIED.R ")
			}else if(ANS_D == "CLUSTER" | ANS_D == "Cluster" | ANS_D == "cluster"){
				print(noquote("How many primary sampling units you've sampled?"))
				n <- as.numeric(readline(noquote("n : ")))
					if (n>2 | max(table(s_frame$DEPT_CODE)) >= nrow(s_frame)/n  ){
								#source CLS with replacement
			source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Statistical%20Test/Cluster%20WR.R ")
					}else{
								#source CLS without replacement
			source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/Rao%20Scott/Statistical%20Test/Cluster%20WOR.R ")
					     }
			}else{			
				stop("Only 3 options available")
			     }
	}else{
		stop("only 2 options available!")
		}
			}


full()

