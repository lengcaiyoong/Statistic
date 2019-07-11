

#Enter DATA
    dat_1504 <- read.table(url("http://www.uvm.edu/~dhowell/methods8/DataFiles/Ex15-4.dat"), head=T)
    head(dat_1504)  #different variables on Job Satisfication  
    

    
    
    
    
######Diagnose
	#influential points
	#normality
  #overview
        
    
    fit_1504 <- lm( Satisf ~ . , data=dat_1504 )
    
    diagnose_1504 <- cbind(fit_1504$residuals, rstudent(fit_1504), hatvalues(fit_1504),cooks.distance(fit_1504))
    colnames(diagnose_1504) <- c("Residuals", "Studentized", "Hat", "Cook")
    
  #influential points
    par(mfrow=c(1,3))
    plot(fit_1504,pch=18,col="red",which=c(4) )
    round(diagnose_1504,4)
    

  #normality
      #multivariate normal
    par(mfrow=c(1,6))
    mapply(hist,dat_1504,round(sqrt(nrow(dat_1504))),main=colnames(dat_1504))
    boxplot(dat_1504)

    

              
      #residuals normality
    par(mfrow=c(1,3))
    qqnorm(rstandard(fit_1504))
    qqline(rstandard(fit_1504),col="red")

    source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/QQ-plot%20test.R") #qqplot test

  #overview
      #scatterplot
    plot(dat_1504)
    
      #residuals, qqplot and influential points
    par(mfrow=c(1,4))
    plot(fit_1504)
  
      #all graph
    par(mfrow=c(4,5))
    mapply(hist,dat_1504,round(sqrt(nrow(dat_1504))),main=colnames(dat_1504))
    apply(dat_1504,2,function(x){qqnorm(x);qqline(x)})
    
    for (i in 1:5){
      plot(dat_1504[,c(1,i)],ylim=c(0,10),xlab=colnames(dat_1504)[i]) ; abline(lm(dat_1504[,1]~ dat_1504[,i]), col='red')
    }
      
    boxplot(dat_1504)
    
    
    

    
    
######Correlation
	#correlation matrix
	#semipartial (relative importance of weights) and partial

    
  #correlation matrix (zero order)  
    library(Hmisc)
    zcor_1504 <- rcorr(as.matrix(dat_1504),type="pearson") 
    zcor_1504$r
    zcor_1504$P
  
    #plot graph
        library(GGally)
        ggcorr(dat_1504, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')
  
  #semeipatial and partial matrix    
    library(ppcor)  
    pcor(dat_1504)$estimate   #left column treated as Y
    spcor(dat_1504)$estimate
    
      

#######Standardized coefficients
      #coefficient_x*(sd(x)/sd(y))
    
    #Beta weights for four variables
      std_fit_1504 <- lm(scale(Satisf)~scale(Respons)+scale(NumSuper)+scale(Environ)+scale(YearsService),data=dat_1504)
      round(std_fit_1504$coefficients,5)[-1]

        #should be identical
      round(fit_1504$coefficients[-1]*(apply(dat_1504[,-1],2,sd)/sd(dat_1504[,1])),5)
    

#######Backward Elimination
      step(fit_1504, direction = "backward")
      
      
      
