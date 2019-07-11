### Exercise 15.4:
#   A large corporation is interested in predicting a measure of job satisfaction among its employees. 
#       They have collected data on 15 employees who each supplied information on
#       job satisfaction, level of responsibility, number of people supervised, rating of working
#       environment, and years of service.





######      Enter neccesary data

    library(Hmisc)
    library(GGally)
    library(ppcor)  
    
        dat_1504 <- read.table(url("http://www.uvm.edu/~dhowell/methods8/DataFiles/Ex15-4.dat"), head=T)
        fit_1504 <- lm( Satisf ~ . , data=dat_1504 )
        diagnose_1504 <- cbind(fit_1504$residuals, rstudent(fit_1504), hatvalues(fit_1504),cooks.distance(fit_1504))
        colnames(diagnose_1504) <- c("Residuals", "Studentized", "Hat", "Cook")
        zcor_1504 <- rcorr(as.matrix(dat_1504),type="pearson") 
        std_fit_1504 <- lm(scale(Satisf)~scale(Respons)+scale(NumSuper)+scale(Environ)+scale(YearsService),data=dat_1504) #Beta weights for four variables
        
        
        
    
#######     Summary and interpretation




#01
##  Respons and Environ correlated higher with Satisf and have the lowest p-value than other variables

            #correlation matrix
        zcor_1504$r   #correlation
        zcor_1504$P   #p-value
        ggcorr(dat_1504, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')




#02
## Most of the explanatory variables aren't really normal distributed

            #histogram, qqplot and scatterplots
        par(mfrow=c(3,5))
        mapply(hist,dat_1504,round(sqrt(nrow(dat_1504))),main=colnames(dat_1504))
        apply(dat_1504,2,function(x){qqnorm(x);qqline(x)})
        for (i in 1:5){
             plot(dat_1504[,c(1,i)],ylim=c(0,10),xlab=colnames(dat_1504)[i]) ; abline(lm(dat_1504[,1]~ dat_1504[,i]), col='red')
                }




#03
##  NumSuper and Respons seems correlated with other variables than Environ    

            #VIF
        library(faraway)
        vif(fit_1504)





#04
## Environ variable makes the most contribution in prediction amongst variables.
#     In fact, Environ has the largest t-value and semipartial correlation

            #semipartial and t-value
        spcor(dat_1504)$estimate[-1,1]
        summary(fit_1504)$coefficients[c(2:5),3]


        
        



#05
## Two variables found interesting, namely, "number of people supervised (NOPS)" and "years of service (YOS)".
#     If we look at the correlation between "YOS" and job satisfication, they are negatively correlated, although 
#     the correlation isn't really high. However, when other variables are held constant, the relationship becomes 
#     positive! This happens to "NOPS" as well. In fact, when others variable are held constant, supervision seems
#     unpleasant in workplace.      


            #scatterplot graph of both variables
        par(mfrow=c(1,2))
        plot(dat_1504$YearsService,dat_1504$Satisf, main= "Year vs Satisf")
        abline(lm(dat_1504$Satisf~dat_1504$YearsService), col="red")
        plot(dat_1504$NumSuper,dat_1504$Satisf, main= "NumSuper vs Satisf")
        abline(lm(dat_1504$Satisf~dat_1504$NumSuper), col="blue")


        zcor_1504$r[1,-1][c(2,4)]                       #zero order correlation
        summary(fit_1504)$coefficients[-1,1][c(2,4)]    #regression coefficients






        
        

#06
## R squared doesn't significant unless to include Environ and Respons into the equation.
#     When comparing models include only Environ variable, 
#     the complex model (Environ and Respons) doesn't significantly predict better; However, 
#     we still cannot conclude that the simpler model is better! In fact, adding Respons 
#     into the  equation increases quite amount of R squared and decreases the overall standard error, 
#     although accompanied with a slight increase in standard error of Environ predictor trade-off. Besides,
#     the AIC of the complex model is smaller even parameter increases. Thus, if prediction is the main concern, I would suggest the complex model.            

        summary(fit_1504)
        fit01 <- lm(Satisf ~ Environ , data = dat_1504)
        fit02 <- lm(Satisf ~ Environ+Respons, data = dat_1504)

        anova(fit01, fit02)      #p-value not less than .05

        extractAIC(fit01) ; extractAIC(fit02) #smaller AIC better
        summary(fit01)            
        summary(fit02)          #increase in R-squared


        
        

#07
##  If the complex model (Environ and Respons) is utilized, is adding an extra variables considered redundant?
#     As shown in VIF, the additional NumSuper variable might have potential multicollinearity issue,
#     namely, when compared to the additional YearsService variable, the standard error of predictors increase.
#     Although R-squared is higher when compared both, these 2 models, unfortunately, don't lead to overall increase
#     in R-squared when compared to the simpler model. Therefore, the extra variables is redundant.      


          fit03 <- lm(Satisf~ Environ+Respons+YearsService,data=dat_1504) 
          fit04 <- lm(Satisf~ Environ+Respons+NumSuper,data=dat_1504) 
          
          vif(fit02)  ; vif(fit03) ; vif(fit04) #VIF of each model
          
          summary(fit02)
          summary(fit03)
          summary(fit04) 





#08
##  Case 12 scored very high in Cooks, obviously greater than 1! Eliminating this data show a boost in R-squared (along with
#     some uneven effect on standard error of coefficients)!
#     However, we cannot simply eliminate the value for assumption fitting purpose! Legitimate reason needs to be 
#     found in order to set this case aside.


          fit_1504 <- lm( Satisf ~ . , data=dat_1504 )

          diagnose_1504 <- cbind(fit_1504$residuals, rstudent(fit_1504), hatvalues(fit_1504),cooks.distance(fit_1504))
          colnames(diagnose_1504) <- c("Residuals", "Studentized", "Hat", "Cook")

              #influential points
          par(mfrow=c(1,3))
          plot(fit_1504,pch=18,col="red",which=c(4) )
          round(diagnose_1504,4)

          dat_1504b <- dat_1504[-12,]
          fit_1504b <- lm( Satisf ~ . , data=dat_1504b )

          summary(fit_1504)
          summary(fit_1504b)

          par(mfrow=c(2,4))
          plot(fit_1504)
          plot(fit_1504b)



          
