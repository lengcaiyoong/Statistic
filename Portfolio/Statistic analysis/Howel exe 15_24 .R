### Exercise 15.24:
#   A reasonable model might propose that depression (DepressT) is a function
#       of (1) the person's current perceived level of vulnerability to additional loss (PVLoss),
#       (2) the person's level of social support (SuppTotl), and (3) the age at which the person lost a
#       parent during childhood (AgeAtLos).
#       Should we include PVTotal for prediction purpose?



##  Enter neccesary data

d1524 <- read.table(url("http://www.uvm.edu/~dhowell/methods8/DataFiles/Mireault.dat"),head=T, na.strings = ".")
  diagnose_1524 <- cbind(fit_1524$residuals, rstudent(fit_1524), hatvalues(fit_1524),cooks.distance(fit_1524))
  colnames(diagnose_1524) <- c("Residuals", "Studentized", "Hat", "Cook")
head(d1524);tail(d1524)




######  Summary and Interpretation   #####


#01
##  Once PVLoss is added into the equation, R-squared doesn't change much. However, vif and standard error of PVLoss coefficient
#         increases, leads to more instability! Thus, PVTotal should be excluded, since it is redundant with others.
#

                #linear models
          fit_1524 <- lm(DepressT ~ PVLoss+SuppTotl+AgeAtLos, data=d1524)
          fit_1524b <- lm(DepressT ~ PVLoss+SuppTotl+AgeAtLos+PVTotal, data=d1524)

          summary(fit_1524)$r.squared   #R squared without PVTotal
          summary(fit_1524b)$r.squared  #R squared with PVTotal

          round(summary(fit_1524)$coefficients[2,],5)    #standard error of coefficents PVLoss ,without PVTotal
          round(summary(fit_1524b)$coefficients[2,],5)   #standard error of coefficents PVLoss ,with PVTotal

          
          vif(fit_1524)[1]      #vif of PVLoss without PVLoss
          vif(fit_1524b)[1]     #vif of PVLoss with PVTotal
          
          


          
#02
##  Should we delete the data (292) which scores highest in Cook's D?
#       Although it has the highest Cooks' D, it is not sufficient extreme. Its studentized
#       residual isn't significantly large. Besides, after deletion, the overall R squared only increases
#       slightly, and the standard error of regression or coefficients only decrease slightly.
#

          
        
                #influential points
          par(mfrow=c(1,2))
          plot(fit_1524,pch=18,col="red",which=c(4) )
          diagnose_1524[101,] 
                
                #Studentized residual is smaller than critical value
          diagnose_1524[101,][2] <  
          qt(.975,df=(nrow(diagnose_1524)-4)) #studentized residual is less than critical value &
          
                #hats value is large
          diagnose_1524[101,][3] <
              (3*4)/(nrow(diagnose_1524)) #3*(parameter)/n
          
                #model which case 292 deleted 
          d1524b <- d1524[-292,]
          fit_1524c <- lm(DepressT ~ PVLoss+SuppTotl+AgeAtLos, data=d1524b)
          
                #model comparison
          summary(fit_1524)
          summary(fit_1524c)
          
          
          
          
