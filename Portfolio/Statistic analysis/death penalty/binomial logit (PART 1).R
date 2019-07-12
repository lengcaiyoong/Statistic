#logistic regression

### DEATH PENALTY:
#     Are economic development and political freedom related to legalization of death penalty?
#     Currently I have found 2 indicators to measure both of the variables, namely HDI (human development index)   
#     and freedom house rating. I have combined all the required information and uploaded on GitHub.
#



### Measurement Scale:
#   In general, HDI measures the need of economic development of a country. The lower the score, the higher the needs.
#     On the other hands, Freedom house rating measures degree of political freedom. In fact, it is a categorical variable, which
#     contains three levels, namely, "Free", "Partly Free" and "Not Free".


### Status of Death Penalty:
#   A country could fully abolish, partly abolish or retain such capital punishment. In fact,
#     a country which abolished death penalty for ordinary crimes are considered partly abolishment.
#     However, since I'm running binomial logistic regression, I will collapse partly
#     abolishment into retention group at this stage.


### Formulate Questions:
#   Is there any relationship between HDI or/and Freedom rating with Legalization of Death Penalty?
#   Or we can even ask if any interaction effect within them? 
#   Are the results "practically" significant if small p-value is obtained?  
#


### Things to take note:
#   Before we run the analysis, several assumption and cautions should be raised.
#     Firstly, the attempt which is to find the significance amongst any combination of these variables leads to expansion of Type I
#     error. In other words, if any significance result is obtained, it could be due to this expansion.
#     Secondly, are the sample really sample? Or should it be treated as population? In fact, if these data
#     are population, there is no need to run any statistical test. Unfortunately, my data are incomplete,
#     some countries are left out, and I have no idea why they're not included. Thus, to bear with me,
#     I would just assume these data are random sample from population, and run these data without any any weighting/nonresponse adjustment.
#     Lastly, to mentioned, both the variables are come in hand in different years, namely 2017 and 2018.
#



###01 Enter data and variables
dat <- read.csv(url("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/dataset/fulldeathpenaltydata.csv"),head=T)

head(dat); str(dat) ; DPb <- na.omit(dat)
head(DPb) ; str(DPb)


# Collapse death penalty status to 2 levels
levels(DPb$DS) <- list(AB="FAB",RET=c("HAB","RET"))  #Fully-Abolishment vs Retention

# Convert DS to numeric
DPb$DS <- as.numeric(DPb$DS)-1    #1:retention; 0:abolishment

# Relevel FREEDOM levels
DPb$FRE <- factor(DPb$FRE,levels=c("NF","PF","F"))  #No Free vs Partly Free vs Free

# Collapse Freedom Variable (stored in new column)
DPb$CFRE <- DPb$FRE
levels(DPb$CFRE) <- list(F=c("F","PF"),NF="NF")
DPb$CFRE <- factor(DPb$CFRE,levels=c("NF","F"))
head(DPb)

# Set reference level
contrasts(DPb$FRE)=contr.treatment(levels(DPb$FRE),base=1)  #F as baseline
contrasts(DPb$FRE)




###02 Cross Tabulation Analysis


# Enter Variable
library(dplyr)
Assign = function(df,v,interval){   #df = data frame ; v stands for which column variable ; interval vector
  df$new <- NA
  for (i in 1:length(interval)){
    df$new[! which(df[,v] <= interval[i]) %in% which(df[,v] <= interval[i-1]) ] <-  interval[i]
  }
  df
}
  Interval <- c(0.55,0.75,1.0)  #group numeric HDI into 3 intervals
  DPb <- Assign(DPb,5,Interval)
  names(DPb)[7] <- "fHDI"       #new column of 3-interval HDI named fHDI
  
  head(DPb)
  
  
# Table Analysis
#   Let's look at the main effect (pairwise comparison)! Both HDI and FRE show effect on DS, namely between NF and F.
#   In fact, if we look at the three-way table, the increasing/decreasing patterns differ between "NF" and "F" as HDI increases.
#   For abolishment, F tends to INCREASE higher than other group as HDI increases, whereas NF DECREASE slowly.
#   Therefore, before running the data, we can conclude that a FRE x HDI interaction effect might exist!
#   Lastly, since PF and F have similar increasing trends in abolishment as HDI increases, to collapse the former into F
#   is recommended.
  
prop.table(table(DPb$DS,DPb$fHDI),2)  #HDI vs DS (death penalty status)

prop.table(table(DPb$DS, DPb$FRE),2)  #FRE vs DS (FRE = Freedom Rating)

round(table(subset(DPb, DPb$DS == 0)[,c(3,4,7)][,c(2:3)]) / table(DPb$FRE,DPb$fHDI),3) #Variation of percentage in Abolishment
round(table(subset(DPb, DPb$DS == 1)[,c(3,4,7)][,c(2:3)]) / table(DPb$FRE,DPb$fHDI),3) #Variation of percentage in Retention






###03 Running Analysis
fitF  <- glm(DS~FRE,family=binomial,data=DPb)
fitH  <- glm(DS~HDI,family=binomial,data=DPb)
fitFH  <- glm(DS~FRE+HDI,family=binomial,data=DPb)
fitCFH  <- glm(DS~CFRE+HDI,family=binomial,data=DPb)  #collapsed PF into F
fitI  <- glm(DS~HDI*FRE,family=binomial,data=DPb)     #Interaction
fitI2  <- glm(DS~HDI*CFRE,family=binomial,data=DPb)   #Interaction between collapsed FRE and HDI
null <- glm(DS~1,family=binomial,data=DPb)






###04 Summary and Interpretation
#(a) As predicted, interaction effect exists. It is almost significant! The calculation is presented as well. All calculated results 
#     should be identical with the results calculated from Anova software package.

library(car)
Anova(fitI)

#(Calculation) 
# pvalue of FRE*HDI Interaction
fitFH$deviance - fitI$deviance        #residual deviance
fitFH$df.residual - fitI$df.residual  #df of residual deviance
pchisq((fitFH$deviance - fitI$deviance ),(fitFH$df.residual - fitI$df.residual),lower.tail=F)

#pvalue of HDI
fitF$deviance - fitFH$deviance        #residual deviance
fitF$df.residual - fitFH$df.residual  #df of residual deviance
pchisq((fitF$deviance - fitFH$deviance),(fitF$df.residual - fitFH$df.residual),lower.tail=F)

#pvalue of FRE
fitH$deviance - fitFH$deviance        #residual deviance
fitH$df.residual - fitFH$df.residual  #df of residual deviance
pchisq((fitH$deviance - fitFH$deviance ),(fitH$df.residual - fitFH$df.residual),lower.tail=F)



#(b) As mentioned as well, the collapsed model seems to have lower pvalue, which indicates
#     more stable effect of interaction and FRE.

Anova(fitI)
Anova(fitI2) 



#(c)  Although the additional interaction doesn't highly significant, AIC suggests that the interaction should be considered.
#       In fact, additional term doesn't increase AIC but makes it smaller.


anova(fitCFH, fitI2)                    #compare models with and without interaction
pchisq(3.0461,1,lower.tail=F)           #nearly significant


#Calculation of Collapsed FRE x HDI Interaction 
fitCFH$deviance - fitI2$deviance        #residual deviance
fitCFH$df.residual - fitI2$df.residual  #df of residual deviance
pchisq((fitCFH$deviance - fitI2$deviance ),(fitCFH$df.residual - fitI2$df.residual),lower.tail=F)


Anova(fitI2)                            #comparisons between predictors

AIC(fitCFH)
AIC(fitI2)          #AIC of interaction smaller!









###05 Summary of Effect (in terms of probabilities)

    
#(a) The estimated probabilities of CFRE*HDI model (fitI2)

par(mfrow=c(1,2))

HDIs = seq(0.4,1,0.15)  #set sequence of X axis

plot(HDIs,
     predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response"),
     type="o",pch=21,bg="black",ylab="P( RETENTION )",ylim=c(0,1))
points(HDIs,
       predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response"),
       type="o",pch=21,bg="red",col="red")
legend(x="topleft",pch=21,col=c("black","green","red"),
       pt.bg=c("black","green","red"),
       legend=c("Not Free","Not Free (simple model)","Free"))



#(b) The estimated probabilities of CFRE & HDI model (fitCFH)

points(HDIs,
       predict(fitCFH,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response"),
       type="o",pch=22,lty=2,bg="green ",col="green")
points(HDIs,
       predict(fitCFH,newdata=data.frame(CFRE="F",HDI=HDIs),type="response"),
       type="o",pch=22,lty=2,bg="darkolivegreen2 ",col="darkolivegreen2 ")



#(c) The estimated probabilities of FRE x HDI model ((fitI)

HDIs = seq(0.4,1,0.15)  #set sequence of X axis

plot(HDIs,
     predict(fitI,newdata=data.frame(FRE="NF",HDI=HDIs),type="response"),
     type="o",pch=21,bg="black",ylab="P( RETENTION )",ylim=c(0,1))
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="PF",HDI=HDIs),type="response"),
       type="o",pch=21,bg="blue",col="blue")
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="F",HDI=HDIs),type="response"),
       type="o",pch=21,bg="red",col="red")
legend(x="topleft",pch=21,col=c("black","blue","red"),
       pt.bg=c("black","blue","red"),
       legend=c("Not Free","Partly Free","Free"))



#(d) Summary:
#   Let's look at the graph on the left. 
#     The solid line came from the collapsed FRE x HDI model. when HDI <0.7, 
#     NF and F countries have similar chances to retain death penalty. Both scores about 0.5.
#     However, when HDI >0.7, NF countries are more likely (0.78 vs 0.13) to retain death penalty.
#   Thus, we could no longer simply say that economy performance is positively related to legalization of capital punishment.
#     As we can see, it also depends on which types of country (FREE or NOT FREE) you refer to.
#   Now, look at the dashed line with bright color. They're come from without interaction model.
#      It ignores the interaction effect, so NF is always tends to retain across HDI than F.
#   Lastly, within the "uncollapsed" FRE x HDI model,
#      partly free countries do not change much in P(RET) as HDI varies (shown on the right), and
#      when HDI > 0.7, it shares the similar probability with "F" in Retention.


estimated_fitI2 <- as.data.frame(
cbind(
predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response"),
predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response")))

colnames(estimated_fitI2) <- levels(DP$CFRE)
rownames(estimated_fitI2) <- HDIs
print(estimated_fitI2)             #NF countries has 0.78 chance while F only has 0.13 in P(RET)





###06 Model Fitting
#   Before I made the conclusion, let's check if one of the model, such as FRE model, fits enough?
#     In other words, does the model only include FRE variable better than any complex model?
#   The residual deviance test shows nearly significant result, thus, complex model doesn't significantly
#     fit better.
#   However, although none of the standardized residuals exceed 2 or 3, which indicates lack of fit, 
#     13 std.residuals out of 121 are about 1.8, nearly about the critical value of 2 or 3.


#Goodness of fit test
pchisq(fitF$deviance,fitF$df.residual,lower.tail=F)


#standardized residuals
fit_table <- data.frame(FRE = DPb$FRE,fit=fitted(fitF), Pearson=rstandard(fitF,type="pearson"))
fit_table <- fit_table[order(fit_table$FRE),]
any (abs(fit_table$Pearson) >=2)  #none of the residuals exceed 2 (>2 indicate lack of fit)
summary(fit_table$Pearson)
#agrees with residual deviance

length(which(abs(fit_table$Pearson) >=1.8)) 





###07 Conclusion
#   However, the interaction result is contrary to our beliefs, especially the boost in P(RET) of NF 
#     after HDI > 0.7. Since I've combined both Retention and partly abolishment into the same category,
#     does this graph still look the same if I separate it out? In other words, is this boosted effect contributed 
#     from partly-abolishment?



























##PERSONAL NOTES

  #methods for significance test
#01 pchisq(z^2 ,df= , lower.tail=F) #wald test
#02 anova(fit1, fit2) compare 2 models , and get the residual diff btwn both
    #Anova(fit2) compare predictors within model

  #methods for checking model fitting
#01 anova(fit1, fit2)
#02 pchisq(fit$deviance, fit$df.residual,lower.tail=F) 
      #if significant, means the proposed model isn't better than saturated model
      #in other words, model complex than proposed model is better
      #but significance doesn't guarentee practical significant, have to look at AIC and effect in order to conclude if complex model better
#03 pearson residual and standardized residual to check goodness of fit of model
          #Values larger than about 2 or 3 in absolute value are worthy of attention
      #can also use fitted value to see if model fit well (pg82)
      #but need large sample for each row pg 131
        #**for continuous predictor, when calculated for logistic regression models having any continuous or nearly-continuous explanatory variables, the residual deviance does not have an approximate chi-squared distribution.
        #thus, can construct a complex model and compare!


