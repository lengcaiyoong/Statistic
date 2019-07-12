#multicategory-baseline category logit

## Introduction
#     In previous part, I've utilized binomial logistic regression to fit the data. Although the effect is significant.
#       The result looks weird. Thus, I decided to separate "Partly Abolishment" from "Retention", and fit
#       the data with multicategory logit model to see if I can come up different conclusion.






###01 Enter data and variables
dat <- read.csv(url("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/dataset/fulldeathpenaltydata.csv"),head=T)
str(dat)
head(dat);tail(dat)
DP <- na.omit(dat)
head(DP)

# Relevel factor
DP$FRE <- factor(DP$FRE,levels=c("NF","PF","F"))  #NOT FREE vs PARTLY FREE vs FREE
DP$DS <- factor(DP$DS,levels=c("FAB","HAB","RET")) #Full vs HALF ABOLISHMENT vs RETENTION


# Collapse Freedom Variable (stored in new column)
DP$CFRE <- DP$FRE
levels(DP$CFRE) <- list(F=c("F","PF"),NF="NF")
DP$CFRE <- factor(DP$CFRE,levels=c("NF","F"))
head(DP)


# Set reference level
contrasts(DP$FRE)=contr.treatment(levels(DP$FRE),base=1)  #F as baseline
contrasts(DP$FRE)








###02 Cross tabulation

# Enter Variable
Assign = function(df,v,interval){   #df = data frame ; v stands for which column variable ; interval vector
  df$new <- NA
  for (i in 1:length(interval)){
    df$new[! which(df[,v] <= interval[i]) %in% which(df[,v] <= interval[i-1]) ] <-  interval[i]
  }
  df
}
Interval <- c(0.55,0.75,1.0)  #group numeric HDI into several interval
DP <- Assign(DP,5,Interval)
names(DP)[7] <- "fHDI"       #new column named fHDI



# Table Analysis
#     Let's look at the three way table, there are only little difference in NF group.    
#       Most probabilities are similar in each Death Penalty Status. However, in PF and F group,
#       Partly Free and Free countries tends to fully abolish death penalty. It is more obvious if we look
#       at collapsed FRE 3-way table. This might suggests an interaction term.
#     On the other hand, FRE shows quite a lack of fit in FULL-ABOLISHMENT, compared to HDI,
#       the main effect is weak. Thus, FRE or collapsed FRE might achieve significance.



table(DP$DS,DP$FRE)   # main effect of FRE
table(DP$DS,DP$fHDI)  # main effect of HDI
table(DP$DS,DP$CFRE)  # collapsed FRE main effect

round(prop.table(t(as.matrix(ftable(DP$FRE,DP$fHDI,DP$DS))),2),3)  #column totals
        #Free and Partly Free countries prefer to abolish death penalty

round(prop.table(t(as.matrix(ftable(DP$CFRE,DP$fHDI,DP$DS))),2),3)  #column totals
        #Collapsed Free countries prefer to abolish death penalty











###03 Run Analysis
library(VGAM)
fit0 <- vglm(DS~1, family=multinomial , data=DP)   #null model
fitF <- vglm(DS~FRE, family=multinomial(refLevel="FAB") , data=DP)  #FAB as base
fitCF <- vglm(DS~CFRE, family=multinomial(refLevel="FAB") , data=DP)  #FAB as base
fitH <- vglm(DS~HDI, family=multinomial(refLevel="FAB") , data=DP)  
fitFH <- vglm(DS~FRE+HDI, family=multinomial(refLevel="FAB") , data=DP) 
fitFHb <- vglm(DS~FRE+HDI-1, family=multinomial(refLevel="FAB") , data=DP) #show all coefficients
fitCFH <- vglm(DS~CFRE+HDI, family=multinomial(refLevel="FAB") , data=DP) 
fitI <- vglm(DS~FRE*HDI, family=multinomial(refLevel="FAB") , data=DP)  
fitI2 <- vglm(DS ~ CFRE*HDI, family=multinomial(refLevel="FAB") , data=DP) 

#or
library(nnet)
DP$DS <- relevel(DP$DS, ref="FAB")  #FAB as base
nfit0 <- multinom(DS ~ 1, data=DP)  #null model
nfitF <- multinom(DS ~ FRE, data=DP) 
nfitCF <- multinom(DS ~ CFRE, data=DP) 
nfitH <- multinom(DS ~ HDI, data=DP) 
nfitFH <- multinom(DS ~ FRE+HDI, data=DP) 
nfitFHb <- multinom(DS ~ FRE+HDI-1, data=DP) #show all coefficients
nfitCFH <- multinom(DS ~ CFRE+HDI, data=DP) 
nfitI <- multinom(DS ~ FRE*HDI, data=DP)
nfitI2 <- multinom(DS ~ CFRE*HDI, data=DP) 


pv_calculator <- function(fit){
  zstats = summary(fit)$coefficients/summary(fit)$standard.errors
  pvalue = (1 - pnorm(abs(zstats), 0, 1)) * 2
  print(pvalue)
}

#pv_calculator(fit)  #Pvalue calculator for nnet fit model
    #eg. pv_calculator(nfitI)











###04 Summary and Interpretation
#(a) As mentioned, the interaction term nearly significants only when the FRE is collapsed.
#       However, AIC decreases when additional term is added, although only little reduction.
#

  lrtest(fitI,fitFH)      #model comparison between with and without interaction

#Identical Calculation of FRE*HDI Interaction 
  anova(nfitFH,nfitI)
  pchisq(deviance(fitFH) - deviance(fitI), df.residual(fitFH) - df.residual(fitI), lower.tail=F)

lrtest(fitI2,fitCFH)    #collapsed FRE model comparison btwn with / without interaction


#Identical Calculation of Collapsed FRE*HDI Interaction 
  anova(nfitCFH,nfitI2)
  pchisq(deviance(fitCFH) - deviance(fitI2), df.residual(fitCFH) - df.residual(fitI2), lower.tail=F)


#AIC
  AIC(nfitCFH)
  AIC(nfitI2)
  
  
  
  
  
  
#(b) As previously stated, the main effect of FRE (or collapsed FRE) is significant.
#       but HDI doesn't.


lrtest(fitFH,fitH)  #FRE + HID  
lrtest(fitCFH,fitH) #collapsed FRE + HID
  # or 
  #Anova(nfitI)   # FRE * HID
  #Anova(nfitI2)  # collapsed FRE * HID
    # or
deviance(nfitH) - deviance(nfitFH)
pchisq(deviance(fitF) - deviance(fitFH), df.residual(fitF) - df.residual(fitFH),lower.tail=F)
    #predictor HDI isn't significant















###05 Summary of Effect (in terms of probabilities)

#(a) The estimated probabilities of FRE*HDI model (fitI)


par(mfrow=c(2,3))
HDIs = seq(0.4,1,0.15)  #set sequence of X axis

plot(HDIs,
     predict(fitI,newdata=data.frame(FRE="NF",HDI=HDIs),type="response")[,1],
     type="o",pch=21,bg="black",ylab="P( FAB )",ylim=c(0,1),main="Full Abolishment (FAB)")
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="PF",HDI=HDIs),type="response")[,1],
       type="o",pch=21,bg="green",col="green")
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="F",HDI=HDIs),type="response")[,1],
       type="o",pch=21,bg="grey",col="grey")
legend(x="topleft",pch=21,col=c("black","green", "grey" ),
       pt.bg=c("black","green", "grey" ),
       legend=c("Not Free","Partly Free","Free"))

plot(HDIs,
     predict(fitI,newdata=data.frame(FRE="NF",HDI=HDIs),type="response")[,2],
     type="o",pch=21,bg="black",ylab="P( PAB )",ylim=c(0,1),main="Partly Abolishment (PAB)")
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="PF",HDI=HDIs),type="response")[,2],
       type="o",pch=21,bg="green",col="green")
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="F",HDI=HDIs),type="response")[,2],
       type="o",pch=21,bg="grey",col="grey")
legend(x="topleft",pch=21,col=c("black","green", "grey" ),
       pt.bg=c("black","green", "grey" ),
       legend=c("Not Free","Partly Free","Free"))

plot(HDIs,
     predict(fitI,newdata=data.frame(FRE="NF",HDI=HDIs),type="response")[,3],
     type="o",pch=21,bg="black",ylab="P( RET )",ylim=c(0,1),main="Retention")
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="PF",HDI=HDIs),type="response")[,3],
       type="o",pch=21,bg="green",col="green")
points(HDIs,
       predict(fitI,newdata=data.frame(FRE="F",HDI=HDIs),type="response")[,3],
       type="o",pch=21,bg="grey",col="grey")
legend(x="topleft",pch=21,col=c("black","green", "grey" ),
       pt.bg=c("black","green", "grey" ),
       legend=c("Not Free","Partly Free","Free"))

estimated_fitI <- list(
  data.frame(NF=predict(fitI,newdata=data.frame(FRE="NF",HDI=HDIs),type="response")[,1],
             PF=predict(fitI,newdata=data.frame(FRE="PF",HDI=HDIs),type="response")[,1],
             F = predict(fitI,newdata=data.frame(FRE="F",HDI=HDIs),type="response")[,1]
  ),
  data.frame(NF=predict(fitI,newdata=data.frame(FRE="NF",HDI=HDIs),type="response")[,2],
             PF=predict(fitI,newdata=data.frame(FRE="PF",HDI=HDIs),type="response")[,2],
             F = predict(fitI,newdata=data.frame(FRE="F",HDI=HDIs),type="response")[,2]
  ),
  data.frame(NF=predict(fitI,newdata=data.frame(FRE="NF",HDI=HDIs),type="response")[,3],
             PF=predict(fitI,newdata=data.frame(FRE="PF",HDI=HDIs),type="response")[,3],
             F = predict(fitI,newdata=data.frame(FRE="F",HDI=HDIs),type="response")[,3]
  ))

names(estimated_fitI) <- levels(DP$DS)




#(b) The estimated probabilities of CFRE*HDI model (fitI2)


plot(HDIs,
     predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response")[,1],
     type="o",pch=21,bg="black",ylab="P( FAB )",ylim=c(0,1),main="Full Abolishment (FAB)")
points(HDIs,
       predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response")[,1],
       type="o",pch=21,bg="grey",col="grey")
legend(x="topleft",pch=21,col=c("black", "grey" ),
       pt.bg=c("black", "grey"),
       legend=c("Not Free","Free"))

plot(HDIs,
     predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response")[,2],
     type="o",pch=21,bg="black",col="black",ylab="P( PAB )",ylim=c(0,1), main="Partly Abolishment(PAB)")
points(HDIs,
       predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response")[,2],
       type="o",pch=21,bg="grey",col="grey")
legend(x="topleft",pch=21,col=c("black", "grey" ),
       pt.bg=c("black", "grey"),
       legend=c("Not Free","Free"))

plot(HDIs,
     predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response")[,3],
     type="o",pch=21,bg="black",col="black",ylab="P( RETENTION )",ylim=c(0,1),main="Retention")
points(HDIs,
       predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response")[,3],
       type="o",pch=21,bg="grey",col="grey")
legend(x="topleft",pch=21,col=c("black", "grey" ),
       pt.bg=c("black", "grey" ),
       legend=c("Not Free","Free"))


estimated_fitI2 <- list(
  data.frame(NF=
               predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response")[,1],
             F = predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response")[,1]
  ),
  data.frame(NF=
               predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response")[,2],
             F = predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response")[,2]
  ),
  data.frame(NF=
               predict(fitI2,newdata=data.frame(CFRE="NF",HDI=HDIs),type="response")[,3],
             F = predict(fitI2,newdata=data.frame(CFRE="F",HDI=HDIs),type="response")[,3]
  ))
names(estimated_fitI2) <- levels(DP$DS)




#(d) Summary:
#     Since the main effect of FRE is significant, we can check if the model with only FRE (or collapsed FRE) fits.
#       The residual deviance gives high p-value, which indicateds this simple model fits well.       
#       To look at the effects, Partly Free or Free countries more likely to fully abolish (0.63 & 0.78), but 
#       No Free countries prefer to retain the punishment(0.6).



pchisq(deviance(fitCF),df.residual(fitCF),lower.tail = F)   #goodness of fit test (Collapsed FRE)
pchisq(deviance(fitF),df.residual(fitF),lower.tail = F)     #goodness of fit test (FRE)

estimated_fitF <- data.frame(FRE=levels(DP$FRE), 
                             rbind(predict(fitF,newdata=data.frame(FRE="NF"),type="response"),
                                   predict(fitF,newdata=data.frame(FRE="PF"),type="response"),
                                   predict(fitF,newdata=data.frame(FRE="F"),type="response")))
rownames(estimated_fitF) <-1:3

t(estimated_fitF)    #predicted values from fitF model







###05 Conclusion
#     Let's look at the six graph plotted. Firstly, take a look at 3 graphs at first row.
#       As you can see, the green line PARTLY FREE is quite stable across HDI. Besides, it is quite close
#       to the black line FREE. This somewhat explains why the collapsed FRE gives smaller p-value for interaction term than uncollapsed.
#     Now, move to the second row!
#         The second row graphs show that as HDI increases,
#         NOT free countries tend to "PARTLY" abolish death penalty, rather than retention! This result is different from the previous     
#         binomial logistic analysis. Consequently, the likelihood of retention in Not Free countries decrease as HDI increases!
#     Thus, according to the graphs, for NOT FREE coutries, we can conclude that, when HDI less than 0.7, they are more likely to 
#         retain (0.51 - 0.63), when HDI more than 0.7, those countries prefer PARTLY abolishment (0.7). (DIFFERENT CONCLUSION)
#     However, if interaction is ignored, the main effect of FRE is quite large and stable, the relationship between them 
#         should be worth investigation.
#

estimated_fitI2   #predicted probabilities in fitI2 model
  #estimated_fitI

