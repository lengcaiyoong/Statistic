
##  Professor Chen has conducted a survey to support her theory. Let's check if some of her statement
#     is statistically valid.
#   Reference: https://tw.news.yahoo.com/%E9%99%B3%E6%98%AD%E5%A6%82%E5%B0%88%E6%AC%84-%E8%94%A1%E8%8B%B1%E6%96%87%E4%B8%8D%E6%BB%BF%E6%84%8F%E5%BA%A6%E7%9A%84%E8%83%8C%E5%BE%8C-%E7%A4%BE%E6%9C%83%E6%B0%91%E4%B8%BB%E5%9C%8B%E7%9A%84%E5%8F%B0%E7%81%A3%E5%85%B1%E8%AD%98-160200606.html



##  Enter Data
Count = c(3,4,81,154,4,11,83,77,9,14,0,2,9,14,1,0,4,1,1,0,0,22,58,15,6,1,19,81,92,16,0,4,17,27,3
          )
Party = gl(7, 5, labels= c("KMT","DPP","NEWD","COA",
                           "ERA","NEU","OTHERS"))
Response = rep(ordered(c(1,2,4,5,3)),7)
app <- data.frame(Party=Party, Response=Response, Count=Count)
head(app)
grp_data <- app  #for conversion purpose



##  Convert to long format data
source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/CONVERT/convert%20grouped%20to%20long%20data%20(multicategory).R")
  #Press Y and enter if done
class(grp_data.df$Party)  #check if this column is factor
class(grp_data.df$Response) #check if it is ordered
App <- grp_data.df  #rename data
head(App);tail(App)



##  Set reference level
contrasts(App$Party)=contr.treatment(levels(App$Party),base=1)  #KMT as baseline
contrasts(App$Party)





###  Summary and Interpretation     ###





##01
#   According to the results, Chen said KMT supporters show the most disagreement (highest score) with their current president,
#     whereas DPP shows moderate agreement. Besides, ERA and NEU do not like their president,
#     in fact, the degree of diagreement of NEU is high.

table(App)



##02
#   Let's run an analysis to see if her statement agrees with the data.
#     Firstly, since there is one row which has the most zero scores in the table, I have collapse that row
#       to the last row. 
#     Secondly, to avoid Type I error expansion, I would only check the degree of disagreeement of ERA and NEU.
#       In fact, KMT and DPP should be obviously significant, but this is out of my interest anyway.



#03
#   Formulate the questions:
#     Do the coefficients of ERA and NEU significantly differ from zero?



##04
#   Run the analysis
#     Cumulative Logistic Regression is utilized to model the data, since the response variable is ordinal.
#     Besides, I will set j=3, which leads to P(Y<=3) or P(Y>3).
#     Since I am not interested in the pairwise coefficients, which the statistical software only offer,
#     I will estimate both of the standard error of coefficients with bootstrapping approach.
#     


# Fit the model
library(VGAM)
fit <- vglm( Response ~ Party, family= cumulative(parallel = TRUE),data=App)
fit0 <- vglm( Response ~ 1, family= cumulative(parallel = TRUE),data=App)
    #or
library(rms)
contrasts(App$Party)=contr.treatment(levels(App$Party),base=1)
ologit <- lrm(Response ~ Party,data=App)
ologit0 <- lrm(Response ~ 1,data=App)


ologit ; pchisq(1.08^2,1,lower.tail=F) #Another way to test coefficients eg. NEWD


# Calculate the effect in terms of probabilities
predict_ologit <- predict(ologit,newdata=App,type="fitted.ind")
prediction_ologit <- data.frame(Party=unique(App$Party), 
                                apply(round(predict_ologit,4),2,unique))

effect_ologit<-cbind(   # Effect of each row in terms of probabilities
    apply(prediction_ologit[,c(2:4)],1,sum),
    apply(prediction_ologit[,c(5:6)],1,sum)
                    )

colnames(effect_ologit) <- c("Like", "Dislike") #rename the column
rownames(effect_ologit) <- prediction_ologit$Party  #rename the row


#Bootstrap estimation of 95%  confidence interval
#Parameter
nsims = 2000
df_stor <- list()
df_stor2 <- list()
coe_stor <- rep(NA, nsims)
coe_stor2 <- rep(NA, nsims)
len_ck <- rep(NA,nsims)
len_ck2 <- rep(NA,nsims)
mo_stor <- list()
mo_stor2 <- list()

for (i in 1:nsims){
  df_stor[[i]] <- App[sample(nrow(App), nrow(App),replace=T), ]
  mo_stor[[i]] <- lrm(Response ~ Party,data=df_stor[[i]])
  len_ck[i] <- length(coef(mo_stor[[i]]))
  coe_stor[i]   <- coef(mo_stor[[i]])[[which (names(coef(mo_stor[[i]])) == "Party=ERA")]] + coef(mo_stor[[i]])[[which (names(coef(mo_stor[[i]])) == "y>=3")]]
}

for (i in 1:nsims){
  df_stor2[[i]] <- App[sample(nrow(App), nrow(App),replace=T), ]
  mo_stor2[[i]] <- lrm(Response ~ Party,data=df_stor2[[i]])
  coe_stor2[i]  <- coef(mo_stor2[[i]])[[which (names(coef(mo_stor2[[i]])) == "Party=NEU")]] + coef(mo_stor2[[i]])[[which (names(coef(mo_stor2[[i]])) == "y>=3")]]
  len_ck2[i] <- length(coef(ologit <- lrm(Response ~ Party,data=df_stor[[i]])))
}



#Check if any level missing during bootstrapping
length(which(is.na(coe_stor)))  #should have no NA
length(which(is.na(coe_stor2)))
unique(len_ck)  #should have the same length with length(coef(ologit))
unique(len_ck2)





##05
#   Interpret result
#     Firstly, the model actually fits well. The additional predictors perform better than null model

    lrtest(ologit,ologit0)

#     Secondly, we gonna check if both coefficients significantly differ from zero.
#       From both histograms, we can see that most of the resampled coefficients are all well above zero!
#       Besides, I have chosen the lowest 2.5% and the highest 2.5% from the sample to form the 95% interval.
#       Both intervals don't cover zero. Thus, we can conclude that they are significantly differ from zero! 
#       This result therefore agrees with what Chen's statement.
      
    
    #histogram of coefficients
    par(mfrow=c(1,2))
    hist(round(coe_stor,4),breaks=40)$counts  #frequency
    #hist(round(coe_stor,4),breaks=40)$breaks  #which coefficient has the highest freq
    hist(round(coe_stor2,4),breaks=40)$counts  #frequency
    #hist(round(coe_stor2,4),breaks=40)$breaks  #which coefficient has the highest freq
    
    
    #create confidence interval of 95%
    quantile(coe_stor, probs = c(0.025, 0.975)) #interval of ERA
    quantile(coe_stor2, probs = c(0.025, 0.975)) #interval of NEU
    
    
    #compare the values provided by software with the quantiles
    coef(ologit)[[8]] + coef(ologit)[[2]] ; coef(ologit)[[9]] + coef(ologit)[[2]]
     


#     Lastly, let's look at the effect in terms of probabilities.
#       From the table, we can see that both NEU and ERA have higher chances to be categoried into higher categories, namely
#       NEU. In addtional, if we fixed at KMT, supporters are almost in the "Dislike" categories.
#       Although DPP supporters are more likely to rate lower, the difference between the probabilities isn't as huge as KMT.
#       Thus, this result is sending a warning message to president Tsai about her popularity amongst citizens.
    
    
effect_ologit
table(App)

par(mfrow=c(1,1))
barplot(t(effect_ologit), 
        beside = TRUE,
        names.arg = rownames(effect_ologit),
        xlim=c(1,24),  #scaling
        ylim = c(0,1),
        ylab = "P(Y)",
        legend.text = T,
        xlab = "Like vs Dislike")

abline(h=0.5,col='red', lty=5)
text(18,0.40,"P(Y)=0.5", col = "blue")



