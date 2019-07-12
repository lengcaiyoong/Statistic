##    Subjects were randomly assigned to taste one of four different cheeses. 
#           Response categories are 1 = strong dislike to 9 = excellent taste.
#           https://newonlinecourses.science.psu.edu/stat504/node/177/



      #Enter data
grp_data<- read.table(url("https://newonlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson07/cheese/index.dat"),col.names = c("Cheese","Response","Count"))
grp_data$Response <- factor(grp_data$Response,ordered=T)


      #convert data to long data
source("https://raw.githubusercontent.com/lengcaiyoong/Statistic/master/CONVERT/convert%20grouped%20to%20long%20data%20(multicategory).R")


      #check the class of column
grp_data.df -> cheese
class(cheese$Cheese)
class(cheese$Response)

      #set reference level
contrasts(cheese$Cheese)=contr.treatment(levels(cheese$Cheese),base=1)
contrasts(cheese$Cheese)

      #run analysis
library(VGAM)
fit <- vglm( Response ~ Cheese, family= cumulative(parallel = TRUE),data=grp_data.df)
fit0 <- vglm( Response ~ 1, family= cumulative(parallel = TRUE),data=grp_data.df)
      
      or
library(rms)
ologit <- lrm(Response~Cheese,data=cheese)
      #https://youtu.be/vDXEo2vzKbQ




      ###Intepretation and summary###




##01
#   Obviously, Cheese A and D show increasing trend, but B from the other end.
#       
      table(cheese)

            

##02
#   If  Cheese A is treated as the baseline, all of the coefficients show significance!
#     In fact, this result agrees with what we've seen before running the statistical test.      

      summary(fit)

      
      
##03      
#   Let's formulate the logit equation with the coefficients! When j = 4,
#      logit(P(Y<j)) = -2.24401 + 3.35184 X1 +1.70989 X2 - 1.61279 X3
#      
       

      
##04      
#   From the summary, we can state that for any fixed j which P(Y<j), Cheese B is in the "dislike" direction rather 
#     than the "excellent" direction equals to 28.55 times the estimated odds for Cheese A.
#      
      
      exp(3.3518)
     
      
##05
#   Let's interpret in terms of probability. When j = 4,
#      Cheese A is rated slightly lower in category 1 till 4, but Cheese B is higher in 
#      these categories.
#      In general, we can also say that, D > A > C > B in terms of quality.
#      
      
      data.frame(Cheese=unique(cheese$Cheese), 
          apply(round(fitted(fit),4),2,unique))

      
      
##06
#   This model is significantly better than null model with only intercept.

            
      lrtest(fit,fit0)



