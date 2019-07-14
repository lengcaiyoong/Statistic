
#Anova



#1 Enter Data
dat = read.table(url("http://www.uvm.edu/~dhowell/methods8/DataFiles/Tab11-2.dat"),head=T)

str(dat)                              #check if explanatory variables are FACTOR
dat$Group <- factor(dat$Group)



#2 Means of each group
dat_mean <- model.tables(anova1,type="means",data=dat)



#3 Ten post hoc pairwise tests in total
levels(dat$Group)   #levels of explanatory variable
dim(combn(seq(0,8,2),2))[2]   #how many pairwise comparison can be conducted



#4 Run Anova
anova1 <- aov(dv~Group, data=dat)



#5 Plot Graph
plot(dat$dv~dat$Group)
abline(h=mean(dat$dv),col="red")




####          Interpretation           ###

#01   Result is significant! 
#
summary(anova1)



#02   PostHoc Test
#       Using TukeyHSD test, we found that Group 4 is significant different from Group 0 and 8

#(a) Critical value (in terms of mean difference)
#     Group means difference should be more than 1.732 in order to show significance   

qtukey(0.95,5,df=55)*sqrt(summary(anova1)[1][[1]][[3]][[2]]/table(dat$Group)[[1]])
dat_mean$tables$Group[order(dat_mean$tables$Group)] #sort the means in ascending order
     #(4) =/= (0,8)


#(b) Critical value in terms of q
#     Calculated q should more than 3.98 in order to show significance

qtukey(0.95,5,55,lower.tail=T)
(1.8025-0.6750) / sqrt(  2.263  / 12)    # Difference between Group 0 and Group 2 <3.9
ptukey(2.59636,5,55,lower.tail=F)        # pv >.05
(1.8891667+0.9125) / sqrt(  2.263  / 12) #Group Difference between 8 and 4 >3.9
ptukey(6.451561,5,55,lower.tail=F)       # pv <.05


#(c) All pairwise test
#     The results should be identical with the calculation above

TukeyHSD(anova1)

