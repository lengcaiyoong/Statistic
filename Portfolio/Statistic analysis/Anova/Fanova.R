#FANOVA


#01 Enter data
dat = read.table(url("http://www.uvm.edu/~dhowell/methods8/DataFiles/Tab13-2.dat"),head=T)
str(dat)


#02 convert column to factor
dat$Age <- factor(c("Young", "Old")[dat$Age], levels=c("Young", "Old"))
dat$Condition <- factor(c("C1", "C2","C3","C4","C5")[dat$Condition])

str(dat)



#03 construct simple effect table
dat_mean <- model.tables(aov1,type="means",data=dat)
age_mean <- dat_mean$tables$Age
con_mean <- dat_mean$tables$Condition
ac_mean <- dat_mean$tables$'Age:Condition'

dat_a1 <- subset(dat, dat$Age=="Young")
dat_a2 <- subset(dat, dat$Age=="Old")
dat_c1 <- subset(dat, dat$Condition=="C1")
dat_c2 <- subset(dat, dat$Condition=="C2")
dat_c5 <- subset(dat, dat$Condition=="C5")




#04 run Anova
aov1 <- aov(Recall~Age*Condition, data=dat)  #model with interaction term

aovY <- aov(Recall~Condition, data=dat_a1)  #simple effect
aovO <- aov(Recall~Condition, data=dat_a2)
aovc1 <- aov(Recall~Age, data=dat_c1)
aovc2 <- aov(Recall~Age, data=dat_c2)
aovc5 <- aov(Recall~Age, data=dat_c5)






###    Interpretation    ###
#   Interaction significance found! 
#     under both Age, condition effects significant, but SS is different (351.5 vs 1354)
#     under Condition, c1 and c2 not significant, but others do (diff between age group occur in higher mental processing)



summary(aov1)   #interaction significants
summary(aovY)   #condition effect significants in Young
summary(aovO)   #condition effect significants in Old
summary(aovc1)  #age effect not significants in C1
summary(aovc2)  #age effect not significants in C2
summary(aovc5)  #age effect  significants in C5

TukeyHSD(aov1,'Condition')
TukeyHSD(aov1,'Age')
TukeyHSD(aov1,'Age:Condition')




#(a) plot graph of interaction


par(mfrow=c(1,2))
barplot(ac_mean,beside=T,legend.text = T, ylab="Recall",
        ylim=c(0,max(dat$Recall)+5) ,xlab="Condition" ,xlim=c(0,20))
interaction.plot(x.factor = dat$Condition, trace.factor = dat$Age, response =dat$Recall,
                 fun = mean,  
                 type="b",
                 col=c("black","grey"),  ### Colors for levels of trace var.
                 pch=c(19, 17),             ### Symbols for levels of trace var.
                 fixed=T,                    ### Order by factor order in data
                 leg.bty = "o",
                 trace.label = "",
                 xlab="Condition"   , ylab="Recall"  )


   