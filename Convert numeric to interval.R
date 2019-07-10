
# Group numeric values in a column into different interval


Assign = function(df,v,interval){   #df = data frame ; v stands for which column variable ; interval vector
    df$new <- NA
  for (i in 1:length(interval)){
    df$new[! which(df[,v] <= interval[i]) %in% which(df[,v] <= interval[i-1]) ] <-  interval[i]
  }
    df
}



#Example
dat = data.frame(A=runif(10,1,10), B=runif(10,2,9))  #data
group = c(5,10)                           #wanna group column 1 into <= 5 or more than 5
dat <- Assign(dat,1,interval)               #v = 1
dat




#decide the length of interval
library(Hmisc)
x <- runif(1000, 0, 100)
z <- cut2(x, c(10,20,30))
table(z)
table(cut2(x, g=10))      # quantile groups
table(cut2(x, m=50))      # group x into intevals with at least 50 obs



