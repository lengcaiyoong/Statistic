
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
k <- Assign(dat,1,interval)               #v = 1
print(k)

