

#input data
data00<-
  head(data00);tail(data00)

    #example
Gators_1 <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat",header=TRUE)
data00 <- Gators_1









##sorting data if continuous variable haven't sorted
            #https://youtu.be/Nh6tSD4i4qs?t=342
    #example
data00[order(data00$x, decreasing=T),]
    #decreasing example










#if data did not assign value to each observation, create a column and assign them
    #example
data00$profile <- 1:nrow(data00)











#set boundaries and interval parameter
c1      <-      # eg. data00$x ; column of continuous variable
profile <-      # eg. data00[,1] ; assigned number of each obsrvatation 
nI      <-      # number of interval needed 

  
    #example
c1  <-  data00$x                    # eg. data00$x ; column of continuous variable
nI  <-  4                           # number of interval needed 
profile <-data00$profile          # eg. data00[,1] ; assigned number of each obsrvatation 








##execute and run

boundaries <- seq(min(c1),max(c1),(max(c1)-min(c1))/nI)
storage <- as.list(rep(NA,nI)) #group data by boundaries

for (i in 1:nI){
    storage[[i]] <- as.character(profile [c1 <= boundaries [i + 1] & c1 > boundaries[i]])
  }

storage[[1]][length(storage[[1]]) + 1] <- as.character (profile[c1 == boundaries[1]])
    #replace missed value which equal to boundaries 1

for (i in 1:nI){
    storage [[i]] <- as.numeric(storage[[i]])
  }


storage_2 <- as.list(rep(NA,nI))

for (i in 1:nI){
    storage_2[[i]] <- data00$y [min(storage[[i]]):max(storage[[i]])]
  }

storage_3 <- as.list(rep(NA,nI))

for (i in 1:nI){
    storage_3[[i]] <- table(storage_2[i])
  }

z <- do.call(rbind,storage_3)

sum(z)

print(z)


