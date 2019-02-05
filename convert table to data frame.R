#function convert table to raw data


# 1. input data 
  # example:
       data<-read.table("gator.txt", header=T)
              #https://newonlinecourses.science.psu.edu/stat504/node/226/
              #https://newonlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson08/gator/index.txt
       
       
data <- 
  
  
  

  
  
  
  
# 2. activate function

table_convert2_data = function(x,a,b){
  n <- apply(x[,c(a:b)],1,sum)
  result <- x[rep(seq.int(1,nrow(x)),n),]
  row.names(result)=NULL
  return(result)
}





# 3. input parameters

#example
r = c("F","I","R","B","O") # response level ; ; colnames(data)[5:9]
a = 5 # 1st column of counts
b = 9 # Last column of counts
      #a and b is the column which decide the amount of duplication


r = 
a = 
b = 



  
  
  
# 4. execute following command
  
  
x <- data
n <- apply(x[,c(a:b)],1,sum) #row total from column a till b
x <- table_convert2_data(x,a,b)

Response <- matrix(rep(NA,nrow(x)*max(n)),nrow=nrow(x))

for (i in 1:nrow(data)){
  Response[i,c(1:n[i])] <- rep(r,data[i,c(a:b)])
}

Response<-t(Response)
Response <- matrix(Response,ncol=1)
Response[!is.na(Response)] -> Response
x$Response <- Response

x <- x[,-c(a:b)]


head(x) ; tail(x)






# 5. set response as factor
x$Response <- as.factor(x$Response)




