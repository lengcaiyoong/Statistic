
#Parameters
#grp_data.df (as data frame)
#convert variable to factor if neccesary


#Example
##https://newonlinecourses.science.psu.edu/stat504/node/177/
    #grp_data<- read.table(url("https://newonlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson07/cheese/index.dat"),col.names = c("Cheese","Response","Count"))
    #grp_data$Response <- factor(grp_data$Response,ordered=T)




cat(
  "\n",
  "1. Please enter DATA FRAME with the name as 'grp_data'!",
  "\n","\n",
  "2. NAME the columns with x or y variable names!",
  "\n","\n",
  "3. Convert Y variable to order factor eg. factor(grp_data$Response,ordered=T)",
  "\n","\n",
  '4. If there are 1 X and 1 Y, it would have three columns, with the last column as "Count" eg. ', 
  "\n","\n"
  )

print(
head(read.table(url("https://newonlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson07/cheese/index.dat"),col.names = c("Cheese","Response","Count")))
)




ANS <- readline(noquote("Have you finished all of the above? [Y/N]  : "))

if (ANS == "No" | ANS =="no" | ANS =="n" | ANS =="N" | ANS == "NO"){
  break("please name it")
}else{
}


n_xvar = ncol(grp_data)-1

grp_data.df <- grp_data
grp_data.df <- grp_data.df[rep(seq_len(nrow(grp_data.df)), grp_data.df$Count), 1:n_xvar]

rownames(grp_data.df) <- seq_len(nrow(grp_data.df)) #rename row


print(head(grp_data.df))
print(table(grp_data.df))

print("NAME of data = grp_data.df")

print("REMINDER: remember to CHECK the class of column if it is factor or numeric")


