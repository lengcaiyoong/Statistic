

  #parameters
    #grp_data (x variables at left side, matrix format)
        #name column names as Y variable 
        #name row names as X variable   

  #EXAMPLE
          #grp_data <- matrix(c(24,35,21,30,1355,603,192,224), nc=2) #x variable at left
          #rownames(grp_data) <- c("Never", "Occasional","Nearly every night", "Every night")


cat(
"\n","Name your matrix as 'grp_data'! ",
"\n","Name row using x variable!",
"\n","\n","\n",
"Eg. grp_data <- matrix(c(24,35,21,30,1355,603,192,224), nc=2) ","\n",
'Eg. rownames(grp_data)  <- c("Never", "Occasional","Nearly every night", "Every night") ',"\n","\n"
)


ANS <- readline(noquote("Have you name it? [Y/N]  : "))

if (ANS == "No" | ANS =="no" | ANS =="n" | ANS =="N" | ANS == "NO"){
  break("please name it")
}else{
}



colnames(grp_data) <- c("Yes","No")
y_var <- colnames(grp_data )
x_var <- rownames(grp_data )


grp_data.df <- data.frame(x=gl(length(x_var), 1, labels=x_var),
                         y=gl(length(y_var), 0.5*length(y_var)*length(x_var), labels=y_var),
                         counts=as.vector(grp_data))

grp_data.df <- grp_data.df[rep(seq_len(nrow(grp_data.df)), grp_data.df$counts), 1:2]


if(is.character(x_var) | is.factor(x_var)){
  #if x variable is factor
  grp_data.df$y <- abs(as.numeric(grp_data.df$y)-2)
}else{
  #if x variable is numeric
  levels(grp_data.df$x) <- x_var
  grp_data.df$x <- as.numeric(as.character(grp_data.df$x))  #convert x to numeric
  grp_data.df$y <- abs(as.numeric(grp_data.df$y)-2)
}



rownames(grp_data.df) <- seq_len(nrow(grp_data.df))

  print("grp_data.df")
  print(head(grp_data.df))
  
  
  
  
  
  