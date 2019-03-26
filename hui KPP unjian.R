1. excel open 10 sheets for convenience analysis
2. °? instruction ?‘?ú script in R



A <- 1:171
B <- 172: 347
C <- 348: 522

sum(length(A),length(B),length(C))


#522 questions /8 groups = 65 to 66 questions in a group

a <- 1:66
b <- 67 : (67+64)
c <- ((67+64) + 1) :  ((67+64+1) + 64)
d <- (((67+64+1) + 64) +1) : ((((67+64+1) + 64) +1)+64)
e <- ( ((((67+64+1) + 64) +1)+64) +1) : ( ( ((((67+64+1) + 64) +1)+64) +1)+64)
f <- ( ( ( ((((67+64+1) + 64) +1)+64) +1)+64) +1) : ( ( ( ( ((((67+64+1) + 64) +1)+64) +1)+64) +1)+64)
g <- (( ( ( ( ((((67+64+1) + 64) +1)+64) +1)+64) +1)+64) +1) : ((( ( ( ( ((((67+64+1) + 64) +1)+64) +1)+64) +1)+64) +1)+64)
h <- ( ((( ( ( ( ((((67+64+1) + 64) +1)+64) +1)+64) +1)+64) +1)+64)+1) : ((( ((( ( ( ( ((((67+64+1) + 64) +1)+64) +1)+64) +1)+64) +1)+64)+1)+64)+1)


sample(a,10);sample(b,10);sample(c,10);sample(d,10);sample(e,10);sample(f,10);sample(g,10);sample(h,10)




data <- read.csv(file.choose())



Lable <- c("1:66", "67:131","132:196","197:261","262:326","327:391","392:456","457:522")








T <- c(
length(data[1:10,2][data[1:10,2]== TRUE]),

length(data[11:20,2][data[11:20,2]== TRUE]),

length(data[21:30,2][data[21:30,2]== TRUE]),

length(data[31:40,2][data[31:40,2]== TRUE]),

length(data[41:50,2][data[41:50,2]== TRUE]),

length(data[51:60,2][data[51:60,2]== TRUE]),

length(data[61:70,2][data[61:70,2]== TRUE]),

length(data[71:80,2][data[71:80,2]== TRUE]))





plot(1:8,T, type="o",ylim=c(0,10), axes=F, ann=FALSE)
abline(6,0,col="red")
axis(1, at=1:8, lab=Lable)
axis(2, las=1, at=0:10)
box()


##################


1. 总共有8个 Sections ，每个部分有10个题目。

2. Sec_1 是第一个section, 打  Sec_1  在R Console, 它会就显示10个题目

3. 题目做完后，答案请写在 Excel

4. 做完 Sec_1 后， 打 Sec_2 到 R Console, 会出现另外10个题目，
以此类推，做到 Sec_8 为止


5. 做完后叫 Gor Gor 就可以了

