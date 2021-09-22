par(mfrow=c(1,3))
hist(rnorm(100),border="black",col="steelblue")
hist(rnorm(100),border = "black",col = "darkgreen")
hist(rnorm(100),border = "black",col = "darkred")

#The histograms are not the same

#Question 2
par(mfrow=c(1,2))
q<-scan(text=" 26 30 54 25 70 52 51 26 67 18 21 29 17 12 18 35 30 36 21 24 18 10 43 28 15 26 27")
hist(q,probability = TRUE,col = "purple",cex.axis=1,breaks = 5,main = "Bandwidth 1")
lines(density(q,adjust = 1),col="darkgreen",lwd=.9)

hist(q,probability = TRUE,col = "purple",cex.axis=1,breaks = 5,main="Bandwith 2")
lines(density(q,adjust = 2),col="darkgreen",lwd=.9)


##Questio 3
mean(q)
median(q)
#The mean is 30.7037 while the median is 26.The mean is bigger than the median value
sd(q)
IQR(q)
mad(q)
#IQR =16  a value greater than sd and mad. The standard deviation is 15.83767 and mad 11.8608

#Question 4
w<-boxplot(q)
outliers
w$out
#Outliers are he values 70 and 67

#mean of data with outlier
mean(q)
#mean of data without outleir
#removing the outlier
qq<-q[-c(70,67)]
str(qq)



##Question 1
cyl<-factor(scan(text="6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4"))
cyl
am<-factor(scan(text="1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1")) 
am
levels(am)<-c("auto","manual")
am
table(am, cyl)

#b
levels(cyl) <- c("Sm", "Sm", "Lg")
cyl
tab=table(cyl,am) 
tab
chisq.test(tab)
# Here the  p-value is 0.02074 < 0.05 we  reject the null hypothesis and conclude that we are 95% confident there is significant evidence of association between many cylinders and automatic transmissions.

#Question 3
#a
data("faithful")
head(faithful)
str(faithful)

#b
hist(faithful$eruptions,col="steelblue")#Eruption histogram
hist(faithful$waiting,col="red")#Eruption histogram

#C
plot(faithful$eruptions,faithful$waiting,pch=6,col="darkgreen",cex=0.5)
#d


#Question 3
model<-lm(faithful$waiting~faithful$eruptions)
abline(model,col="red")

#b
#There is a positive relationship between  waiting and eruptions

#c
model$residuals
#There is  an indication of
