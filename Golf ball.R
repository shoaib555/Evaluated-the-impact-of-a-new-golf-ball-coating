#Loading the data and library for analysis:
library(readxl)
go<-read_excel("Go.xls")
summary(go)

#Visulization of the data to check the distribution of the distance between the 
#balls and determine outliers and skewness of the data
par(mfrow=c(2,2))
hist(go$Current,xlab="Current(Yards)",ylab="Frequency",main="Current ",col="Blue")
hist(go$New,xlab="New(Yards)",ylab="Frequency",main="New",col="Orange")
boxplot(go$Current,main="Current",col="blue")
boxplot(go$New,main="New",col="orange")

#Perfrom F-test to check variance
var.test(go$Current,go$New,alternative = "two.sided")

#Establishing the Critical value
#Formulating Hypothesis Test using 2 sample T test unequal Variance.
   #H0=No Difference between the current and the new model of balls 
   #HA=The New Ball has a different driving distance than the current ball
res<-t.test(go$Current,go$New,var.equal = T,alternative = "two.sided",paired = F)
res
#95% CI Current
t.test(go$Current,alternative = "two.sided")

#95% CI New
t.test(go$New,alternative = "two.sided")

#Power of the test
delta=mean(go$Current)-mean(go$New)
delta
sd(go$Current)
sd(go$New)
pooleSD=(((40-1)*(8.75^2)+(40-1)*(9.9^2))/(40+40-2))^0.5
pooleSD
power.t.test(n=40,delta = 2.775,sd=9.34,sig.level = 0.05,type="two.sample",alternative = "two.sided")

##How many sample required to increase power
power.t.test(power=0.95,delta=2.775,sd=9.34,sig.level = 0.05,type="two.sample",alternative = "one.sided")

##Conclusion
#with the given data it shows there isn't any significant test to determine if New ball is better than the current ball.
# The power of the test determines that more data approx 245 observation are required to increase the demarcation of
#null and alternative hypothesis
