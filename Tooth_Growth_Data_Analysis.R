#Question 1 : Show the sample mean and compare it to the theoretical mean distribution
n <- 40
Simulations <- 1000
Lambda <- 0.2
SampleMean <- NULL
for(i in 1:Simulations) {
  SampleMean <- c(SampleMean, mean(rexp(n, Lambda)))
}
mean(SampleMean)

#Question 2: Show the sample variance and compare it to the thoretical variance of the distribtution.
#The theoretical standard deviation of the distribution is also 1/lambda , which, for a lambda of 0.2 , equates to 5 . The variance is the square of the standard deviation, which is 25 
Variance <- var(SampleMean)
0.6 is close to the theoretical distribution.
#0.6 is close to the theoretical distribution.
#Show that the distribution is appoximately normal
hist(SampleMean, breaks = n, prob = T, col = "blue", xlab = "Means")
x <- seq(min(SampleMean), max(SampleMean), length = 100)
lines(x, dnorm(x, mean = 1/Lambda, sd = (1/Lambda/sqrt(n))), pch = 25, col = "green")
hist(SampleMean, breaks = n, prob = T, col = "blue", xlab = "Means")
x <- seq(min(SampleMean), max(SampleMean), length = 100)
lines(x, dnorm(x, mean = 1/Lambda, sd = (1/Lambda/sqrt(n))), pch = 25, col = "green")
# Normal QQ Plot
qqnorm(SampleMean)
qqline(SampleMean, col = "blue")
#The distribution averages of 40 exponentials is very close to a normal distribution


#Part 2: Basic Inferential Data Analysis Instructions
#We are going to analyze the ToothGrowth data in the R datasets package.
#Load the ToothGrowth data and perform some basic exploratory data analysis
library(datasets)
data(ToothGrowth)
library(ggplot2)

str(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)
ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ supp) +
  xlab("Dose(mg)") +
  ylab("Tooth length")
#hypothesis tests to compare tooth growth by supp and dose.
hypoth1 <- t.test(len ~ supp, data = ToothGrowth)
hypoth1$conf.int
hypoth1$p.value
hypoth2<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 0.5))
hypoth2$conf.int
hypoth2$p.value
hypoth3<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 1))
hypoth3$p.value
hypoth4<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 2))
hypoth4$conf.int
hypoth4$p.value

#Conclusions
OJ ensures more tooth growth than VC for dosages 0.5 & 1.0. OJ and VC givesthe same amount of tooth growth for dose amount 2.0 mg/day. For the entire trail we cannot conclude OJ is more effective that VC for all scenarios.