library(readr)
library(dplyr)
library(tidyverse)
options(scipen = 999)
mlb2013 <- read_csv(file = url("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Instructor/mlb2013.csv"))
mlb2002 <- read_csv(file = url("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Instructor/mlb2002.csv"))


#Random Sample
mlbSample <- mlb2013[sample(nrow(mlb2013), 200), ]


#1 - What is the 95% Confidence Interval that MLB baseball players over the mean age make more than
#mean salary?
musal <- mean(mlb2013$salary)
muage <- mean(mlb2013$age)


#In 2002, the average salary of all baseball players was $2,384,779, with a standard deviation of $3,067,715. 
#In 2013, the mean salary of all baseball players was $3,334,482, with a standard deviation of $4,932,139. 
#Using a random sample of 152 and 159, respectively, conduct a hypothesis test at the 5% significance level 
#to determine whether the mean salary of all baseball players in 2002 is less than the mean salary of all 
#baseball players in 2013.

mean(mlb2002$salary)
sd(mlb2002$salary)
mean(mlb2013$salary)
sd(mlb2013$salary)
      
#mean sal 2002
ybar1 <- mean(mlb2002$salary)
s1 <- sd(mlb2002$salary)
n1 <- 152
      #mean sal 2013
ybar2 <- mean(mlb2013$salary)
s2 <- sd(mlb2013$salary)
n2 <- 159
      
df <- (s1^2/n1 + s2^2/n2)^2/(1/(n1 - 1)*(s1^2/n1)^2 + 1/(n2 - 1)*(s2^2/n2)^2)
df
tstat <- (ybar1 - ybar2)/sqrt(s1^2/n1 + s2^2/n2)
tstat
LTpval <- pt(tstat,df)
LTpval
      
      


#3 - Test an appropriate hypothesis to see if there is evidence that pitchers in the AL East division make a greater salary 
#than pitchers in the AL West division. Using a random sample, the salaries of 10 pitchers in the two divisions are given below. 
#Find a 90% confidence interval for the difference of players’ salaries in the two divisions.


AlEast <- mlb2013 %>% select(position, division, salary) %>%
                      filter(position == "Starting pitcher",
                             division == "AL East")
AlWest <- mlb2013 %>% select(position, division, salary) %>% 
                      filter(position == "Starting pitcher",
                             division == "AL West")

AlEastSamp <- AlEast[sample(nrow(AlEast), 10), ]
AlWestSamp <- AlWest[sample(nrow(AlWest), 10), ]

AlEastSamp$salary
AlWestSamp$salary

eastsalary <- c(15950000, 518500, 3000000, 15000000, 508500, 12000000, 516500, 3700000, 5000000, 499500) 
westsalary <-  c(500700, 506000,  495000,  492500, 2000000, 3725000,  492500, 480000, 3400000, 5200000)
diffsalary <- eastsalary - westsalary

hist(diffsalary)

conf.level <- 0.90
n <- length(diffsalary)
df <- n - 1
tstar <- qt((1 + conf.level)/2, df)

LCL <- mean(diffsalary) - tstar*sd(diffsalary)/sqrt(n)
UCL <- mean(diffsalary) + tstar*sd(diffsalary)/sqrt(n)

CI <- c(LCL, UCL)

cat("The ", conf.level*100, " % confidence interval for mu_difference is", CI)

tobs <- mean(diffsalary)/(sd(diffsalary)/sqrt(n))

LT <- pt(tobs,df)

GT <- 1 - pt(tobs,df)

Tail <- 2*min(LT,GT)

cat("The t-stat is",tobs,"\n P-value will be \n")

list(LessThan = LT, GreaterThan = GT,TailArea = Tail)


#4 Chi-squared Test (independence)
#Using a random sample of 200 MLB players, we will use a Chi-squared test to 
#determine the relationship between age and salary.
#H0: Age and salary are indepedent
#HA: Age and salary are dependent

mlbage1 <- mlbSample %>% 
  filter(age < 27,
         salary < 1200000)

mlbage2 <- mlbSample %>% 
  filter(age < 27,
         salary >= 1200000)

mlbage3 <- mlbSample %>% 
  filter(age > 27,
         salary < 1200000)

mlbage4 <- mlbSample %>% 
  filter(age > 27,
         salary >= 1200000)
         
#chi-square independence
agesal <- matrix(c(87, 22, 26, 65), 
                 ncol = 2, 
                 byrow = TRUE)
colnames(agesal) <- c("LT 1200000","GT= 1200000")
rownames(agesal) <- c("LT= 27", "GT 27")

obstable <- as.table(agesal)
obstable

chisq.test(obstable)

ChiSq <- chisq.test(as.table(obstable), correct = FALSE)
ChiSq

names(ChiSq)

ChiSq$observed
ChiSq$expected
ChiSq$residuals
ChiSq$stdres
ChiSq$p.value
ChiSq$stat



