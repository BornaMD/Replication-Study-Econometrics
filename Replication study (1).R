library(TeachingDemos)
txtStart("Replication Study")
#Replication study

#Libraries
pkgs <- c('haven', 'dplyr', 'car', 'foreign', 'ggplot2', 'psych')
libs <- c(haven, dplyr, car, foreign', 'ggplot2', 'psych')
install.packages(pkgs)
library(pkgs)
#Loading Data
data4 <- read_stata("paper4.dta")
attach(data4)

# Defining Functions
part_I <- function(x) {
  return(c((length(na.omit(x))) ,
           (sd(na.omit(x)))
           (mean(na.omit(x)))))
}

sum_s <- function(x) {
  return(c((length(na.omit(x))) ,
  (sd(na.omit(x)))
  (summary(na.omit(x)))))
}
regr_s <- function(x) {
  return(c((x),
  (summary(x)),
  (lenght(x$fitted.values)),
  (x$coefficients)))
}

# Playing with data

#sum_s(data4)
#summary(data4)
#sd(data4$agriculture_top)

#summary(data4$time_weighted_eucldist)
#sum_s(data4$time)

#summary(data4$rain_sd)

describe(data4)





barplot(height = data4['ward'])
counts <- table(data4['ward'])
barplot(counts, main="wardNum", ylab="Frequency")

newdata <- subset(mydata, age >= 20 | age < 10,
                  select=c(ID, Weight)) 

part_I(data4$agriculture_top)
part_I(data4$selfemployment_top)
part_I(data4$employee_top)
part_I(data4$time_eucldist_bu)
part_I(data4$time_eucldist_rw)
part_I(data4$ageyr)
part_I(data4$female)
part_I(data4$head)
part_I(data4$married)
part_I(data4$district_population_pc)
part_I(data4$rain_sd)

#Defining Plot


#Emptying Workspace
detach()
rm(list=ls())
txtStop()


