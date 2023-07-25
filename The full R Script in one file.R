library(TeachingDemos)
txtStart("Replication Study.txt")
#Replication study

#Libraries
pkgs <- c('haven', 'dplyr','carData', 'car', 'foreign', 'ggplot2', 'summarytools', 'stargazer', 'lmtest', 'robustbase')
install.packages(pkgs)
library(haven); library(dplyr); library(carData); library(car); library(foreign); library(ggplot2); library(summarytools)
library(stargazer); library(robustbase); library(lmtest)

#Loading Data
data4 <- read.dta("paper4.dta")
attach(data4)

# Defining Functions
diff_aver <- function(x){
  after <-subset(x,data4$time==1); before <-subset(x,data4$time==0)
  return(mean(after,na.rm = TRUE)-mean(before,na.rm=TRUE))
}
diff_st_var <- function(x){
  after <-subset(x,data4$time==1); before <-subset(x,data4$time==0)
  return(sd(after,na.rm = TRUE)-sd(before,na.rm=TRUE))
}


tables <- by(data4,data4$time, function(x) descr(x,stats = c("n.valid","mean","sd"),transpose = TRUE,style = "grid"))
print(tables, method = "pander")

# Playing with data
diff_aver(data4$selfemployment_top)
diff_aver(data4$agriculture_top)
diff_aver(data4$female)
diff_aver(data4$rain_sd)
diff_aver(data4$ageyr)
round(diff_aver(data4$size), 2)

year_before <- round(2010-diff_aver(data4$ageyr))
year_before

round(diff_aver(data4$district_population_pc), 2)
round(diff_st_var(data4$district_population_pc),2)

round(diff_aver(data4$school_grade), 2)
round(diff_st_var(data4$school_grade),2)

round(diff_st_var(data4$ageyr),4)

#Defining Plot on work
data4.agg <- data4 %>%
  group_by(time) %>%
  summarise(agriculture_top = sum(agriculture_top, na.rm = T),
            selfemployment_top = sum(selfemployment_top, na.rm = T),
            employee_top = sum(employee_top, na.rm = T),
            farming_livestock = sum(farming_livestock, na.rm = T),
            trade = sum(trade, na.rm = T),
            professional = sum(professional, na.rm = T),
            government = sum(government, na.rm = T),
            retirement = sum(retirement, na.rm = T))

data4.agg <- rename(data4.agg, "Period" = "time")

data4.agg <- reshape(as.data.frame(data4.agg),
                     direction = "long",
                     idvar = "Period",
                     varying = colnames(data4.agg)[-1],
                     v.names = "value",
                     times = c("Agriculture","Self Employment","Employee","Farming","Trade","Professional","Govenment","retired"))

data4.agg$Period <- factor(data4.agg$Period,
                           levels = c(0,1),
                           labels = c("Pre","Post"))

data4.agg$time <- factor(data4.agg$time,
                         levels = c("Agriculture","Self Employment","Employee","Farming","Trade","Professional","Govenment","retired"))

# Printing plot on work
ggplot(data4.agg, aes(x = time, y = value, group = Period, fill = Period)) +
  stat_summary(geom = "bar", position = "dodge", fun.y = sum) +
  xlab("Type of Work") +
  ylab("Number of observations")

# Main Regressions
ln_eucldist_bu <- log(1/data4$time_eucldist_bu)
# Not fixed
Reg1 <- lm(agriculture_top~ln_eucldist_bu+married+ageyr
           +factor(school_grade)+head+child_adult_7
           +female+size+married_head+rain_sd
           +district_population_pc+factor(ward)
           +time,data=data4)
Reg2<- lm(selfemployment_top~ln_eucldist_bu+married+ageyr
          +factor(school_grade)+head+child_adult_7
          +female+size+married_head+rain_sd
          +district_population_pc+factor(ward)
          +time,data=data4)
Reg3 <- lm(employee_top~ln_eucldist_bu+married+ageyr
           +factor(school_grade)+head+child_adult_7
           +female+size+married_head+rain_sd
           +district_population_pc+factor(ward)
           +time,data=data4)
#Fixed
Reg4 <- lm(agriculture_top~ln_eucldist_bu+married+ageyr
           +factor(school_grade)+head+child_adult_7
           +female+size+married_head+rain_sd
           +district_population_pc+factor(ward)
           +time+factor(id),data=data4)
Reg5 <- lm(selfemployment_top~ln_eucldist_bu+married+ageyr
           +factor(school_grade)+head+child_adult_7
           +female+size+married_head+rain_sd
           +district_population_pc+factor(ward)
           +time+factor(id),data=data4)
Reg6 <- lm(employee_top~ln_eucldist_bu+married+ageyr
           +factor(school_grade)+head+child_adult_7
           +female+size+married_head+rain_sd
           +district_population_pc+factor(ward)
           +time+factor(id),data=data4)

# Discussion Part
coeftest(Reg1, vcov=hccm(Reg1, type="hc0"))
coeftest(Reg2, vcov=hccm(Reg2, type="hc0"))
coeftest(Reg3, vcov=hccm(Reg3, type="hc0"))

coeftest(Reg4, vcov=hccm(Reg4, type="hc0"))
coeftest(Reg5, vcov=hccm(Reg5, type="hc0"))
coeftest(Reg6, vcov=hccm(Reg6, type="hc0"))

# Test for heteroskedasticity
bptest(Reg1)
bptest(Reg2)
bptest(Reg3)
bptest(Reg4)
bptest(Reg5)
bptest(Reg6)

# Controll for heteroskedasticity

# Not fixed
Reg1_rob <- lm(agriculture_top~ln_eucldist_bu+married+ageyr
               +factor(school_grade)+head+child_adult_7
               +female+size+married_head+rain_sd
               +district_population_pc+factor(ward)
               +time,data=data4)
Reg2_rob<- lmrob(selfemployment_top~ln_eucldist_bu+married+ageyr
                 +factor(school_grade)+head+child_adult_7
                 +female+size+married_head+rain_sd
                 +district_population_pc+factor(ward)
                 +time,data=data4)
Reg3_rob <- lmrob(employee_top~ln_eucldist_bu+married+ageyr
                  +factor(school_grade)+head+child_adult_7
                  +female+size+married_head+rain_sd
                  +district_population_pc+factor(ward)
                  +time,data=data4)
#Fixed
Reg4_rob <- lmrob(agriculture_top~ln_eucldist_bu+married+ageyr
                  +factor(school_grade)+head+child_adult_7
                  +female+size+married_head+rain_sd
                  +district_population_pc+factor(ward)
                  +time+factor(id),data=data4)
Reg5_rob <- lmrob(selfemployment_top~ln_eucldist_bu+married+ageyr
                  +factor(school_grade)+head+child_adult_7
                  +female+size+married_head+rain_sd
                  +district_population_pc+factor(ward)
                  +time+factor(id),data=data4)
Reg6_rob <- lmrob(employee_top~ln_eucldist_bu+married+ageyr
                  +factor(school_grade)+head+child_adult_7
                  +female+size+married_head+rain_sd
                  +district_population_pc+factor(ward)
                  +time+factor(id),data=data4)

# Main and Discussion output
stargazer(Reg1, Reg1_rob, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
stargazer(Reg2, Reg2_rob, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
stargazer(Reg3, Reg3_rob, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
stargazer(Reg4, Reg4_rob, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
stargazer(Reg5, Reg5_rob, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
stargazer(Reg6, Reg6_rob, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
