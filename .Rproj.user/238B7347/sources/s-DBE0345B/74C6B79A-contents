rm(list=ls())

# packages you will need for this script
library(foreign)
library(dplyr)
library(ggplot2)

# import the file
mydata <- read.dta("paper4.dta")

# aggregate the data into a new object
# for each time point (0, 1), sum the four variables
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


# the next command will create a variable called time, 
# so here I rename time to Period to prevent errors
# I use a capital P (Period and not period) so that it looks nicer in the graph later on
mydata.agg <- rename(mydata.agg, "Period" = "time")

# Currently we have the period variable and four variables of type of work
# ggplot2 requires type of work to be one single variable
# to do so, we reshape the data from wide (four variables) to long (one variable)
# varying = which variables to change (in this case, all but the first column)
# times = what categories to use in the new variable, use capital letters and spacing so it looks nice in the graph
# v.names = the name of the new numeric variable, I just chose value
# as.data.frame() is important. The previous commands store the object as a tibble. reshape only works with data.frames.
mydata.agg <- reshape(as.data.frame(mydata.agg),
                      direction = "long",
                      idvar = "Period",
                      varying = colnames(mydata.agg)[-1],
                      v.names = "value",
                      times = c("Agriculture","Self Employment","Employee","Farming","Trade","Professional","Govenment","retired"))

# Rather than use 0, 1 in the graph, change the labels to something meaningful
mydata.agg$Period <- factor(mydata.agg$Period,
                            levels = c(0,1),
                            labels = c("Pre","Post"))

# This is category variable created in the reshape command, called time.
# If you want to control the order of the categories in the graph, edit their order in levels
mydata.agg$time <- factor(mydata.agg$time,
                          levels = c("Agriculture","Self Employment","Employee","Farming","Trade","Professional","Govenment","retired"))

# the graph
# x = the four categories of type of work, called time because of the reshape command
# y = the variable to plot, value
# group, fill = split the bars by Period, and color them in different colors
# stat_summary = use a bar chart, place them next to each other (dodge), and give some function to y (doesn't matter which)
# xlab, ylab, controls titles of x axis and y axis
ggplot(mydata.agg, aes(x = time, y = value, group = Period, fill = Period)) +
  stat_summary(geom = "bar", position = "dodge", fun.y = sum) +
  xlab("Type of Work") +
  ylab("N")

