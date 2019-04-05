#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: April 2019
# Desc: Proposal Gant Chart

#clear environments
rm(list=ls())

library(reshape2)
library(ggplot2)

# Initialize tasks
tasks <- c("Review Literature", 
           "Data Exploration", 
           "Develop Model", 
           "Model Fitting",
           "Final Push",
           "Thesis Write-Up")

# Dataframe creation
dfr <- data.frame(
  name        = factor(tasks, levels = tasks),
  start.date  = as.Date(c("2019-04-08", "2019-05-01", "2019-05-13", "2019-05-13", "2019-07-15", "2019-05-01")),
  end.date    = as.Date(c("2019-04-30", "2019-05-10", "2019-06-13","2019-07-15", "2019-08-29", "2019-08-29"))
)
mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))

# Generate and save gant chart

png(filename = "Data/gantt.png", res = 100)
print(ggplot(mdfr, aes(value, name)) + 
  geom_line(size = 6, color = "deepskyblue2") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_bw())
dev.off()
