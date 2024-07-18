######################################################################
###### Disha Ghatak
###### Replication Code for "The Politics of Sub-National Social Policy:
###### Centre-State Relations in India"
###### Working Paper (September 2023 - present)
######################################################################

rm(list=ls())
library(foreign)
library(MASS)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(readr)


# load dataset from wherever it has been saved
Dissertation_Data <- read_csv("Dissertation Data.csv")
View(Dissertation_Data)
# Renaming the data set to something easier!
dataset <- Dissertation_Data
View(dataset)



#################
# Table 1: Descriptive Statistics
#################

desc_stat_tab <- data.frame(cbind( as.numeric(paste(dataset$Year)),
                                   as.numeric(paste(dataset$SocExp)),
                                   as.numeric(paste(dataset$SocExpPC)),
                                   as.numeric(paste(dataset$Alignment1)),
                                   as.numeric(paste(dataset$GDP)),
                                   as.numeric(paste(dataset$GDPPC)),
                                   as.numeric(paste(dataset$HDI)),
                                   as.numeric(paste(dataset$Population)),
                                   as.numeric(paste(dataset$Turnout)),
                                   as.numeric(paste(dataset$WinmargSS)),
                                   as.numeric(paste(dataset$`Coalition at State`)),
                                   as.numeric(paste(dataset$`Coalition at Centre`)),
                                   as.numeric(paste(dataset$AgriNDP)),
                                   as.numeric(paste(dataset$ElectionYear1))
                             ))

#Table as text 

stargazer(desc_stat_tab,
          median=TRUE,
          type = 'text',
          covariate.labels=c("Year", 
                             "Social Expenditure (in millions of INR)",
                             "Social Expenditure Per Capita (in INR)",
                             "Alignment", 
                             "State GDP (in millions of INR)",
                             "State GDP Per Capita (in INR)", 
                             "Human Development Index (HDI)",
                             "Population (in millions)", 
                             "Turnout (%)",
                             "Victory Margin (seat share %)",
                             "Coalition at State",
                             "Coalition at Centre",
                             "Agriculture/NDP (in millions of INR)",
                             "Election Year"))

#Table for Latex

stargazer(desc_stat_tab,
          median=TRUE,
          type = 'text',
          out = "stattbl.tex",
          covariate.labels=c("Year", 
                             "Social Expenditure (in millions of INR)",
                             "Social Expenditure Per Capita (in INR)",
                             "Alignment", 
                             "State GDP (in millions of INR)",
                             "State GDP Per Capita (in INR)", 
                             "Human Development Index (HDI)",
                             "Population (in millions)", 
                             "Turnout (%)",
                             "Victory Margin (seat share %)",
                             "Coalition at State",
                             "Coalition at Centre",
                             "Agriculture/NDP (in millions of INR)",
                             "Election Year"))

######################################################
# Table 2: Social Expenditure Per Capita - Alignment
######################################################

### Model 1

Model1 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(GDPPC) +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                `Coalition at State`,
                 data=dataset))

stargazer(Model1, type = 'text')


### Model 2

Model2 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                as.factor(States)-1,
                data=dataset))

stargazer(Model2, type = 'text', omit = c(8:35))

stargazer(Model1, Model2, type = 'text', omit = c(9:35))


### Model 3

Model3 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Trend +
                as.factor(States)-1,
                data=dataset))

stargazer(Model3, type = 'text', omit = 9:37)


stargazer(Model1, Model2, Model3, type = 'text', omit = 10:36)


Model4 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS,
                data=dataset))

stargazer(Model4, type = 'text')


Model5 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS +
                as.factor(States)-1,
              data=dataset))

stargazer(Model5, type = 'text', omit = c(8:35))


Model6 <- (lm(log(SocExpPC) ~ Alignment1 +
                          log(GDPPC) +
                          HDI +
                          ElectionYear1 +
                          Turnout +
                          WinmargSS +
                          `Coalition at State` +
                          Alignment1*WinmargSS +
                          Trend +
                          as.factor(States)-1,
                          data=dataset))

stargazer(Model6, type = 'text', omit = c(10:36))


stargazer(Model4, Model5, Model6, type = 'text', omit = c (10:36))


stargazer(Model1, Model2, Model3, Model4, Model5, Model6, omit = c (10:36), type = 'latex', out = 'regtab.tex')