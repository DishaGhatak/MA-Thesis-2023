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
#Summary Statistics
#################

#The Summary Statistics Table

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


############################################################################
#Social Expenditure Per Capita - Alignment Regression Tables
############################################################################


### Model 1 (social exp ~ alignment, no fixed effects, no time trend)


Model1 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(GDPPC) +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                `Coalition at State`,
                 data=dataset))

stargazer(Model1, type = 'text') #viewing model 1 coefficients


### Model 2 (social exp ~ alignment with state fixed effects, no time trend)


Model2 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                as.factor(States)-1,
                data=dataset))

stargazer(Model2, type = 'text', omit = c(8:35)) #viewing model 2 coefficients


### Model 3 (social exp ~ alignment with state fixed effects and a linear time trend)


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


stargazer(Model3, type = 'text', omit = 9:37) #viewing model 3 coefficients


stargazer(Model1, Model2, Model3, type = 'text', omit = 10:36) #putting the coefficients of model 1, 2, and 3, together into the table


### Model 4 (social exp ~ alignment*victorymargin, no fixed effects, no time trend)


Model4 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS,
                data=dataset))


stargazer(Model4, type = 'text') #viewing model 4 coefficients


### Model 5 (social exp ~ alignment*victorymargin with state fixed effects, no time trend)


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


stargazer(Model5, type = 'text', omit = c(8:35)) #viewing model 5 coefficients


### Model 6 (social exp ~ alignment*victorymargin with state fixed effects and linear time trend)


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


stargazer(Model6, type = 'text', omit = c(10:36)) #viewing model 6 coefficients


stargazer(Model4, Model5, Model6, type = 'text', omit = c (10:36)) #putting the coefficients of model 4, 5, and 6, together into the table


# stargazer(Model1, Model2, Model3, Model4, Model5, Model6, omit = c (10:36), type = 'latex', out = 'regtab.tex') #putting the coefficients of models 1, 2, 3, 4, 5, and 6, together into the final reg table


######################
#Appendix Tables
######################


### Models 1, 2, and 3 with alternative alignment variable

Model11 <- (lm(log(SocExpPC) ~ Alignment2 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State`,
              data=dataset))


Model21 <- (lm(log(SocExpPC) ~ Alignment2 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                as.factor(States)-1,
              data=dataset))


Model31 <- (lm(log(SocExpPC) ~ Alignment2 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Trend +
                as.factor(States)-1,
              data=dataset))


stargazer(Model11, Model21, Model31, type = 'text', omit = 10:36)

stargazer(Model11, Model21, Model31, type = 'latex', out = 'tablea1', omit = 10:36)


### Models 4, 5, and 6 with alternative alignment variable


Model41 <- (lm(log(SocExpPC) ~ Alignment2 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS,
              data=dataset))


Model51 <- (lm(log(SocExpPC) ~ Alignment2 +
                log(GDPPC) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS +
                as.factor(States)-1,
              data=dataset))

Model61 <- (lm(log(SocExpPC) ~ Alignment2 +
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


stargazer(Model41, Model51, Model61, type = 'text', omit = c (10:36)) 

stargazer(Model41, Model51, Model61, type = 'latex', out = 'tablea2', omit = c (10:36)) 

### Models 1, 2, and 3 controlling for level of urbanization

#checked for skeweness in the variable, moderately right-skewed, so will take log of agriNDP like we did with gdppc
#install.packages("moments")
#library(moments)
#skewness(dataset$GDPPC, na.rm = TRUE)
#skewness(dataset$AgriNDP, na.rm = TRUE)


Model12 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(AgriNDP) +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                 `Coalition at State`,
               data=dataset))


Model22 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(AgriNDP) +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                 `Coalition at State` +
                 as.factor(States)-1,
               data=dataset))


Model32 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(AgriNDP) +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                 `Coalition at State` +
                 Trend +
                 as.factor(States)-1,
               data=dataset))


stargazer(Model12, Model22, Model32, type = 'text', omit = 10:36)

stargazer(Model12, Model22, Model32, type = 'latex', out = 'tablea3', omit = 10:36)

### Models 4, 5, and 6 controlling for level of urbanization


Model42 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(AgriNDP) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS,
              data=dataset))


Model52 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(AgriNDP) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS +
                as.factor(States)-1,
              data=dataset))

Model62 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(AgriNDP) +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS +
                Trend +
                as.factor(States)-1,
              data=dataset))


stargazer(Model42, Model52, Model62, type = 'text', omit = c (10:36))

stargazer(Model42, Model52, Model62, type = 'latex', out = "tablea4", omit = c (10:36))


### Models 1, 2, and 3 controlling for literacy levels

Model13 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(GDPPC) +
                 LitRate +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                 `Coalition at State`,
               data=dataset))


Model23 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(GDPPC) +
                 LitRate +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                 `Coalition at State` +
                 as.factor(States)-1,
               data=dataset))


Model33 <- (lm(log(SocExpPC) ~ Alignment1 +
                 log(GDPPC) +
                 LitRate +
                 HDI +
                 ElectionYear1 +
                 Turnout +
                 WinmargSS +
                 `Coalition at State` +
                 Trend +
                 as.factor(States)-1,
               data=dataset))


stargazer(Model13, Model23, Model33, type = 'text', omit = 11:37)

stargazer(Model13, Model23, Model33, type = 'latex', out = 'tablea5', omit = 11:37)


### Models 4, 5, and 6 controlling for literacy levels

Model43 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                LitRate +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS,
              data=dataset))


Model53 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                LitRate +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS +
                as.factor(States)-1,
              data=dataset))

Model63 <- (lm(log(SocExpPC) ~ Alignment1 +
                log(GDPPC) +
                LitRate +
                HDI +
                ElectionYear1 +
                Turnout +
                WinmargSS +
                `Coalition at State` +
                Alignment1*WinmargSS +
                Trend +
                as.factor(States)-1,
              data=dataset))


stargazer(Model43, Model53, Model63, type = 'text', omit = c (11:37)) 

stargazer(Model43, Model53, Model63, type = 'latex', out = 'tablea6', omit = c (11:37)) 

wtf
