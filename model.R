# Clean Working Environment

rm(list=ls())

# Loading of packages

library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)
library(plyr)
library(apaTables)
library(apa)

# Dataload from Github

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')


# Building the subset of two groups UdSSR/Russia and USA

russia <-subset(astronauts, astronauts$nationality=="U.S.S.R/Russia")

usa <- subset(astronauts, astronauts$nationality=="U.S.")


#### Hypothesis I

#  H0: The duration of the UdSSR/Russian space missions are shorter on average, as the missions conducted by the USA, or there is no difference in time between
#    the space missions of both superpowers. 

#  H0: µ usa$hours_mission ≥ µ russia$hours_mission


#  H1: The duration of the UdSSR/Russian space missions are longer on average, as the missions conducted by the USA.

#  H1: µ usa$hours_mission < µ russia$hours_mission


# Descriptive statistics

summary(russia$hours_mission)
summary(usa$hours_mission)

sd(russia$hours_mission)
sd(usa$hours_mission)

str(usa$hours_mission)


# T-test 

t.test(russia$hours_mission, usa$hours_mission, alternative = "greater")


# Apa format for publication

t_apa(t_test(russia$hours_mission, usa$hours_mission, alternative = "greater"))


#### Hypothesis II


#H0: The longer the space mission, the shorter the space operations (repair) during the mission, 
#    or the duration of tha space mission has no influence on the duration of the space operations.

#    β ≤ 0


#H1: The longer the space mission, the shorter the space operations.

#    β > 0


# Linear regression model I 

modelI <- lm(astronauts$eva_hrs_mission ~ astronauts$hours_mission)


# Linear regression model I in Apa format for publication 

apa.reg.table(modelI, filename = "modelI.doc", table.number = 2)




