# Clean Working Environment

rm(list=ls())

# Notwendige Pakete in R laden

library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)
library(plyr)
library(apaTables)
library(apa)


# Datensatz von Github als csv Datei einlesen

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')


# Bildung von zwei Gruppen UdSSR/Russland und USA mit einem subset Befehl

russia <-subset(astronauts, astronauts$nationality=="U.S.S.R/Russia")

usa <- subset(astronauts, astronauts$nationality=="U.S.")


# Formulierung der Hypothese I

#### Hypothese I

#H0: Die Dauer der Raumfahrtmissionen der  UdSSR/Russland sind im Durchschnitt kürzer als Missionsdauer der USA
#    oder die Missionsdauer unterscheidet sich nicht zwischen den beiden Großmächten.  

#H0: µ usa$hours_mission ≥ russia$hours_mission



# H1: Die Dauer der Raumfahrtmissionen der UdSSR/Russland ist im Durchschnitt länger als die Missionsdauer der USA.

# H1: µ usa$hours_mission < russia$hours_mission


# Deskriptive Lagemaße Berechnen

summary(russia$hours_mission)
summary(usa$hours_mission)

sd(russia$hours_mission)
sd(usa$hours_mission)

str(usa$hours_mission)


# T-test durchführen

t.test(russia$hours_mission, usa$hours_mission, alternative = "greater")


# Apa Format erzeugen

t_apa(t_test(russia$hours_mission, usa$hours_mission, alternative = "greater"))


# Formulierung der Hypothese II

#### Hypothese II


#H0: Je länger die Missionsdauer, desto kürzer die Dauer der Außenbordeinsätze während der Mission
#    oder die Missionsdauer hat keinen Einfluss auf die Dauer der Außenbordeinsätze.

#    β ≤ 0



#H1: Je länger die Missionsdauer, desto länger die Dauer der Außenbordeinsätze während der Mission.

#    β > 0


# Lineares Regressionsmodell aufstellen 

modelI <- lm(astronauts$eva_hrs_mission ~ astronauts$hours_mission)


# Lineares Regressionsmodell ins Apa Format übertragen 

apa.reg.table(modelI, filename = "modelI.doc", table.number = 2)




