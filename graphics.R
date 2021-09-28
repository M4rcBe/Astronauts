# Loading Packages
library(tidyverse)
library(janitor)
library(knitr)

# Loading data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')


# Building gender specific subsets 
female <-subset(astronauts, astronauts$sex=="female")

male <- subset(astronauts, astronauts$sex=="male")

female_hours <- female$hours_mission
male_hours <- male$hours_mission
female_years <- female$year_of_mission
male_years <- male$year_of_mission

female_d <- data.frame(female_hours, female_years)
male_d <- data.frame(male_hours, male_years)


mean_female_hours <- female_d %>%
       group_by(female_years) %>%
       summarise(mean = mean(female_hours))

mean_male_hours <- male_d %>%
       group_by(male_years) %>%
       summarise(mean = mean(male_hours))


## Relying on ggplot2 for visualization

# I Time series diagram

p <- ggplot() +
       geom_line(data=mean_female_hours, aes(x=female_years, y=mean, colour="Weiblich"),
                size=1)+
       geom_line(data=mean_male_hours, aes(x=male_years, y=mean, colour="Männlich"), 
                 size=1)+
       labs(x = "Jahre", y = "Durchschnittliche Missionsdauer in Stunden", title = "Weltraummissionen (1961-2020) nach Geschlecht")+
       scale_color_manual(values= c("midnightblue","maroon4"), name = "Legende") +
                            theme_light()
 
p

pilot <- subset(astronauts, astronauts$occupation =="pilot")


female<-sum(pilot$sex =="female")
male<- sum(pilot$sex =="male")


d <- c(female,male)
Legende <- c("Weiblich","Männlich")

pilot_mf<- cbind(Legende,d)

pilot_mf <- data.frame(pilot_mf)

pilot_mf$d<- as.numeric(pilot_mf$d)

# II Barplot

d <- ggplot(pilot_mf, aes(x=Legende, y=d, fill=Legende)) + 
       geom_bar(stat="identity")+
       labs(x = "Geschlecht", y = "Absolute Häufigkeit", title = "Piloten/-innen bei Weltraummissionen (1961-2020) nach Geschlecht")+
       theme_light()+
       scale_fill_manual(values=c("midnightblue", "maroon4", name = "Lnde"))
       

d









