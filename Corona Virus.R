library(ggplot2)
library(tidyverse)
library(plotly)

#Geographic 
Coronavirus <- read.csv('C:/Users/shuya/OneDrive/Documents/Depaul/DSC465/Project/covid_19_data.csv')
Coronavirus_Format <- Coronavirus %>%
  group_by(Country.Region,Province.State)%>%
  summarise(Long = mean(Long), Lat=mean(Lat),Confirmed = max(Confirmed),Deaths = max(Deaths),Recovered = max(Recovered))

#World Map
worldmap <- map_data('world')

#Merge World Map and Coronavirus 
confirmed_worldmap <- left_join(worldmap, Coronavirus_Format, by = c("region" = "Country.Region"),na.rm=TRUE)
head(confirmed_worldmap)
#Text
confirmed_data <- Coronavirus_Format %>%
  arrange(Confirmed)%>%
  mutate(country=factor(Country.Region,unique(Country.Region))) %>%
  mutate(mytext=paste("Country: ", Country.Region,"\n",
                      "Province: ",Province.State,"\n",
                      "Confirmed Case: ",Confirmed, sep=''))

#Plot Map
confirm <- confirmed_data%>%
  ggplot()+geom_polygon(data=worldmap, aes(x=long,y=lat,group=group),colour='black',fill=NA)+
  geom_point(aes(x=Long,y=Lat,colour='Yellow',size=10,alpha=0.5,text= mytext))+
  theme_bw() + theme(legend.position = "none",axis.title = element_blank(),
                     axis.ticks = element_blank(),axis.text=element_blank())+
  labs(title = "COVID-19 - Confirmed As Of 3-1-2020")
#Add text to plot
confirm_1 <- ggplotly(confirm,tooltip = "text")
confirm_1



#Merge World Map and Coronavirus 
deaths_worldmap <- left_join(worldmap, Coronavirus_Format , by = c("region" = "Country.Region"),na.rm=TRUE)
head(deaths_worldmap)
#Text
deaths_data <- Coronavirus_Format  %>%
  filter(Deaths!=0)%>%
  arrange(Deaths)%>%
  mutate(country=factor(Country.Region,unique(Country.Region))) %>%
  mutate(deathtext=paste("Country: ", Country.Region,"\n",
                      "Province: ",Province.State,"\n",
                      "Deaths Case: ",Deaths, sep=''))
#Plot Map
deaths <- deaths_data%>%
  ggplot()+geom_polygon(data=worldmap, aes(x=long,y=lat,group=group),colour='black',fill=NA)+
  geom_point(aes(x=Long,y=Lat,colour='Red',size=10,alpha=0.5,text= deathtext))+
  theme_bw() + theme(legend.position = "none",axis.title = element_blank(),
                     axis.ticks = element_blank(),axis.text=element_blank())+
  labs(title = "COVID-19 - Deaths As Of 3-1-2020 ")
#Add text to plot
deaths <- ggplotly(deaths,tooltip = "text")
deaths

#Merge World Map and Coronavirus 
recovered_worldmap <- left_join(worldmap, Coronavirus_Format, by = c("region" = "Country.Region"),na.rm=TRUE)
head(recovered_worldmap)
#Text
recovered_data <- Coronavirus_Format %>%
  filter(Recovered!=0)%>%
  arrange(Recovered)%>%
  mutate(country=factor(Country.Region,unique(Country.Region))) %>%
  mutate(recoveredtext=paste("Country: ", Country.Region,"\n",
                         "Province: ",Province.State,"\n",
                         "Recovered Case: ",Recovered, sep=''))
#Plot Map
recovered <- recovered_data%>%
  ggplot()+geom_polygon(data=worldmap, aes(x=long,y=lat,group=group),colour='black',fill=NA)+
  geom_point(aes(x=Long,y=Lat,colour='Green',size=10,alpha=0.5,text= recoveredtext))+
  theme_bw() + theme(legend.position = "none",axis.title = element_blank(),
                     axis.ticks = element_blank(),axis.text=element_blank())+
  labs(title = "COVID-19 - Recovered As Of 3-1-2020 ")
#Add text to plot
recovered <- ggplotly(recovered,tooltip = "text")
recovered


