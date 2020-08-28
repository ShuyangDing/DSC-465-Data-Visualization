library(tidyverse)

#1.a
FSBC <- read.delim("C:/Users/shuya/OneDrive/Documents/Depaul/DSC465/Week4/DATA VISUALIZATION - 2019-2020 Winter - 222020 - 532 PM/FoodSrvcByCounty.txt")
#Reformat FSBC table - States
FSBC_State <- FSBC %>%
  filter(County != "UNITED STATES" & State == "")%>%
  mutate(state = tolower(County))%>%
  rowwise()%>%
  mutate(FoodServices = median(c(FoodServices.97, FoodServices.2002, FoodServices.2007, na.rm = TRUE)))
head(FSBC_State)
#Merge States map and FSBC map
states_map <- map_data("state")
FSBC_State_Map <- left_join(states_map, FSBC_State, by = c("region" = "state"),na.rm=TRUE)
head(FSBC_State_Map)
#Map
ggplot(FSBC_State_Map, aes(x=long,y=lat,group=group, fill = FoodServices))+ geom_polygon(colour=NA)

  
#1.b
#Reformat FSBC table - County
FSBC_County <- FSBC %>%
  filter(County != "UNITED STATES" & State != "")%>%
  mutate(county  = tolower(County))%>%
  rowwise()%>%
  mutate(FoodServices = median(c(FoodServices.97, FoodServices.2002, FoodServices.2007, na.rm = TRUE)))%>%
  mutate(FoodServices_Level = cut(FoodServices, breaks=c(-Inf,15,32,65,165,16406),
                                  labels=c("Low","Lo-Medium","Medium","Medium-High","High")))
head(FSBC_County)
#Merge County map and FSBC map
county_map <- map_data("county")
FSBC_County_Map <- left_join(county_map, FSBC_County, by = c("subregion" = "county"),na.rm=TRUE)
head(FSBC_County_Map)
#Map
ggplot()+geom_polygon(data=FSBC_County_Map, aes(x=long,y=lat,group=group,fill=FoodServices_Level))+
  geom_polygon(data=FSBC_State_Map, aes(x=long,y=lat,group=group),color="black",size=1,fill=NA)+
  scale_fill_brewer(palette="PuBu",direction=-1)+theme_bw()

#1.c
install.packages("cartogram")
library(cartogram)
cartogram()


#3.a
install.packages('tidyquant')
library(tidyquant)
install.packages('chron')
require(chron)
PWL <- read.csv("C:/Users/shuya/OneDrive/Documents/Depaul/DSC465/Week4/DATA VISUALIZATION - 2019-2020 Winter - 222020 - 532 PM./PortlandWaterLevel2003.csv")
Date_Time <- PWL %>%
  unite("date_string",c(Date,Time),sep="/")
head(Date_Time)
PWL_Final <- Date_Time%>%
  mutate(Date = parse_date_time(date_string,"%m/%d/%Y/%H%M"))
head(PWL_Final)
WL_Plot <- ggplot(PWL_Final, aes(x=Date, y=WL))
WL_Plot + geom_ma(ma_fun=SMA,n=407,size=1, color="red")

#4
ggplot(PWL_Final, aes(x=Date, y=WL,fill=WL))+ geom_bar(stat="identity")+
  scale_fill_gradient2(low = hsv(0.6,0.33,1), mid = hsv(0.7,0.6,0.4),high=hsv(0.8,1,0.09),midpoint=median(PWL_Final$WL),
                        space="Lab",na.value="grey50")

