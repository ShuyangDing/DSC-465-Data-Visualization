# Assignment 3
install.packages('devtools')
devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)

#1.a
#Add error column
PE <- read.csv('C:/Users/shuya/OneDrive/Documents/Depaul/DSC465/Week7/DATA VISUALIZATION - 2019-2020 Winter - 2222020 - 1256 AM/PerceptionExperiment.csv')
Error <- PE%>%
  mutate (Error = Response - TrueValue)%>%
  mutate(Result = ifelse(Error > 0, "Overestimate",ifelse(Error <0, "Underestimated","Correct")))
head(Error)

#Mosaic Plot
ErrorMosaic <- ggplot(data=Error) + 
  geom_mosaic(aes(x=product(Result,Test),fill=Result,na.rm=TRUE)) + coord_flip()+
  labs(title = 'Which Test Has More Error Estimates')
ErrorMosaic

#1.b
#Add absolute error
AbsoluteError <- Error %>%
  mutate(AbsoluteError = abs(Error))
head(AbsoluteError)

#Univariate Scatterplot
US <- ggplot(data=AbsoluteEorror, aes(x=factor(Test),y=AbsoluteError)) + geom_point()
US


#1.c
#Filter 56-73
New_PE <- Error %>%
  filter(Subject >= 56 & Subject <= 73)%>%
  group_by(Display)
head(New_PE)

#Mosaic Plot
ErrorMosaic_2 <- ggplot(data=New_PE) + 
  geom_mosaic(aes(x=product(Display,Test),fill=Result,na.rm=TRUE)) + coord_flip()+
  labs(title = 'Subjects 56-73 Comparison For Each Test')
ErrorMosaic_2

#1.d
Outlier <- ggplot(data=New_PE, aes(x=factor(Display),y=Error)) + geom_boxplot()
Outlier

#2.a
Messier <- read.csv('C:/Users/shuya/OneDrive/Documents/Depaul/DSC465/Week7/DATA VISUALIZATION - 2019-2020 Winter - 2222020 - 1256 AM/MessierData.csv')
head(Messier)

New_Messier <- Messier %>%
  mutate(logDistance = log10(Distance..LY.))
head(New_Messier)

PVM <- ggplot() + geom_line(data=New_Messier, aes(x=Messier.., y=Apparent.Magnitude), color = "skyblue2", size=1) + 
  geom_line(data=New_Messier, aes(x=Messier.., y=logDistance), color = "red", size=1) + 
  geom_line(data=New_Messier, aes(x=Messier.., y=Size.....), color = "darkgreen", size=1)+ ylab('Value') + 
  labs(title = 'Messier VS Properties')
PVM

#2.b
install.packages("ggbeeswarm")
library(ggbeeswarm)
DK <- New_Messier %>%
  ggplot(aes(x=factor(Kind),y=logDistance,na.rm=TRUE)) + geom_beeswarm() + labs(title = "Distance in Each Kind") +
  xlab('Kind')
DK

#2.c
AM <- New_Messier %>%
  ggplot(aes(x=Apparent.Magnitude, y=logDistance)) + coord_flip() + geom_point() + theme_minimal() + 
  labs(title='Apparent Magnitude VS logDistance')
AM

#2.d
Size <- New_Messier %>%
  ggplot(aes(x=Apparent.Magnitude, y=logDistance)) + coord_flip() + 
  geom_point(size = New_Messier$Size.....,col=alpha(colour = 'royalblue',alpha=0.3)) + 
  theme_minimal() + 
  labs(title='Apparent Magnitude VS logDistance',subtitle = 'Points size based on angular Size of the objects') 
Size


#4
library(tidyverse)
WindSolar <- read.csv('C:/Users/shuya/OneDrive/Documents/Depaul/DSC465/Week7/DATA VISUALIZATION - 2019-2020 Winter - 2222020 - 1256 AM/AirQuality.csv')

#4.a
install.packages('scales')
library(scales)

WindSolar_New <- WindSolar %>%
  mutate(Solar.R.Adj = rescale(Solar.R, to=c(0,21)))
WS <- ggplot(data = WindSolar_New, aes(x=Wind , y=Solar.R.Adj))
WS_Scatterployt <- WS + geom_point()+geom_abline(slope=-0.2,intercept=13) + labs(title = "Solar.R vs Wind")
WS_Scatterployt

#4.b
WindSolar_1 <- WindSolar_2 %>%
  filter(Measurement == "Wind" | Measurement == "Solar.R")
head(WindSolar_1)
WS_1 <- ggplot(data = WindSolar_1, aes(x=Measurement , y=value))
WS_Scatterployt_1 <- WS_1 + geom_beeswarm()+labs(title = "Distribution of Solar.R and Wind")
WS_Scatterployt_1

#4.c
library(reshape)
WindSolar_Copy <- as.data.frame(WindSolar)
head(WindSolar_Copy)
WindSolar_2 <- melt(WindSolar_Copy, id=c("Month","Day"))
names(WindSolar_2)[3] = "Measurement"
head(WindSolar_2)

WS_Plot <- WindSolar_2 %>%
  ggplot(aes(Measurement,value)) + geom_beeswarm()+ labs(title = "Distribution of Different Measur")
WS_Plot

#4.d
library(magrittr)
library(scales)
head(WindSolar)
WindSolar_qq <- WindSolar %$%
  data.frame(wind=sort(Wind),
             ozone=sort(Ozone),
             solar=sort(Solar.R),
             temp=sort(Temp))
WindSolar_qq %>% ggplot(aes(wind,solar)) + geom_point()
