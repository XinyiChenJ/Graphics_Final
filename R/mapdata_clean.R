
library(rgdal)

load("../data(tidy)/Indicators.RData")
load("../data(tidy)/EatHab.RData")
load("../data(tidy)/Major.RData")
load("../data(tidy)/GDP.RData")


us.map.state <- readOGR(dsn= '../data(raw)/cb_2016_us_state_20m/cb_2016_us_state_20m.shp', 
                            layer = "cb_2016_us_state_20m", 
                            stringsAsFactors = FALSE)
us.map.state <- subset(us.map.state, us.map.state$NAME %in% Major$State[c(1:10,12:50)])
#save(us.map.state,file = "../data(tidy)/USMap.RData")




AgeP <- Age[Age$State %in% Major$State[c(1:10,12:50)],]
EduP <- education[education$State %in% Major$State[c(1:10,12:50)],]
GDP_P <- GDP[GDP$State %in% Major$State[c(1:10,12:50)],]
Ind_P <- Indicators[Indicators$State %in% Major$State[c(1:10,12:50)],]
names(AgeP)[1] <- "NAME"
names(EduP)[1] <- "NAME"
names(GDP_P)[1] <- "NAME"
names(Ind_P)[1] <- "NAME"
#save(AgeP,EduP,GDP_P,Ind_P,file = "../data(tidy)/Risk.RData")

Dtime <- read_csv('../data(raw)/DiabetesTime.csv',skip = 2)
#Dtime
ggplot(data = Dtime[1:23,], aes(x = as.character(Year), y = as.numeric(Percentage),group=1))+
  geom_line(color = "blue")+
  ggtitle("US Diabetes Percentage") +
  labs(x = "year", y = "percent")+theme_minimal()+theme(axis.text.x = element_text(angle = 90))
AgeP$Cat <- apply(AgeP[,2:5], 1, which.max)

#save(AgeP,EduP,GDP_P,Ind_P,file = "../data(tidy)/Risk.RData")


