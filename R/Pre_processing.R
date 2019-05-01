####Scraping data from webpages
 
alcohol <- read_html("https://www.cdc.gov/alcohol/data-stats.htm") %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  data.frame()
colnames(alcohol) <- c("Location", "Total Cost($)","Cost Per Drink($)","Cost Per Capital($)")
alcohol$`Total Cost($)` <- as.numeric(gsub(",","",alcohol$`Total Cost($)`))
alcohol$`Cost Per Capital($)` <- as.numeric(gsub(",","",alcohol$`Cost Per Capital($)`))
# alcohol
# write.csv(alcohol, file = "alcohol.csv")
 

 
sugar2 <- read_html("https://www.cdc.gov/mmwr/volumes/65/wr/mm6507a1.htm#T2_down") %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  .[2] %>%
  data.frame()

colnames(sugar2) <- sugar2[2,]
sugar2 <- sugar2[c(-1,-2,-3),]
sugar2$`18–24` <- as.numeric(sapply(unlist(sugar2$`18–24`),substr,1,4))
sugar2$`25–34` <- as.numeric(sapply(unlist(sugar2$`25–34`),substr,1,4))
sugar2$`35–54` <- as.numeric(sapply(unlist(sugar2$`35–54`),substr,1,4))
sugar2$`≥55` <- as.numeric(sapply(unlist(sugar2$`≥55`),substr,1,4))
sugar2$Male <- as.numeric(sapply(unlist(sugar2$Male),substr,1,4))
sugar2$Female <- as.numeric(sapply(unlist(sugar2$Female),substr,1,4))
sugar2$`White, Non-Hispanic` <- as.numeric(sapply(unlist(sugar2$`White, Non-Hispanic`),substr,1,4))
sugar2$`Black, Non-Hispanic` <- as.numeric(sapply(unlist(sugar2$`Black, Non-Hispanic`),substr,1,4))
sugar2$Hispanic <- as.numeric(sapply(unlist(sugar2$Hispanic),substr,1,4))
sugar2$`Other, Non-Hispanic` <- as.numeric(sapply(unlist(sugar2$`Other, Non-Hispanic`),substr,1,4))
sugar2$`No. respondents` <- as.numeric(gsub(",","",sugar2$`No. respondents`))
# sugar2
# write.csv(sugar2, file = "sugar2.csv")
 

 
sugar3 <- read_html("https://www.cdc.gov/mmwr/volumes/65/wr/mm6507a1.htm#T3_down") %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  .[3] %>%
  data.frame()

colnames(sugar3) <- sugar3[2,]
sugar3 <- sugar3[c(-1,-2,-3),]
sugar3$Employed <- as.numeric(sapply(unlist(sugar3$Employed),substr,1,4))
sugar3$`Not employed` <- as.numeric(sapply(unlist(sugar3$`Not employed`),substr,1,4))
sugar3$Retired <- as.numeric(sapply(unlist(sugar3$`Not employed`),substr,1,4))
sugar3$`<High school` <- as.numeric(sapply(unlist(sugar3$`<High school`),substr,1,4))
sugar3$`High school` <- as.numeric(sapply(unlist(sugar3$`High school`),substr,1,4))
sugar3$`Some college` <- as.numeric(sapply(unlist(sugar3$`Some college`),substr,1,4))
sugar3$`College graduate` <- as.numeric(sapply(unlist(sugar3$`College graduate`),substr,1,4))
sugar3$`No. respondents` <- as.numeric(gsub(",","",sugar3$`No. respondents`))
# sugar3
# write.csv(sugar3, file = "sugar3.csv")
 


####Read in Raw Data
 
Sn <- read.csv("../data(raw)/Sn.csv",skip = 2, na.strings = "No Data")
Sp <- read.csv("../data(raw)/Sp.csv",skip = 2, na.strings = "No Data")
On <- read.csv("../data(raw)/On.csv",skip = 2, na.strings = "No Data")
Op <- read.csv("../data(raw)/Op.csv",skip = 2, na.strings = "No Data")
PGn <- read.csv("../data(raw)/PGn.csv",skip = 2, na.strings = "No Data")
PGp <- read.csv("../data(raw)/PGp.csv",skip = 2, na.strings = "No Data")
Dn <- read.csv("../data(raw)/Dn.csv",skip = 2, na.strings = "No Data")
Dp <- read.csv("../data(raw)/Dp.csv",skip = 2, na.strings = "No Data")
Phn <- read.csv("../data(raw)/Phn.csv",skip = 2, na.strings = "No Data")
Php <- read.csv("../data(raw)/Php.csv",skip = 2, na.strings = "No Data")
Hypern <- read.csv("../data(raw)/Hypern.csv",skip = 2, na.strings = "No Data")
Hyperp <- read.csv("../data(raw)/Hyperp.csv",skip = 2, na.strings = "No Data")
HCn <- read.csv("../data(raw)/HCn.csv",skip = 2, na.strings = "No Data")
HCp <- read.csv("../data(raw)/HCp.csv",skip = 2, na.strings = "No Data")
alcohol <- read.csv("../data(raw)/alcohol.csv")
sugar1 <- read.csv("../data(raw)/sugar2.csv")
sugar2 <- read.csv("../data(raw)/sugar3.csv")
sugar3 <- read.csv("../data(raw)/sugar1.csv")
underH <- read.csv("../data(raw)/<H.csv",skip = 2, na.strings = "No Data")
H <- read.csv("../data(raw)/H.csv",skip = 2, na.strings = "No Data")
postH <- read.csv("../data(raw)/>H.csv",skip = 2, na.strings = "No Data")
age1 <- read.csv("../data(raw)/18-44.csv",skip = 2, na.strings = "No Data")
age2 <- read.csv("../data(raw)/45-64.csv",skip = 2, na.strings = "No Data")
age3 <- read.csv("../data(raw)/65-74.csv",skip = 2, na.strings = "No Data")
age4 <- read.csv("../data(raw)/75+.csv",skip = 2, na.strings = "No Data")
GDP2015 <- read.csv("../data(raw)/2015.csv",skip = 4)
GDP2016 <- read.csv("../data(raw)/2016.csv", skip = 4)
mcdonald <- read.xls("../data(raw)/mcdonald.xlsx")
 


####Basic Data Cleaning

 
clean <- function(df1,df2){
  df1 <- df1[-nrow(df1),]
  df2 <- df2[-nrow(df2),]
  overall <- full_join(df1,df2, by="State")
  return(overall)
}
clean2 <- function(df1){
  df1 <- df1[-nrow(df1),] %>% select(1:2)
  return(df1)
}
 

*DataFrame1--Minor Risk Factors*
   
Smoking <- clean(Sn,Sp)
Smoking$Type <- rep("Smoking",nrow(Smoking))

Obesity <- clean(Op, On)
Obesity$Type <- rep("Obesity",nrow(Obesity))


Poor_Health <- clean(PGn, PGp)
Poor_Health$Type <- rep("Poor_Health",nrow(Poor_Health))

Num <- clean(Dn,Dp)
Num$Type <- rep("Num",nrow(Num))

Inactivity <- clean(Phn,Php)
Inactivity$Type <- rep("Inactivity",nrow(Inactivity))

Hypertension <- clean(Hyperp,Hypern)
Hypertension$Type <- rep("Hypertension",nrow(Hypertension))

Cholesterol <- clean(HCn, HCp)
Cholesterol$Type <- rep("HighCholesterol",nrow(Cholesterol))

Indicators <- rbind(Smoking,Obesity,Poor_Health,Num, Inactivity,Hypertension, Cholesterol)
Indicators$Type <- as.factor(Indicators$Type)
Indicators <- Indicators[-nrow(Indicators),]
#Indicators
 

*DataFrame2--LifeStyle*
   
alcohol <- alcohol %>% 
  select(2:5) %>% 
  mutate(Num.Drink.per.Capita = ceiling(Cost.Per.Capital.../Cost.Per.Drink...))
names(alcohol)[1] <- "State"
#alcohol

names(sugar3) <- c("State","ResponseNum","NoSweetBev","less1perDay","more1perDay")
SweetBev <- sugar3 %>% select(1,3,4,5)
#SweetBev

mcdonald <- mcdonald %>% select(State, Rank,McDonald.s.per.100.000)
mcdonald$McDonald.s.per.100.000 <- as.numeric(gsub("[[:punct:]]","",as.character(mcdonald$McDonald.s.per.100.000)))
names(mcdonald)[3] <- "McDonald.s.per.1M" 
#mcdonald

EatHab <- merge(merge(SweetBev,alcohol%>%select(1,5),by="State"),mcdonald%>%select(1,3),by="State")
#EatHab
 

*Major Influencial Factors*
  
   
underH <- clean2(underH)
names(underH) <- c("State","UnderHighSchool")
H <- clean2(H)
names(H) <- c("State","HighSchool")
postH <- clean2(postH)
names(postH) <- c("State","PostHighSchool")
education <- full_join(full_join(underH,H,by="State"),postH,by="State")

#education


age1 <- clean2(age1)
age2 <- clean2(age2)
age3 <- clean2(age3)
age4 <- clean2(age4)
names(age1) <- c("State","18-44")
names(age2) <- c("State","45-64")
names(age3) <- c("State","65-74")
names(age4) <- c("State","75+")
Age <- full_join(full_join(full_join(age1,age2,by="State"),age3, by="State"),age4,by="State")

Major <- full_join(education,Age,by="State")
#Major
 

*Other Factors: GDP*
   
GDP2015 <- GDP2015[2:(nrow(GDP2015)-11),]
GDP2016 <- GDP2016[2:(nrow(GDP2016)-11),]
names(GDP2015)[2:3] <- c("State","GDP2015")
GDP <- GDP2015 %>% select(2,3)
#GDP
 