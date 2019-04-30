library(shiny)
library(ggplot2)
library(shiny)
library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(readr)
library(ggpubr)
library(data.table)
library(ggthemes)


#load("../data(tidy)/GDP.RData")

load("../data(tidy)/USMap.RData")
load("../data(tidy)/Risk.RData")
load("../data(tidy)/Sbev.RData")
load("../data(tidy)/MCplot.RData")
load("../data(tidy)/EduP.RData")
load("../data(tidy)/GDP_plot.RData")

Dtime <- read.csv('../data(raw)/DiabetesTime.csv',skip = 2)

# Smk <- Ind_P[Ind_P$Type=="Smoking",]
Obs <- Ind_P[Ind_P$Type=="Obesity",]
#PH <- Ind_P[Ind_P$Type=="Poor_Health",]
Total <- Ind_P[Ind_P$Type=="Num",]
#PI <- Ind_P[Ind_P$Type=="Inactivity",]
Hyp <- Ind_P[Ind_P$Type=="Hypertension",]
HC <- Ind_P[Ind_P$Type=="HighCholesterol",]


color = list(color1 = c('#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494'),
             color2 = c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15'),
             color3 = c('#f1eef6', '#045a8d'),
             color4 = c( '#f2f0f7', '#cbc9e2', '#9e9ac8','#756bb1','#54278f'),
             color5 = c('#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
             color6 = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c"),
             color7 = c('#f7f7f7','#cccccc','#969696','#636363','#252525'))
label = list(label1 = c("<8%","8%-10%","10%~12%","12%~14%",">14%"),
             label2 = c("<75%","75%-80%","80%~85%","85%~90%",">90%"),
             label3 = c("65-74","75+"),
             label4 = c("<35%","35%-40%","40%~45%","45%~50%",">50%"),
             label5 = c("<50%","50%-55%","55%~60%","60%~65%",">65%"),
             label6 = c("<65%","65%-70%","70%~75%","75%~80%",">80%"),
             label7 = c("<15%","15%-20%","20%~25%","25%~30%",">30%"))
title = list(t1 = "Percentage", t2 = "Dominant Age",t3="Neighborhood Cluster")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  output$plot1<- renderPlot({
    ggplot(data = Dtime[1:23,], aes(x = as.character(Year), y = as.numeric(as.character(Percentage)),group=1))+
      geom_line(color = "blue")+
      ggtitle("Prevalence of Diagnosed Diabetes 1994-2016") +
      labs(x = "Year", y = "Percent")+theme_minimal()+theme(axis.text.x = element_text(angle = 90))
  })
  
  output$map1 <- renderLeaflet({
    
    Total_stat <- merge(us.map.state,Total, by="NAME")
    Ob_stat <- merge(us.map.state,Obs, by="NAME")
    #PI_stat <- merge(us.map.state,PI, by="NAME")
    #PH_stat <- merge(us.map.state,PH, by="NAME")
    HC_stat <- merge(us.map.state,HC, by="NAME")
    Hyp_stat <- merge(us.map.state,Hyp, by="NAME")
    #Smk_stat <- merge(us.map.state,Smk, by="NAME")
    Age_stat <- merge(us.map.state,AgeP,by="NAME")
    
    ####Total Num
    pal1 <- colorBin(color[[1]], bins=c(0,8,10,12,14,16))
    popup1 = paste0('<strong>State: </strong><br>', Total_stat$NAME,
                    '<br><strong>Number: </strong><br>', Total_stat$Number, 
                    '<br><strong>Percentage: </strong><br>', Total_stat$Percentage,"%")
    
    pic1 <- leaflet(Total_stat)  %>%
      addProviderTiles("CartoDB.Positron")
    
    
    ####Obesity
    pal2 <- colorBin(color[[2]], bins=c(0,75,80,85,90,100))
    popup2 = paste0('<strong>State: </strong><br>', Ob_stat$NAME,
                    '<br><strong>Number: </strong><br>', Ob_stat$Number, 
                    '<br><strong>Percentage: </strong><br>', Ob_stat$Percentage,"%")
    
    pic2 <- leaflet(Ob_stat)  %>%
      addProviderTiles("CartoDB.Positron")
    
    ####Age
    pal3 <- colorFactor(color[[3]], c(3,4))
    popup3 = paste0('<strong>State: </strong><br>', Age_stat$NAME,
                    '<br><strong>18-44: </strong>', Age_stat$`18-44`,"%",
                    '<br><strong>45-64: </strong>', Age_stat$`45-64`,"%",
                    '<br><strong>65-74: </strong>', Age_stat$`65-74`,"%",
                    '<br><strong>>75: </strong>', Age_stat$`75+`,"%")

    pic3 <- leaflet(Age_stat)  %>%
      addProviderTiles("CartoDB.Positron")
    
    ####Poor General Health
    # pal4 <- colorBin(color[[4]], bins=c(0,35,40,45,50,60))
    # popup4 = paste0('<strong>State: </strong><br>', PH_stat$NAME,
    #                 '<br><strong>Number: </strong><br>', PH_stat$Number, 
    #                 '<br><strong>Percentage: </strong><br>', PH_stat$Percentage,"%")
    # 
    # pic4 <- leaflet(PH_stat)  %>%
    #   addProviderTiles("CartoDB.Positron")
    
    ####High Cholesterol
    pal5 <- colorBin(color[[4]], bins=c(0,50,55,60,65,80))
    popup5 = paste0('<strong>State: </strong><br>', HC_stat$NAME,
                    '<br><strong>Number: </strong><br>', HC_stat$Number, 
                    '<br><strong>Percentage: </strong><br>', HC_stat$Percentage,"%")
    
    pic5 <- leaflet(HC_stat)  %>%
      addProviderTiles("CartoDB.Positron")
    
    ####Hypertension
    pal6 <- colorBin(color[[6]], bins=c(0,65,70,75,80,100))
    popup6 = paste0('<strong>State: </strong><br>', Hyp_stat$NAME,
                    '<br><strong>Number: </strong><br>', Hyp_stat$Number, 
                    '<br><strong>Percentage: </strong><br>', Hyp_stat$Percentage,"%")
    
    pic6 <- leaflet(Hyp_stat)  %>%
      addProviderTiles("CartoDB.Positron")
    
    ####Smoking
    # pal7 <- colorBin(color[[7]], bins=c(0,15,20,25,30,40))
    # popup7 = paste0('<strong>State: </strong><br>', Smk_stat$NAME,
    #                 '<br><strong>Number: </strong><br>', Smk_stat$Number, 
    #                 '<br><strong>Percentage: </strong><br>', Smk_stat$Percentage,"%")
    # 
    # pic7 <- leaflet(Smk_stat)  %>%
    #   addProviderTiles("CartoDB.Positron")
    
    ###########
    #Draw First Panel
    ##########
    
    if (input$CF == "count"){
      pic1<-pic1 %>% 
        addPolygons(color = "#444444",weight = 1,
                    popup = popup1, smoothFactor = 0.6,
                    opacity = 1.0, fillOpacity = 0.6,
                    fillColor = ~pal1(Percentage),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))%>%
        addLegend(position = "bottomright",
                  colors = color[[1]],
                  labels = label[[1]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    else if(input$CF == "OB"){
      pic2<-pic2 %>% 
        addPolygons(color = "#444444",weight = 1,
                    popup = popup2, smoothFactor = 0.6,
                    opacity = 1.0, fillOpacity = 0.6,
                    fillColor = ~pal2(Percentage),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))%>%
        addLegend(position = "bottomright",
                  colors = color[[2]],
                  labels = label[[2]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    else if(input$CF == "age"){
      pic3<-pic3 %>% 
        addPolygons(color = "#444444",weight = 1,
                    popup = popup3, smoothFactor = 0.6,
                    opacity = 1.0, fillOpacity = 0.6,
                    fillColor = ~pal3(Cat),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))%>%
        addLegend(position = "bottomright",
                  colors = color[[3]],
                  labels = label[[3]],
                  opacity = 0.6,
                  title = title[[2]])
    }
    
    # else if(input$CF == "PGH"){
    #   pic4<-pic4 %>% 
    #     addPolygons(color = "#444444",weight = 1,
    #                 popup = popup4, smoothFactor = 0.6,
    #                 opacity = 1.0, fillOpacity = 0.6,
    #                 fillColor = ~pal4(Percentage),
    #                 highlightOptions = highlightOptions(color = "white", weight = 2,
    #                                                     bringToFront = TRUE))%>%
    #     addLegend(position = "bottomright",
    #               colors = color[[4]],
    #               labels = label[[4]],
    #               opacity = 0.6,
    #               title = title[[1]])
    # }
    else if(input$CF == "HC"){
      pic5<-pic5 %>% 
        addPolygons(color = "#444444",weight = 1,
                    popup = popup5, smoothFactor = 0.6,
                    opacity = 1.0, fillOpacity = 0.6,
                    fillColor = ~pal5(Percentage),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))%>%
        addLegend(position = "bottomright",
                  colors = color[[4]],
                  labels = label[[5]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    else if(input$CF == "Hp"){
      pic6<-pic6 %>% 
        addPolygons(color = "#444444",weight = 1,
                    popup = popup6, smoothFactor = 0.6,
                    opacity = 1.0, fillOpacity = 0.6,
                    fillColor = ~pal6(Percentage),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))%>%
        addLegend(position = "bottomright",
                  colors = color[[6]],
                  labels = label[[6]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    # else if(input$CF == "Sm"){
    #   pic7<-pic7 %>% 
    #     addPolygons(color = "#444444",weight = 1,
    #                 popup = popup7, smoothFactor = 0.6,
    #                 opacity = 1.0, fillOpacity = 0.6,
    #                 fillColor = ~pal7(Percentage),
    #                 highlightOptions = highlightOptions(color = "white", weight = 2,
    #                                                     bringToFront = TRUE))%>%
    #     addLegend(position = "bottomright",
    #               colors = color[[7]],
    #               labels = label[[7]],
    #               opacity = 0.6,
    #               title = title[[1]])
    # }
  }
  )
  
  observeEvent(input$details,{
    if(input$details){
      updateTabsetPanel(session, "map1", selected = "panel3")
    }
  })
  
  output$plot2<- renderPlot({
    g1 <- ggplot()
    if(input$SB=="Show All"){
      temp <- temp
    }
    else if(input$SB=="None"){
      temp <- temp%>%filter(RiskFactor=="NoSweetBev"|RiskFactor=="NumberOfDiabetes")
      # g1 + geom_line(data=temp%>%filter(RiskFactor=="NoSweetBev"|RiskFactor=="NumberOfDiabetes"),
      #                aes(x = reorder(State,Rank), y = Percent,group=RiskFactor,col=RiskFactor),alpha = .7) +
      #   ggtitle("Diagnosed Diabetes vs. No Sweet Drink")+theme(axis.text.x = element_text(angle = 90))+xlab("States")
    }
    else if(input$SB=="Less than 1 time/Day"){
      temp<-temp%>%filter(RiskFactor=="less1perDay"|RiskFactor=="NumberOfDiabetes")
      # g1 + geom_line(data=temp%>%filter(RiskFactor=="less1perDay"|RiskFactor=="NumberOfDiabetes"),
      #                aes(x = reorder(State,Rank), y = Percent,group=RiskFactor,col=RiskFactor),alpha = .7) +
      #   ggtitle("Diagnosed Diabetes and Less than One Sweet Drink")+theme(axis.text.x = element_text(angle = 90))+xlab("States")
    }
    else if(input$SB=="More than 1 time/Day"){
      temp <- temp%>%filter(RiskFactor=="more1perDay"|RiskFactor=="NumberOfDiabetes")
      # g1 + geom_line(data=temp%>%filter(RiskFactor=="more1perDay"|RiskFactor=="NumberOfDiabetes"),
      #                aes(x = reorder(State,Rank), y = Percent,group=RiskFactor,col=RiskFactor),alpha = .7) +
      #   ggtitle("Diagnosed Diabetes and More than One Sweet Drink")+theme(axis.text.x = element_text(angle = 90))+xlab("States")
    }
    else if(input$SB=="Please Make a Selection"){
      temp <- temp%>%filter(RiskFactor=="NumberOfDiabetes")
    }
    g1 + geom_line(data=temp, aes(x = reorder(State,Rank), 
                                  y = Percent,group=RiskFactor,col=RiskFactor),alpha = .7) +
      ggtitle("Diagnosed Diabetes and Sweet Drink")+theme(axis.text.x = element_text(angle = 90))+xlab("States")
    
  })
  
  output$plot3<- renderPlot({
    g2 <- ggplot(MCplot)+
      geom_point(aes(x=reorder(State,Rank),y=McDonald.s.per.1M),stat="identity",col="purple",alpha = 0.5, size = 4)+
      coord_flip()+
      xlab("State")+
      ylab("Number of McDonald/million people")+
      ggtitle("Number of McDonald per Million People in each State")+
      theme_minimal()
    
    if(input$MC=="Show Trend"){
      g2+geom_smooth(aes(x=reorder(State,Rank),y=McDonald.s.per.1M,group=1))
    }
    else{
      g2
    }
  })
  
  output$plot4<- renderPlot({
    ggplot(df_edu)+geom_bar(aes(reorder(State,Rank),percent,fill=edu_level),stat = "identity",alpha=0.6)+
      xlab("State(ordered by Number of Diabetes)")+
      ylab("Percentage")+
      ggtitle("Education Level in each State")+
      coord_flip()+
      theme_classic2()
  })
  
  output$plot5 <- renderPlot({
    ggplot(df_gdp)+
      geom_point(aes(x=reorder(State,Rank),y=GDP2015,col=Diabetes_Rank))+
      coord_flip()+
      xlab("State(ordered by number of diabetes)")+
      ggtitle("GDP in each State")+
      theme_classic()
  })
  
 # output$map2 <- renderLeaflet({
 #   GDP_stat <- merge(us.map.state,GDP_P,by="NAME")
 #   ####GDP
 #   pal <- colorNumeric("Spectral", NULL, n = 13)
 #   popup = paste0('<strong>State: </strong><br>', GDP_stat$NAME,
 #                   '<br><strong>GDP: </strong><br>', GDP_stat$GDP2015)
 #   
 #   pic <- leaflet(GDP_stat)  %>%
 #     addProviderTiles("CartoDB.Positron")
 #   
 #   pic<-pic %>% 
 #     addPolygons(color = "#444444",weight = 1,
 #                 popup = popup, smoothFactor = 0.6,
 #                 opacity = 1.0, fillOpacity = 0.6,
 #                 fillColor = ~pal(GDP2015),
 #                 highlightOptions = highlightOptions(color = "white", weight = 2,
 #                                                     bringToFront = TRUE))
 #     # addLegend(position = "bottomright",
 #     #           colors = color[[1]],
 #     #           labels = label[[1]],
 #     #           opacity = 0.6,
 #     #           title = title[[1]])
 # })

  
})
