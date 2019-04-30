library(ggplot2)
library(leaflet)
library(shiny)
navbarPage("Diabetes in the US",id='map1',
           #################### 1st panel done ####################
           
           tabPanel("Introduction to Diabetes", value = "panel1",
                    h3(strong("Introduction and Background"),align = "center"),
                    br(),
                    p("As is well known, diabetes is a chronic (long-lasting) 
                      health condition that affects how your 
                      body turns food into energy. The time-series plot below shows that 
                      the prevalence of diagnosed diabetes increased from 4.4% in 1994 to 9.50% in 2016."), 
                    
                    br(),
                    column(12,
                           plotOutput('plot1'),
                           br()
                    ),
                    
                    
                    p("According to the ",a(href=
                                              "https://www.cdc.gov/diabetes/pdfs/data/statistics/national-diabetes-statistics-report.pdf", 
                                            "National Diabetes Statistics Report "),
                      "released today by the Centers for Disease Control and Prevention (CDC),
                      more than 100 million U.S. adults are now living with diabetes or prediabetes, which is nonnegligible.
                      But what are the main risk factors that cause this disease?"),
                    p("Select",tags$b("Main Risk Factors Overview"), "in the Navigator Bar on top of the page to see more details.")
                    
           ),
           
           tabPanel("Main Risk Factors Overview",value = "panel2",

                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        tags$style(".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                        leafletOutput("map1", width = "100%", height = "100%"),

                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = "auto", left = 0, right = 40, bottom = 30,
                                      width = 230, height = "auto",
                                      
                                      h6("Instruction:"),
                                      p("Select a State to see details!"),
                                      h6("Summary: "),
                                      p("Evidently, all of the main risk factors are closely related to eating habits."),
                                      p('Click the botton to see more.'),
                  
                                      actionButton("details", "Eating Habits")
                        
                         ),

                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                      width = 180, height = 280,
                                      h6("Total Number and Main Risk Factors for Diabetes."),
                                      radioButtons("CF", label = "Select one to see distribution among states!",
                                                   choices = list("Diagnosed Diabetes" = "count",
                                                                  "Obesity" = "OB",
                                                                  "High Cholesterol" = "HC",
                                                                  "Hypertension" = "Hp",
                                                                  "Age" = "age"),
                                                   selected = "OB")

                        )

                    )
           ),
           tabPanel("Eating Habits",value = 'panel3',
                    h3(strong("Diabetes vs. Easting Habits"),align = "center"),
                    selectInput("SB", label = h5("Sweet Beverage Frequency"), 
                                choices = list("Please Make a Selection","None","Less than 1 time/Day", "More than 1 time/Day","Show All"), 
                                selected = "Please Make a Selection"),
                    column(12,
                           plotOutput('plot2')
                    ),
                    selectInput("MC", label = h5("McDonald's per 1 Million People in each State"), 
                                choices = list("Show Trend","Not Show Trend"), selected = "Not Show Trend"),
                    column(12,
                           plotOutput('plot3')
                    )
                    
           ),
           tabPanel("Interesting Findings",value = 'panel4',
                    h3(strong("Diabetes vs. Education & GDP"),align = "center"),
                    br(),
                    br(),
                    column(6,
                           plotOutput('plot4'),
                           br()
                    ),
                    column(6,
                           plotOutput('plot5'),
                           br()
                    ),
                    br(),
                    br(),
                    p("By the plots above, it is clear 
                      that there are", strong("less"), "diagnosed diabetes among group of 
                      people with ",strong("higher")," education background and in states with" ,strong("higher"), "GDP."),
                    p("Combining with what we've learned from previous panels, ",strong("eating habits")," contribute a lot in causing diabetes."),
                    h5("Potential Reason: "),
                    p("People living in states with higher GDP or have well education background are more likely to possess
                      acute awareness of keeping fit, have a balanced diet and less likely to become overweighted."),
                    
                    h4("About:"),
                    p("Team Members: ","Xinyi Chen, ","Yifei Bi"),
                    p("Statistical Graphics, ","Columbia University"),
                    h4("Reference"),
                    a(href="https://www.bea.gov/", "Bureau of Economic Analysis"),
                    br(),
                    a(href="https://gis.cdc.gov","Centers for Disease Control and Prevention (CDC)")
                    
                    
                    
           )

)

