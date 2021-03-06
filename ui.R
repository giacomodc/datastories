library(shiny)
library(ggplot2)
library(leaflet)
library(raster)
library(dygraphs)
library(plyr)
library(xts)
library(zoo)
library(stringr)
library(lubridate)
library(markdown)
library(DT)
library(readr)
library(shinydashboard)
library(shinythemes)

vardescr <- read.table("data/vardescr.txt", sep="\t", stringsAsFactors = F)
tamp_retailers <- read.csv("data/tamp_retailers.csv", stringsAsFactors = F, header=T)
tampString <- read_file('text/tamp_overview.txt')
northpString <- read_file('text/np_overview.txt')
acknowString <- read_file('text/acknow.txt')
welcomeString <- read_file('text/welcome.txt')
dataDescString <- read_file('text/dataDesc.txt')
var1String <- read_file('text/var1Desc.txt')
var2String <- read_file('text/var2Desc.txt')
var3String <- read_file('text/var3Desc.txt')
var4String <- read_file('text/var4Desc.txt')


shinyUI(

navbarPage(title="SGmalls",theme = shinytheme('cerulean'), 

# HOME
tabPanel("Home", icon=icon('home'), column(width=10, offset=1, tabsetPanel(
                   tabPanel('Welcome',
                            br(),
                            HTML(markdownToHTML(fragment.only=TRUE, text=welcomeString))),
                   tabPanel('Large Urban Traffic Generators',
                            fluidRow(
                              br(),
                              column(3,
                                     box(width=NULL,
                                         selectInput("malls_map_visual", "Choose visualization:", 
                                                     choices=c("Location"="loc_only",
                                                               "Number of stores"="no_stores",
                                                               "Stores per squared m."="store_density")
                                         )
                                     ), #END of wellPanel
                                     fluidRow(valueBoxOutput("mallsBox",width=12)),
                                     fluidRow(valueBoxOutput("tripsBox",width=12))
                              ),
                              column(9,leafletOutput("map_malls"))
                            )),
                   tabPanel('Authors',
                            h3("Sustainable Urban Mobility Research Lab"),
                            h5(a("http://mobility.sutd.edu.sg/",href='http://mobility.sutd.edu.sg/')),
                            tags$ul(  
                              tags$li("Lynette Cheah, Professor (PI)."),
                              tags$li("Ngai-Man Cheung, Professor."),
                              tags$li("Costas Courcoubetis, Professor."),
                              tags$li(a("Giacomo Dalla Chiara, Ph.D. candidate.", href="http://esd.sutd.edu.sg/phd-students/giacomo-dalla-chiara/")),
                              tags$li("Guo Ziqi, Undergraduate Research Assistant."),
                              tags$li("Ding Jiatao, Undergraduate Research Assistant."),
                              tags$li("Sun Xin, Postdoctoral Researcher.")
                            )),
                   tabPanel('Acknowledgement',
                            br(),
                            HTML(markdownToHTML(fragment.only=TRUE, text=acknowString)))
                 ))),

# CASE STUDIES
tabPanel("Case Studies", icon=icon('th'), column(width=10, offset=1,
                 tabsetPanel(
                   tabPanel('Mall 1',
                            fluidRow(
                              column(width=12,
                                     br(),
                                     wellPanel(
                              tabsetPanel(
                                      
                                     tabPanel('Overview',HTML(markdownToHTML(fragment.only=TRUE, text=northpString))),
                                     tabPanel('Map',imageOutput('northp_map')),
                                     tabPanel('Vehicle Count',
                                              fluidRow(
                                                br(),
                                                column(3,
                                                       wellPanel(
                                                         selectInput("northp_vehicle_split", "Split by:", 
                                                                     choices=c("Interviewed vs. non-interviewed"="employer", 
                                                                               "Vehicle type"="vehicle_type", 
                                                                               "Refrigerated vs. non-rerigerated"="refrigerated",
                                                                               "Parking location"="park_location",
                                                                               "Number of workers"="no_workers")),
                                                         radioButtons("northp_vehicle_visual", "Choose type of visualization:", 
                                                                      choices = c("Table","Percentage", "Barplot"))
                                                         
                                                       )    
                                                ),
                                                column(8, offset=1,
                                                       conditionalPanel(
                                                         condition="input.northp_vehicle_visual!='Barplot'",
                                                         DT::dataTableOutput("northp_vehicle_table")
                                                       ),
                                                       conditionalPanel(
                                                         condition="input.northp_vehicle_visual=='Barplot'",
                                                         plotOutput('northp_vehicle_barplot')
                                                       )
                                                       
                                                )
                                              )
                                     ),
                                     tabPanel('Delivery Count',
                                              fluidRow(
                                                br(),
                                                column(3,
                                                       wellPanel(
                                                         selectInput("northp_delivery_split", "Split by:", 
                                                                     choices=c("Deliveries vs. pick-ups"="del_pick", 
                                                                               "Size of delivery"="del_size", 
                                                                               "Commodity type"="commodity_type"
                                                                     )),
                                                         radioButtons("northp_delivery_visual", "Choose type of visualization:", 
                                                                      choices = c("Table","Percentage", "Barplot"))
                                                         
                                                       )    
                                                ),
                                                column(8, offset=1,
                                                       conditionalPanel(
                                                         condition="input.northp_delivery_visual!='Barplot'",
                                                         DT::dataTableOutput("northp_delivery_table")
                                                       ),
                                                       conditionalPanel(
                                                         condition="input.northp_delivery_visual=='Barplot'",
                                                         plotOutput('northp_delivery_barplot')
                                                       )
                                                )
                                              ))
                              )))
                            )),
                   tabPanel('Mall 2',
                            fluidRow(
                              column(width=12,
                                     br(),
                                     wellPanel(
                              tabsetPanel(
                                     tabPanel('Overview',HTML(markdownToHTML(fragment.only=TRUE, text=tampString))),
                                     tabPanel('Map',imageOutput('tamp_map')),
                                     tabPanel('Vehicle Count',
                                              fluidRow(
                                                br(),
                                                column(3,
                                                       wellPanel(
                                                         selectInput("tamp_vehicle_split", "Split by:", 
                                                                     choices=c("Interviewed vs. non-interviewed"="employer", 
                                                                               "Vehicle type"="vehicle_type", 
                                                                               "Refrigerated vs. non-rerigerated"="refrigerated",
                                                                               "Parking location"="park_location",
                                                                               "Number of workers"="no_workers")),
                                                         radioButtons("tamp_vehicle_visual", "Choose type of visualization:", 
                                                                      choices = c("Table","Percentage", "Barplot"))
                                                         
                                                       )    
                                                ),
                                                column(8, offset=1,
                                                       conditionalPanel(
                                                         condition="input.tamp_vehicle_visual!='Barplot'",
                                                         DT::dataTableOutput("tamp_vehicle_table")
                                                       ),
                                                       conditionalPanel(
                                                         condition="input.tamp_vehicle_visual=='Barplot'",
                                                         plotOutput('tamp_vehicle_barplot')
                                                       )
                                                       
                                                )
                                              )
                                     ),
                                     tabPanel('Delivery Count',
                                              fluidRow(
                                                br(),
                                                column(3,
                                                       wellPanel(
                                                         selectInput("tamp_delivery_split", "Split by:", 
                                                                     choices=c("Deliveries vs. pick-ups"="del_pick", 
                                                                               "Size of delivery"="del_size", 
                                                                               "Commodity type"="commodity_type"
                                                                     )),
                                                         radioButtons("tamp_delivery_visual", "Choose type of visualization:", 
                                                                      choices = c("Table","Percentage", "Barplot"))
                                                         
                                                       )    
                                                ),
                                                column(8, offset=1,
                                                       conditionalPanel(
                                                         condition="input.tamp_delivery_visual!='Barplot'",
                                                         DT::dataTableOutput("tamp_delivery_table")
                                                       ),
                                                       conditionalPanel(
                                                         condition="input.tamp_delivery_visual=='Barplot'",
                                                         plotOutput('tamp_delivery_barplot')
                                                       )
                                                )
                                              )))
                              )
                            )))
                            )
                 )),

# DATA DESCRIPTION
tabPanel("Data Description", icon=icon('file-text-o'), column(width=10, offset=1,
                 HTML(markdownToHTML(fragment.only=TRUE, text=dataDescString)),
                 br(),
                 tabsetPanel(
                             tabPanel("Delivery variables",
                                      br(),
                                      HTML(markdownToHTML(fragment.only=TRUE, text=var1String)),
                                      wellPanel(
                                        fluidRow(
                                          column(3,br(),strong("Variable name:")),
                                          column(9,selectInput(
                                            "delivery_var", " ",choices = vardescr[vardescr[,1]=="delivery",2]))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Observations:")),
                                          column(9,textOutput("nrows_delivery_var"))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Description:")),
                                          column(9,textOutput("descr_delivery_var"))
                                        )
                                      ) #END of wellpanel
                             ), #END of delivery tabpanel
                             
                             tabPanel("Vehicle variables",
                                      br(),
                                      HTML(markdownToHTML(fragment.only=TRUE, text=var2String)),
                                      wellPanel(
                                        fluidRow(
                                          column(3,br(),strong("Variable name:")),
                                          column(9,selectInput(
                                            "vehicle_var", " ", choices = vardescr[vardescr[,1]=="vehicle",2]))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Observations:")),
                                          column(9,textOutput("nrows_vehicle_var"))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Description:")),
                                          column(9,textOutput("descr_vehicle_var"))
                                        )
                                      ) #END of wellpanel
                             ), #END of vehicle tabpanel
                             
                             tabPanel("Driver variables",
                                      br(),
                                      HTML(markdownToHTML(fragment.only=TRUE, text=var3String)),
                                      wellPanel(
                                        fluidRow(
                                          column(3,br(),strong("Variable name:")),
                                          column(9,selectInput(
                                            "driver_var", " ", choices = vardescr[vardescr[,1]=="driver",2]))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Observations:")),
                                          column(9,textOutput("nrows_driver_var"))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Description:")),
                                          column(9,textOutput("descr_driver_var"))
                                        )
                                      ) # END of wellpanel     
                             ),#END of drivers tabpanel
                             
                             tabPanel("Traffic variables",
                                      br(),
                                      HTML(markdownToHTML(fragment.only=TRUE, text=var4String)),
                                      wellPanel(
                                        fluidRow(
                                          column(3,br(),strong("Variable name:")),
                                          column(9,selectInput(
                                            "traffic_var", " ",choices = vardescr[vardescr[,1]=="traffic",2]))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Observations:")),
                                          column(9,textOutput("nrows_traffic_var"))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Description:")),
                                          column(9,textOutput("descr_traffic_var"))
                                        )
                                      ) # END of wellpanel
                             ),#END of traffic tabpanel 
                             
                             tabPanel("All variables", ###
                                      br(),
                                      wellPanel(
                                        fluidRow(
                                          column(3,br(),strong("Variable name:")),
                                          column(9,selectInput(
                                            "all_var", " ", choices = vardescr[,2]))
                                        ),
                                        fluidRow(
                                          column(3,strong("Observations:")),
                                          column(9,textOutput("nrows_all_var"))
                                        ),
                                        br(),
                                        fluidRow(
                                          column(3,strong("Description:")),
                                          column(9,textOutput("descr_all_var"))
                                        )
                                      ) #END of wellpanel
                             ))#END of "all variables" tabpanel
                 )),

# DATA ANALYSIS
tabPanel("Data Analysis", icon=icon('bar-chart'),
                 sidebarLayout(
                   sidebarPanel(width=3,
                     selectInput("mall_filter", "Mall Selection:", 
                                 choices=c("Mall 1","Mall 2","Both malls"),
                                 selected='Mall 1'),
                     conditionalPanel(condition="input.mall_filter=='Mall 1'",
                                      checkboxGroupInput("date_filter_1", "Date Selection:", 
                                                         choices = c("24 June 2015 (Wed)"="2015-06-24", 
                                                                     "25 June 2015 (Thu)"="2015-06-25", 
                                                                     "26 June 2015 (Fri)"="2015-06-26"),
                                                         selected = c('2015-06-24','2015-06-25','2015-06-26'))),
                     conditionalPanel(condition="input.mall_filter=='Mall 2'",
                                      checkboxGroupInput("date_filter_2", "Date Selection:", 
                                                         choices = c("21 Jan 2016 (Thu)"="2016-01-21", 
                                                                     "22 Jan 2015 (Fri)"="2016-01-22"),
                                                         selected = c('2016-01-21','2016-01-22'))),
                     conditionalPanel(condition="input.mall_filter=='Both malls'",
                                      checkboxGroupInput("date_filter_12", "Date Selection:", 
                                                         choices = c("24 June 2015 (Wed)"="2015-06-24", 
                                                                     "25 June 2015 (Thu)"="2015-06-25", 
                                                                     "26 June 2015 (Fri)"="2015-06-26",
                                                                     "21 Jan 2016 (Thu)"="2016-01-21", 
                                                                     "22 Jan 2015 (Fri)"="2016-01-22"),
                                                         selected = c('2015-06-24','2015-06-25','2015-06-26','2016-01-21','2016-01-22')))
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel('System Congestion',
                                br(),
                                column(width=12,
                         fluidRow(
                           wellPanel(status='warning',
                               checkboxGroupInput("park_location", "Show vehicles that parked...", 
                                                  choices = c("...inside the loading bay"="LB",
                                                              "...inside the passenger carpark"="car_park", 
                                                              "...on the street"="street"),
                                                  selected = c('street','LB','car_park'),inline=TRUE)
                           )
                         ),
                         fluidRow(
                           wellPanel(solidHeader=T,
                               # textOutput('step_plot_text')
                               dygraphOutput("step_plot")
                           )
                         ),
                         fluidRow(
                           column(width=12,'Note: The red dash represents the size of loading bay of Mall 1 (2015), and the green dash represents the size of loading bay of Mall 2 (2016).')
                         ))),
                 tabPanel('Arrival',
                          br(),
                          column(width=12,
                         fluidRow(
                           wellPanel(status='warning',
                               sliderInput("input_interval", label="Choose time interval size:", value=50, min=10, max=90, step=10)
                           )
                         ),
                         fluidRow(
                           tabsetPanel(
                                  tabPanel('Arrivals Distribution',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    dygraphOutput("arrivals_plot")
                                             )
                                           )),
                                  tabPanel('Cumulative Arrivals',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    dygraphOutput("arrivals_plot_cum"))
                                           )
                                  ))))),
                 tabPanel('Handling',
                          br(),
                          column(width=12,
                         fluidRow(
                           wellPanel(status='warning',
                               selectInput("which_visualization_handling", "Select the visualization:", 
                                           choices= c("Handling times distribution"="htime_distribution", 
                                                      "Handling times vs. size of delivery/pick-up"="htime_delsize",
                                                      "Handling time by arrival time"='htime_arrival'), 
                                           selected="htime_distribution"),
                               checkboxInput("byparkloc", "Differentiate by park location"))
                         ),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.byparkloc",
                             wellPanel(status='warning',
                                 checkboxGroupInput("park", "Park location:", 
                                                    c("Street"="street", "Loading bay"="LB", "Carpark"="car_park"),
                                                    selected = c('street','LB','car_park'),inline=TRUE)
                             )
                           )
                         ),
                         fluidRow(
                           #                   conditionalPanel(
                           #                     condition="input.which_visualization_handling=='htime_distribution'",                     
                           #                     textOutput("nrowsfinal"),
                           tabsetPanel(
                                  tabPanel('Histogram',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    plotOutput("hist_htime")
                                             )
                                           )),
                                  tabPanel('Cumulative Distribution',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    plotOutput("cum_htime")
                                             )
                                           )
                                  )))
                           # )
                           # ,
                           #                   conditionalPanel(
                           #                     condition="input.which_visualization_handling=='htime_delsize'",
                           #                     #plotOutput("delsize_htime")
                           #                     "coming soon!"
                           #                   ),
                           #                   conditionalPanel(
                           #                     condition="input.which_visualization_handling=='htime_arrival'",
                           #                     #plotOutput("delsize_htime")
                           #                     "coming soon!"
                           #                   )
                         )),
                 tabPanel('Queueing',
                          br(),
                          column(width=12,
                         fluidRow(
                           #               box(width=12,status='warning',
                           #                      checkboxInput("onlyLB_queue", "Include only vehicles that parked in the loading bay.")
                           #               ),
                           tabsetPanel(
                                  tabPanel('Histogram',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    plotOutput("hist_qtime")
                                             )
                                           )),
                                  tabPanel('Cumulative Distribution',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    plotOutput("cum_qtime")
                                             )
                                           ))
                                  ))
                         )),
                 tabPanel('Dwell',
                          br(),
                          column(width=12,
                         fluidRow(
                           tabsetPanel(
                                  tabPanel('Histogram',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    plotOutput("hist_dtime")
                                             )
                                           )),
                                  tabPanel('Cumulative Distribution',
                                           fluidRow(
                                             column(12,solidHeader=T,
                                                    plotOutput("cum_dtime")
                                             )
                                           )
                                  )))
                         ))))
                 )),
tabPanel("Simulation", icon=icon('play-circle'))

) #END navbarPage

) #END shinyUI

