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
library(shinydashboard)
library(readr)


vardescr <- read.table("data/vardescr.txt", sep="\t", stringsAsFactors = F)
tamp_retailers <- read.csv("data/tamp_retailers.csv", stringsAsFactors = F, header=T)
tampString <- read_file('text/tamp_overview.txt')
npString <- read_file('text/np_overview.txt')

header <- dashboardHeader(title = 'Data Stories from Loading Bays',
                          titleWidth = 320)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Home',tabName='home',icon=icon('dashboard'), 
             menuSubItem('Welcome',tabName='welcome'),
             menuSubItem('Data Description',tabName='dataDescr'),
             menuSubItem('Large Urban Traffic Generators',tabName='traffGen'),
             menuSubItem('Authors',tabName='authors')),
    menuItem('Case Studies Summaries',tabName='summaries',icon=icon('th'),
             menuSubItem('Mall 1',tabName='mall1'),
             menuSubItem('Mall 2',tabName='mall2')),
    menuItem('Visuals',tabName='visuals',icon=icon('bar-chart'),
             menuSubItem('System Performance',tabName='sysPerf'),
             menuSubItem('Arrivals',tabName='arrivals'),
             menuSubItem('Handling',tabName='handling'),
             menuSubItem('Queueing',tabName='queueing'),
             menuSubItem('Dwelling',tabName='dwelling'))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName='welcome',
            p("This website is the result of the IDA - SUTD collaboration in an effort to understand freight-generated traffic in metropolitan areas and 
                                                        explore the concepts of", 
              strong("in-mall goods distribution system"), 
              "and", 
              strong("Urban Consolidation Centre"),
              "in Singapore."),
            p("The concept of in-mall goods distribution system consists in the creation of a central goods receiving station in a retail mall, 
              receiving and consolidating goods deliveries to retailers within the mall. In the absence of a centralised receiving station, 
              each carrier have to independently walk to the store with the deliveries. An in-mall distribution system is expected to reduce handling 
              time and congestion at loading bays. An Urban Consolidation Centre (UCC) have the same function of a central receiving station as in 
              the in-mall distribution, with the difference that the central receiving station is placed in a different location than the one of the 
              mall, hence further reducing the number of arrival trucks at the mall, tackling congestion at the mall and in its premises."),
            
            p("The aim of this interface is to provide:"),
            
            tags$ul(
              tags$li("Meaningful data to support the planning of urban freight systems."),
              tags$li("Better understanding of In-Mall distribution."),
              tags$li("Insights on the current challenges for an effective In-Mall distribution system.")
            ),
            
            p("The project is currently on data analysis phase. System modelling and design recommendations will be included in later stages."),
            h3("Objectives"),
            p("The primary question we would like to answer is: what is the current state of the system? Where by 'system' we mean
              the bundle of processes involved before, during and after a goods vehicle has delivered, picked-up goods or performed some
              form of service at a large retail estalbishment. Such processes usually start with the cruising for parking and parking of the vehicle,
              then followed by the 'handling operations' (loading, unloading of goods deliverying inside the mall, picking-up goods or performing a service).
              Such processes are fundamental for the realization of the logistics process, of the very last meters of a delivery. These very last meters
              are often the most 'inefficient' distance covered by the logistics process, where several inefficiencies arise due to congestion of
              freight vehicles, long handling times, lack of resources and space for performing loading and unloading, as well as generating
              several externalities to the neighbourhood and among tranport operators."),
            h3("Phases"),
            p(strong("1. Data collections:"),"Collect data at large retail establishments and commercial urban areas."),
            p(strong("2. Data analysis:"), "Identify and quantify potential externalities and economic inefficiencies caused by freight traffic at large 
              retail establishments."),
            p(strong("3. Modeling:"), "Model the processes involved in the parking and handling operations at large etablishments. Identify suitable 
              urban logistics initiatives which can possibly tackle the externalities identified and simulate their effects using the identified 
              methodology.")
            ),
    tabItem(tabName='dataDescr',
            h3("Data sources"),
            p("In the data collected a single observation is a 'truck-shipment' entering the premises of a shopping mall. 
              A 'truck-shipment' unit consists in a goods vehicle carrying one driver and sometimes one or more helpers, delivering 
              one or more shipments to its customers (i.e. store(s) located inside a shopping mall)."),
            p("Each observational unit, uniquely identified by the licence plate, is followed from the time it enters the system, i.e. the
              premises of a mall, to the time it leaves the system. In between, we have tracked all possible observable processes
              performed by the goods vehicle and its driver and helpers, in driving, parking, performing the delivery or some other form of services 
              for the customers."),
            p("For each observed unit we measured several variables, which we distinguish into four types: delivery, vehicle, driver and traffic variables.",
              strong("Delivery variables"), "describe a single shipment of a truck-shipment unit, such as size, types of goods delivered and store the goods are delivered to.", 
              strong("Vehicle variables"), "describe the goods vehicle used, such as car maker, vehicle size and loading factor.", 
              strong("Driver variables"), "are collected directly via drivers surveys and describe the drivers' routing choices and parking behaviours. 
              Finally,",
              strong("traffic variables"), "measure the freight traffic level at the premises of the establishment."),
            p("The aim of the current research effort is to integrate the above mentioned sources of data regarding all the goods vehicles 
              arriving at the selected shopping malls, in order to assess the current state of the system, quantify potential expernalities 
              generated and understand how logistics initiatives can impact the system."),
            br(),
            
            tabBox(width=NULL,
              tabPanel("Delivery variables", ###
                       br(),
                       p("Truck drivers and helpers, after having parked the goods vehicle, start a", 
                         strong("handling process"),
                         ". Handling consists in performing one or more",
                         strong("deliveries"), 
                         ". Each delivery consists in carrying a certain amount of goods from the location where the vehicle parked 
                         to the receiver or receivers' locations. We have collected several variables describing single deliveries,
                         some have been collected by observing the handling process, others have been obtained interviewing the driver."),
                       p("Note: a goods vehicle that parked in the premises of an establishment does not necessarily perform a delivery. Some
                         vehicles might instead pick-up goods or perform some form of service (maintenance, garbage collection, 
                         vehicles carrying people etc.). We have collected data on these vehicles as well, although no actual delivery is performed."),
                       wellPanel(
                         fluidRow(
                           column(3,
                                  strong("Variable name:") 
                           ),
                           column(9,
                                  selectInput("delivery_var", " ", 
                                              choices = vardescr[vardescr[,1]=="delivery",2]
                                  )
                           )
                         ),
                         fluidRow(
                           column(3,
                                  strong("Observations:")   
                           ),
                           column(9,
                                  textOutput("nrows_delivery_var")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(3,
                                  strong("Description:")   
                           ),
                           column(9,
                                  textOutput("descr_delivery_var")
                           )
                         )
                       ) #END of wellpanel
                       ), #END of delivery tabpanel
              
              tabPanel("Vehicle variables", ##
                       br(),
                       p("Direct observation of the goods vehicles can give us a lot of information regarding: the amount of
                         pollution it generates, which company owns it and which company operates it (not necessarily the same).
                         We have collected data on the type of the vehicle and the model, whether it is refrigerated, any logo
                         or branding displayed. We have observed the loading factor (how 'full' is the truck) as well
                         as counted the number of workers operating it. Finally, we use the car plate of a vehicle as a unique identifier, although
                         they are not reported in the study for privacy concerns. Further, the information regarding
                         the side plate has been used to extract other information (origin of the vehicle trip, type of company
                         operating the vehicle etc.), while the original name of the company as written in the side plate will not
                         be used."),
                       wellPanel(
                         fluidRow(
                           column(3,
                                  br(),
                                  strong("Variable name:") 
                           ),
                           column(9,
                                  selectInput("vehicle_var", " ", 
                                              choices = vardescr[vardescr[,1]=="vehicle",2]
                                  )
                           )
                         ),
                         fluidRow(
                           column(3,
                                  strong("Observations:")   
                           ),
                           column(9,
                                  textOutput("nrows_vehicle_var")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(3,
                                  strong("Description:")   
                           ),
                           column(9,
                                  textOutput("descr_vehicle_var")
                           )
                         )
                       ) #END of wellpanel
                       ), #END of vehicle tabpanel
              
              tabPanel("Driver variables", ##
                       br(),
                       p("The agents that perform the delivery, pickup or service are usually the drivers of the goods vehicles or the helpers.
                         These agents often are responsible for taking operational decisions regarding the deliveries such as, routing, priorities,
                         time of delivery as well as are those who might decide to abandon the queue, to park on the street, or to leave the area
                         and come back later on."),
                       p("Only through driver surveys we can obtain information regarding the origin and destination of the goods, the utilization
                         of the vehicle (loading, number of stops in a day etc.), the business model behind the logistics operations we are observing
                         as well as obtaining behavioural insights regarding the parking and driving behaviours."),
                       wellPanel(
                         fluidRow(
                           column(3,
                                  br(),
                                  strong("Variable name:")
                           ),
                           column(9,
                                  selectInput("driver_var", " ", 
                                              choices = vardescr[vardescr[,1]=="driver",2]
                                  )  
                           )
                         ),
                         br(),
                         fluidRow(
                           column(3,
                                  strong("Observations:")   
                           ),
                           column(9,
                                  textOutput("nrows_driver_var")  
                           )
                         ),
                         br(),
                         fluidRow(
                           column(3,
                                  strong("Description:")   
                           ),
                           column(9,
                                  textOutput("descr_driver_var")
                           )
                         )
                       ) # END of wellpanel     
                       ),#END of drivers tabpanel
              
              tabPanel("Traffic variables", ##
                       br(),
                       p("Traffic variables aim at capturing the movement of the goods vehicles while it is not parked. From the time
                         it enters the system (i.e. the premises of the mall) to the time it starts the handling operations (loading, unloading or
                         performing some form of service). Most of these variables have been captured with cameras and thereafter processed
                         to extract information such as time of arrival and exit, total dwelling time (the difference between the previous two variables),
                         queueing time (time spent waiting in queue to get into the loading bay) and other variables.") ,
                       
                       wellPanel(
                         fluidRow(
                           column(3,
                                  br(),
                                  strong("Variable name:")
                           ),
                           column(9,
                                  selectInput("traffic_var", " ", 
                                              choices = vardescr[vardescr[,1]=="traffic",2]
                                  )  
                           )
                         ),
                         br(),
                         fluidRow(
                           column(3,
                                  strong("Observations:")   
                           ),
                           column(9,
                                  textOutput("nrows_traffic_var")   
                           )
                         ),
                         br(),
                         fluidRow(
                           column(3,
                                  strong("Description:")   
                           ),
                           column(9,
                                  textOutput("descr_traffic_var")
                           )
                         )
                       ) # END of wellpanel
                       ),#END of traffic tabpanel 
              
              tabPanel("All variables", ###
                       br(),
                       wellPanel(
                         fluidRow(
                           column(3,
                                  br(),
                                  strong("Variable name:") 
                           ),
                           column(9,
                                  selectInput("all_var", " ", 
                                              choices = vardescr[,2]
                                  )
                           )
                         ),
                         fluidRow(
                           column(3,
                                  strong("Observations:")   
                           ),
                           column(9,
                                  textOutput("nrows_all_var")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(3,
                                  strong("Description:")   
                           ),
                           column(9,
                                  textOutput("descr_all_var")
                           )
                         )
                       ) #END of wellpanel
              ) #END of "all variables" tabpanel
              ) #END of TabsetPanel
            ),
    tabItem(tabName='traffGen',
            fluidRow(
              column(3,
                     box(width=NULL,
                       selectInput("malls_map_visual", "Choose visualization:", 
                                   choices=c("Location"="loc_only",
                                             "Number of stores"="no_stores",
                                             "Stores per squared m."="store_density")
                       )
                       #"here hist",
                       #"here plot"
                     ), #END of wellPanel
                     
                     box(width=NULL,
                         fluidRow(valueBoxOutput("mallsBox",width=12)),
                         fluidRow(valueBoxOutput("tripsBox",width=12))
                     )
              ),
              column(9,
                     leafletOutput("map_malls")
              )
            )),
    tabItem(tabName='authors',
            h3("Singapore University of Technology and Design (SUTD) Research Team"),
            tags$ul(  
              tags$li("Lynette Cheah, Professor (PI)."),
              tags$li("Ngai-Man Cheung, Professor."),
              tags$li("Costas Courcoubetis, Professor."),
              tags$li("Laura Guerrero, Research Fellow."),
              tags$li(a("Giacomo Dalla Chiara, Ph.D. candidate.", href="http://esd.sutd.edu.sg/phd-students/giacomo-dalla-chiara/"))
            )),
    tabItem(tabName='mall2',
            fluidRow(
              box(width = 8,
                  height = 380,
                  imageOutput('tamp_map',height='50px')),
              box(width = 4,
                  height = 380,
                  imageOutput("tamp_overview", height = '50px'))
              ),
            fluidRow(
              tabBox(width = 12,
                tabPanel('Overview',HTML(markdownToHTML(fragment.only=TRUE, text=tampString))),
                tabPanel('Vehicle Count',
                         fluidRow(
                           br(),
                           column(3,
                                  wellPanel(
                                    radioButtons("tamp_vehicle_count_visual", "Choose type of visualization:", 
                                                 choices = c("Table","Percentage", "Barplot")),
                                    selectInput("tamp_vehicle_split", "Split by:", 
                                                choices=c("none",
                                                          "Interviewed vs non-interviewed", 
                                                          "Vehicle type", 
                                                          "Refrigerated vs. non-rerigerated",
                                                          "Parking location",
                                                          "Number of workers"))
                                  )    
                           ),
                           column(9,
                                  "main panel",
                                  DT::dataTableOutput("tamp_vehicle_table")
                           )
                         ) #END of fluidrow
                ),
                tabPanel('Delivery Count','...')
              )
            )
    ),
    tabItem(tabName='mall1',
            fluidRow(
              box(width = 8,
                  height = 380,
                  imageOutput('northp_map',height='50px')),
              box(width = 4,
                  height = 380,
                  imageOutput("northp_overview", height = '50px'))
            ),
            fluidRow(
              tabBox(width = 12,
                     tabPanel('Overview',HTML(markdownToHTML(fragment.only=TRUE, text=npString))),
                     tabPanel('Vehicle Count',
                              fluidRow(
                                br(),
                                column(3,
                                       wellPanel(
                                         radioButtons("northp_vehicle_count_visual", "Choose type of visualization:", 
                                                      choices = c("Table","Percentage", "Barplot")),
                                         selectInput("northp_vehicle_split", "Split by:", 
                                                     choices=c("No split"="none",
                                                               "Interviewed vs. non-interviewed"="employer", 
                                                               "Vehicle type"="vehicle_type", 
                                                               "Refrigerated vs. non-rerigerated"="refrigerated",
                                                               "Parking location"="park_location",
                                                               "Number of workers"="no_workers"))
                                       )    
                                ),
                                column(8, offset=1,
                                       DT::dataTableOutput("northp_vehicle_table")
                                )
                              )
                     ),
                     tabPanel('Delivery Count',
                              fluidRow(
                                br(),
                                column(3,
                                       wellPanel(
                                         radioButtons("northp_delivery_count_visual", "Choose type of visualization:", 
                                                      choices = c("Table","Percentage", "Barplot")),
                                         selectInput("northp_delivery_split", "Split by:", 
                                                     choices=c("Deliveries vs. pick-ups"="del_pick", 
                                                               "Size of delivery"="del_size", 
                                                               "Commodity type"="commodity_type",
                                                               "Type of store deliverying to"="store_type"))
                                       )    
                                ),
                                column(8, offset=1,
                                       DT::dataTableOutput("northp_delivery_table")
                                )
                              ))
              )
            )
            
            
            
            
            
    ),
    tabItem(tabName='sysPerf',
            fluidRow(
              box(width=4,
                  checkboxInput("without_passby", "Eliminates vehicles passying by"),
                  checkboxGroupInput("park_location", "Show vehicles that parked...", 
                                     choices = c("...inside the loading bay"="LB",
                                                 "...inside the passenger carpark"="carpark", 
                                                 "...on the street"="street"))
              ),
              box(width=8,
                     dygraphOutput("step_plot")
              )
            )),
    tabItem(tabName='arrivals',
            fluidRow(
              box(width=3,
                  checkboxGroupInput("input_date", "Choose date:", 
                                     choices = c("Wednesday 24th June"="2015-06-24", 
                                                 "Thursday 25th June"="2015-06-25", 
                                                 "Friday 26th June"="2015-06-26"),
                                     selected = "Wednesday 24th June"),
                  sliderInput("input_interval", label="Choose time interval size:", value=50, min=10, max=90, step=10)
              ),
              column(9,
                     dygraphOutput("arrivals_plot")
              )
              
            )),
    tabItem(tabName='handling',
            fluidRow(
              box(width=3,
                  selectInput("which_visualization_handling", "Select the visualization:", 
                              choices= c("Handling times distribution"="htime_distribution", 
                                         "Handling times vs. size of delivery/pick-up"="htime_delsize",
                                         "Handling time by arrival time"), 
                              selected="htime_distribution"),
                  h4("Add filters:"),
                  checkboxInput("onlygv_handling", "Include only goods vehicle"),
                  checkboxInput("byparkloc", "Differentiate by park location"),
                  conditionalPanel(
                    condition = "input.byparkloc",
                    checkboxGroupInput("park", "Park location:", c("Street"="street", "Loading bay"="LB", "Carpark"="carpark"))
                  )
              ), #END of column
              box(width=9,
                     conditionalPanel(
                       condition="input.which_visualization_handling=='htime_distribution'",
                       textOutput("nrowsfinal"),
                       plotOutput("hist_htime")
                     ),
                     conditionalPanel(
                       condition="input.which_visualization_handling=='htime_delsize'",
                       #plotOutput("delsize_htime")
                       "coming soon!"
                     )
              )
            )),
    tabItem(tabName='queueing',
            fluidRow(
              box(width=4,
                     checkboxInput("onlygv_queue", "Include only goods vehicle."),
                     checkboxInput("onlyLB_queue", "Include only vehicles that parked in the loading bay.")
              ),
              
              box(width=8,
                     "main panel",
                     plotOutput("hist_queue")
              )
            )),
    tabItem(tabName='dwelling',
            fluidRow(
              box(width=4,
                  checkboxInput("onlygv_dwell", "Include only goods vehicle.")
              ),
              box(width=8,
                  plotOutput("hist_dwell")
              )
            ))
            
    ) # END of tabItems
  ) # END of sidebarBody

dashboardPage(header,sidebar,body)



      