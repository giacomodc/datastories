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
library(DT)

#load dataset
northp <- read.csv("northp.csv", stringsAsFactors = FALSE, header=T)
#create a empty factor variable to filter the data
northp[,"filter"] <- "no_filter"
northp[,"filter"] <- as.factor(northp[,"filter"])
#defines variables time
northp[,"arrival_time"] <- as.POSIXct(northp[,"arrival_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
northp[,"exit_time"] <- as.POSIXct(northp[,"exit_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
northp[,"start_handling_time"] <- as.POSIXct(northp[,"start_handling_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
northp[,"stop_handling_time"] <- as.POSIXct(northp[,"stop_handling_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")

vardescr <- read.table("vardescr.txt", sep="\t", stringsAsFactors = F)
tamp_retailers <- read.csv("tamp_retailers.csv", stringsAsFactors = FALSE, header=T)

#MAP case studies:
m <- leaflet()
m <- addProviderTiles(m, "Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) )
m <- addMarkers(m, lng=c(103.944696, 103.836052), lat=c(1.352683, 1.429508), layerId=c("tamp", "north") ,popup=c("Mall2", "Mall1"))

#MAP shopping malls
malls <- read.csv("shopping_malls.csv", stringsAsFactors = FALSE, header=T, na.strings=c("","NA"))



##############################################################################################################
shinyServer(function(input, output, session) {

### DATA DESCRIPTION 
#delivery
output$nrows_delivery_var <- reactive({
  n <- length(northp[!is.na(northp[,input$delivery_var]),input$delivery_var])
  paste("There are", n, "available observations.")
})  
output$descr_delivery_var <- reactive({
  nome_var <- input$delivery_var
  descr <- vardescr[vardescr[,2]==nome_var,3]
  descr
})
#vehicle
output$nrows_vehicle_var <- reactive({
  n <- length(northp[!is.na(northp[,input$vehicle_var]),input$vehicle_var])
  paste("There are", n, "available observations.")
})  
output$descr_vehicle_var <- reactive({
  nome_var <- input$vehicle_var
  descr <- vardescr[vardescr[,2]==nome_var,3]
  descr
})
#driver
output$nrows_driver_var <- reactive({
  n <- length(northp[!is.na(northp[,input$driver_var]),input$driver_var])
  paste("There are", n, "available observations.")
})  
output$descr_driver_var <- reactive({
  nome_var <- input$driver_var
  descr <- vardescr[vardescr[,2]==nome_var,3]
  descr
})
#traffic
output$nrows_traffic_var <- reactive({
  n <- length(northp[!is.na(northp[,input$traffic_var]),input$traffic_var])
  paste("There are", n, "available observations.")
})  
output$descr_traffic_var <- reactive({
  nome_var <- input$traffic_var
  descr <- vardescr[vardescr[,2]==nome_var,3]
  descr
})
#ALL
output$nrows_all_var <- reactive({
  n <- length(northp[!is.na(northp[,input$all_var]),input$all_var])
  paste("There are", n, "available observations.")
})  
output$descr_all_var <- reactive({
  nome_var <- input$all_var
  descr <- vardescr[vardescr[,2]==nome_var,3]
  descr
})   






### FLOWCHART
output$LBbay_flowchart <- renderImage({
  filename <- normalizePath(file.path('./images/LBbay_flowchart.jpg', fsep=''))
  list(src=filename,
        width=800)
}, deleteFile = F)
  





### MALLS MAP
#forecast total trips generated
output$tot_malls <- reactive(nrow(malls))

output$trips <- reactive(sum(malls$no_stores_mallwebsite, na.rm = T)*3)

#generate circle size
tmp_malls <- reactive({
  tmp <- malls[malls[,"open_close"]=="open" & !is.na(malls[,"lat"]) & malls[,"type"]=="mall",]
  if (input$malls_map_visual=="no_stores") {
    tmp[,"circle_size"] <- NA
    minimo <- 5 #min circle size
    massimo <- 30 #max circle size
    tmp[!is.na(tmp[,"no_stores_mallwebsite"]),"circle_size"] <- 
      (tmp[!is.na(tmp[,"no_stores_mallwebsite"]),"no_stores_mallwebsite"]-min(tmp[,"no_stores_mallwebsite"], na.rm = T))*
      (massimo/(max(tmp[,"no_stores_mallwebsite"], na.rm = T)-min(tmp[,"no_stores_mallwebsite"], na.rm = T))) + 
      minimo
  }
  tmp
})

#create map
output$map_malls <- renderLeaflet({
  if (input$malls_map_visual=="loc_only" | input$malls_map_visual=="store_density") {
    m_malls <- 
      leaflet(data=tmp_malls()) %>% 
      addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>%
      addMarkers(~lon, ~lat, popup = ~as.character(name), clusterOptions = markerClusterOptions())
  }
  if (input$malls_map_visual=="no_stores") {
    m_malls <- 
      leaflet(data=tmp_malls()) %>% 
      addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>%
      addCircleMarkers(~lon, ~lat, popup = ~as.character(name), radius= ~circle_size, stroke = FALSE, fillOpacity = 0.5)
  }
  m_malls})




### NORTHPOINT IMAGE
output$northp_map <- renderImage({
  filename <- normalizePath(file.path('./images/northp_map.jpg', fsep=''))
  list(src=filename,
       width=600)
}, deleteFile = F)

### TAMPINES IMAGE
output$tamp_map <- renderImage({
  filename <- normalizePath(file.path('./images/tamp_map.jpg', fsep=''))
  list(src=filename,
       width=600)
}, deleteFile = F)





### DATA SUMMARY TAMPINES
## Tampines overview
output$tamp_overview <- renderImage({
  filename <- normalizePath(file.path('./images/tamp_overview.jpg', fsep=''))
  list(src=filename,
       width=370)
}, deleteFile = F)


### DATA SUMMARY NORTHPOINT
## Northpoint overview
output$northp_overview <- renderImage({
  filename <- normalizePath(file.path('./images/northp_overview.jpg', fsep=''))
  list(src=filename,
       width=370)
}, deleteFile = F)

## Table vehicles
northp_vehicle_table_data <- reactive({
  dates_name <- c("Wednesday 24th", "Thursday 25th", "Friday 26th")
  dates <- c("2015-06-24", "2015-06-25", "2015-06-26")
  if (input$northp_vehicle_split=="none") {
    temp <- data.frame(t(data.frame(table(northp$date))))[2,]
    names(temp) <- dates_name
    rownames(temp) <- "Tot. GVs observed"
    }
  if (input$northp_vehicle_split=="employer") {
    temp <- data.frame(matrix(nrow=3, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("Interviewed", "Non-interviewed", "Total")
    for (i in 1:3) {
      temp[1:2,i] <- table(is.na(northp[northp[,"date"]==dates[i] & !is.na(northp[,"date"]),input$northp_vehicle_split]))
      temp["Total",i] <- sum(temp[1:2,i])
    }
  }
  if (input$northp_vehicle_split=="refrigerated") {
    temp <- data.frame(matrix(nrow=3, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("Refrigerated", "Non-refriegrated", "Total")
    for (i in 1:3) {
      temp[1:2,i] <- table(is.na(northp[northp[,"date"]==dates[i] & !is.na(northp[,"date"]),input$northp_vehicle_split]))
      temp["Total",i] <- sum(temp[1:2,i])
    }
  }
  if (input$northp_vehicle_split=="vehicle_type") {
    temp <- data.frame(matrix(nrow=7, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("SmallVans", "LargeVans", "ClosedLightTrucks", "OpenLightTrucks", "HeavyTrucks", "Others", "Total")
    for (i in 1:3) {
      temp[c(3, 5, 2, 4, 6, 1),i] <- table(northp[northp[,"date"]==dates[i] & !is.na(northp[,"vehicle_type"]),input$northp_vehicle_split])
      temp["Total",i] <- sum(temp[1:6,i])
   }
  }
  if (input$northp_vehicle_split=="park_location") {
    temp <- data.frame(matrix(nrow=4, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("Loading Bay", "Passenger carpark", "On street", "Total")
    for (i in 1:2) {
      temp[c(1,3),i] <- table(northp[northp[,"date"]==dates[i] & !is.na(northp[,"vehicle_type"]),input$northp_vehicle_split])
      temp["Total",i] <- sum(temp[c(1,3),i])
    }
    temp[c(2,1,3),3] <- table(northp[northp[,"date"]==dates[3] & !is.na(northp[,"vehicle_type"]),input$northp_vehicle_split])
    temp["Total",3] <- sum(temp[1:3,3])
    temp[] <- lapply(temp, as.character)
    temp[2,1:2] <- "NA"
  }
  if (input$northp_vehicle_split=="no_workers") {
    temp <- data.frame(matrix(nrow=3, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("One driver only", "One driver and at least one helper", "Total")
    for (i in 1:3) {
      temp[1,i] <- table(northp[northp[,"date"]==dates[i] & !is.na(northp[,"vehicle_type"]),input$northp_vehicle_split])[[1]]
      temp[2,i] <- sum(table(northp[northp[,"date"]==dates[i] & !is.na(northp[,"vehicle_type"]),input$northp_vehicle_split])[-1])
      temp["Total",i] <- sum(temp[1:2,i])
    }
  }
  temp
})

output$northp_vehicle_table <- DT::renderDataTable(
  DT::datatable(northp_vehicle_table_data(), options = list(paging = FALSE, searching = FALSE))
)

## Table deliveries
northp_delivery_table_data <- reactive({
  dates_name <- c("Wednesday 24th", "Thursday 25th", "Friday 26th")
  dates <- c("2015-06-24", "2015-06-25", "2015-06-26")
  if (input$northp_delivery_split=="del_pick") {
    temp <- data.frame(matrix(nrow=5, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("Deliveries", "Pick-ups", "Delivery & pick_up", "Nothing", "Total")
    for (i in 1:3) {
      temp[1:4,i] <- table(northp[northp[,"date"]==dates[i] & !is.na(northp[,"date"]),input$northp_delivery_split])
      temp["Total",i] <- sum(temp[1:4,i])
    }
  }
  if (input$northp_delivery_split=="del_size") {
    temp <- data.frame(matrix(nrow=6, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("(0,0.5] m^3", "(0.5,1.5] m^3", "(1.5,2.5] m^3", "(2.5,5] m^3", "more than 5 m^3", 
                        "Total")
    for (i in 1:3) {
      temp[1:5,i] <- table(cut(northp[northp[,"date"]==dates[i] & !is.na(northp[,"date"]),input$northp_delivery_split], breaks = c(0, 0.5, 1.5, 2.5, 5, 35)))
      temp["Total",i] <- sum(temp[1:5,i])
    }
  }
  if (input$northp_vehicle_split=="commodity_type") {
    temp <- data.frame(matrix(nrow=13, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("clothing_accessories", "cosmetics_cleaning", "electronics","fresh_frozen_food","household", 
                        "nonperishable_food", "others", "pharmaceutical", "prepared_food", "recreational_goods", 
                        "service_trash", "stationery", "Total")
    tmp_northp <- northp[!is.na(northp[,"commodity_type"]), c("commodity_type", "date")]
    tmp_northp[tmp_northp[,"commodity_type"]=="optics_photography","commodity_type"] <- "others"
    tmp_northp[tmp_northp[,"commodity_type"]=="jewelry","commodity_type"] <- "others"
    tmp_northp[tmp_northp[,"commodity_type"]=="gardening_pets","commodity_type"] <- "others"
    for (i in 1:3) {
      temp[1:12,i] <- table(tmp_northp[tmp_northp[,"date"]==dates[i], input$northp_delivery_split])
      temp["Total",i] <- sum(temp[1:12,i])
    }
  }
  if (input$northp_vehicle_split=="store_type") {
    temp <- data.frame(matrix(nrow=11, ncol=3))
    names(temp) <- dates_name
    rownames(temp) <- c("books_stationery", "childrenswear_toys_maternity", "conveniencestore","electronics","fashion", 
                        "food_restaurants", "others", "pharmacy_healthcare_cosmetics", "services", "supermarket", 
                        "Total")
    for (i in 1:3) {
      tmp_northp <- c(northp[!is.na(northp[,"store_type1"]) & northp[,"date"]==dates[i],"store_type1"], 
                      northp[!is.na(northp[,"store_type2"]) & northp[,"date"]==dates[i],"store_type2"], 
                      northp[!is.na(northp[,"store_type3"]) & northp[,"date"]==dates[i],"store_type3"], 
                      northp[!is.na(northp[,"store_type4"]) & northp[,"date"]==dates[i],"store_type4"])
      tmp_northp <- tmp_northp[!is.na(tmp_northp)]
      tmp_northp[tmp_northp=="bank_moneychanger"] <- "services"
      tmp_northp[tmp_northp=="entertainment"] <- "services"
      tmp_northp[tmp_northp=="music_audio"] <- "electronics"
      tmp_northp[tmp_northp=="beauty"] <- "pharmacy_healthcare"
      tmp_northp[tmp_northp=="pharmacy_healthcare"] <- "pharmacy_healthcare_cosmetics"
      tmp_northp[tmp_northp=="sports_leisure"] <- "others"
      tmp_northp[tmp_northp=="optical"] <- "others"
      tmp_northp[tmp_northp=="gifts"] <- "others"
      tmp_northp[tmp_northp=="jewellery_watches"] <- "others"
      temp[1:10,i] <- table(tmp_northp)
      temp["Total",i] <- sum(temp[1:10,i])
    }
  }
  temp
})

output$northp_delivery_table <- DT::renderDataTable(
  DT::datatable(northp_delivery_table_data(), options = list(paging = FALSE, searching = FALSE))
)






### HANDLING  
data_handling <- reactive({
  temp <- northp[!is.na(northp[,"htime"]) & northp[,"htime"]>0,]
  if (input$onlygv_handling==T) temp <- temp[temp[,"service"]=="GV" & !is.na(temp[,"service"]),] #GV vs. not GV
  if (input$byparkloc == T) { #filtering by parking location
    temp <- temp[temp[,"park_location"]==input$park & !is.na(temp[,"park_location"]),]
    temp[,"filter"] <- temp[,"park_location"]
    temp[,"filter"] <- as.factor(temp[,"filter"])
  } 
  temp <- as.data.frame(temp)
  temp
})

output$nrowsfinal <- reactive({
  dat <- data_handling()
  paste("There are",nrow(dat), "observations plotted.")
})

output$hist_htime <- renderPlot({
  dat <- data_handling()
  ggplot(dat,aes(x=dat[,"htime"], fill=dat[,"filter"])) +
    geom_histogram(binwidth = 1.5, alpha = 0.6, position="identity") +
    ggtitle("Handling time distribution") + theme_bw() + xlab("time (minutes)") + 
    theme(axis.title.x = element_text(size=16), axis.text.x  = element_text(size=12), axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12), plot.title = element_text(size=20))
    #geom_vline(aes(xintercept=10), colour="#990000", linetype="dashed") +
  })
  

### QUEUEING
data_queue <- reactive({
  temp <- northp
  if (input$onlygv_queue==T) temp <- temp[temp[,"service"]=="GV",] #GV vs. not GV
  if (input$onlyLB_queue==T) temp <- temp[temp[,"park_location"]=="LB" & !is.na(temp[,"park_location"]),] #only in LB bay
  temp <- temp[temp[,"qtime"]<=60,] ###ASSUMPTION: we don't believe qtimes longer than 60 minutes
  temp <- as.data.frame(temp)
  temp
}) #END of data_queue 
output$hist_queue <- renderPlot({
  dat <- data_queue()
  ggplot(dat,aes(x=dat[,"qtime"])) +
    geom_histogram(binwidth = 1.5, alpha = 0.6, position="identity") +
    ggtitle("Queueing time distribution") + theme_bw() + xlab("time (minutes)") +
    theme(axis.title.x = element_text(size=16), axis.text.x  = element_text(size=12), axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12), plot.title = element_text(size=20))
})




### DWELLING
## dwelling time distribution
data_dwell <- reactive({
  temp <- northp[!is.na(northp[,"dtime"]),]
  if (input$onlygv_dwell==T) temp <- temp[temp[,"service"]=="GV" & !is.na(temp[,"service"]),] #GV vs. not GV
  #temp <- temp[temp[,"dtime"]<=60,] ###ASSUMPTION: we don't believe that trucks waited more than 60 minutes
  temp <- as.data.frame(temp)
  temp
}) #END of data_queue 
output$hist_dwell <- renderPlot({
  dat <- data_dwell()
  ggplot(dat,aes(x=dat[,"dtime"])) +
    geom_histogram(binwidth = 1.5, alpha = 0.6, position="identity") +
    ggtitle("Dwelling time distribution") + theme_bw() + xlab("time (minutes)")
})




### VEHICLES IN THE SYSTEM
data_no_agents <- reactive({
  temp <- northp[northp[,"date"]=="2015-06-26" & !is.na(northp[,"arrival_time"]) & !is.na(northp[,"exit_time"]),c("park_location", "arrival_time", "exit_time", "dtime")]
  if (input$without_passby==T) temp <- temp[temp[,"dtime"]>3,] #if dwelling <3min then they are passing by
  if (is.null(input$park_location)) {
    entries <- data.frame(cbind(as.character(temp[,"arrival_time"]), rep(1,length(temp[,"arrival_time"]))), stringsAsFactors=F)
    exits <- data.frame(cbind(as.character(temp[,"exit_time"]), rep(-1,length(temp[,"exit_time"]))), stringsAsFactors=F)
  } else {
    entries <- data.frame(cbind(as.character(temp[,"arrival_time"]), rep(1,length(temp[,"arrival_time"])), rep(0,length(temp[,"arrival_time"]))), stringsAsFactors=F)
    exits <- data.frame(cbind(as.character(temp[,"exit_time"]), rep(-1,length(temp[,"exit_time"])), rep(0, length(temp[,"exit_time"]))), stringsAsFactors=F)
    entries[temp[,"park_location"] %in% input$park_location & !is.na(temp[,"park_location"]),3] <- rep(1, length(entries[temp[,"park_location"] %in% input$park_location & !is.na(temp[,"park_location"]),3]))
    exits[temp[,"park_location"] %in% input$park_location & !is.na(temp[,"park_location"]),3] <- rep(-1, length(exits[temp[,"park_location"] %in% input$park_location & !is.na(temp[,"park_location"]),3]))
  }
  no_agents <- rbind(entries, exits) 
  names(no_agents)[1] <- "time"
  no_agents[,"time"] <- as.POSIXct(no_agents[,"time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
  no_agents <- no_agents[order(no_agents[,"time"] ),]
  while (max(table(no_agents[,"time"])) >1) {
    for (i in 2:(nrow(no_agents)-1)) if (no_agents[i,"time"]==no_agents[i-1,"time"]) no_agents[i,"time"] <- no_agents[i,"time"]+1
    no_agents <- no_agents[order(no_agents[,"time"] ),]
  }
  #no_agents[,"change"] <- as.numeric(no_agents[,"change"])
  #no_agents[,"tot"] <- NA
  #no_agents[1,"tot"] <- no_agents[1,"change"]
  #for (i in 2:nrow(no_agents)) no_agents[i,"tot"] <- no_agents[i-1,"tot"]+no_agents[i,"change"]
  if (ncol(no_agents)==2) {
    no_agents[,2] <- as.numeric(no_agents[,2])
    no_agents[,3] <- NA
    no_agents[1,3] <- no_agents[1,2]
    for (j in 2:nrow(no_agents)) no_agents[j,3] <- no_agents[j-1,3]+no_agents[j,2]
  } else {
    for (i in 2:ncol(no_agents)) {
      no_agents[,i] <- as.numeric(no_agents[,i])
      no_agents[,i+2] <- NA
      no_agents[1,i+2] <- no_agents[1,i]
      for (j in 2:nrow(no_agents)) no_agents[j,i+2] <- no_agents[j-1,i+2]+no_agents[j,i]
    }
  }
  rownames(no_agents) <- no_agents[,"time"]
  if (is.null(input$park_location)) {
    no_agents <- no_agents[,-1]
    no_agents[,1] <- no_agents[,2]
    names(no_agents)[1] <- "goods vehicles (GVs)"
  } else {
    no_agents <- no_agents[,4:5]
    names(no_agents) <- c("goods vehicles", paste("GVs parked ", paste(input$park_location, collapse = ' or ')))
  }
  step_plot <- as.xts(no_agents)
  step_plot
  #step_plot[,1]
  })
output$step_plot <- renderDygraph({
  dygraph(data_no_agents(), main="Total number of goods vehicles in the system") %>%
    dyOptions(stepPlot=T, useDataTimezone = TRUE) %>%
    dyAxis("y", label = "No. goods vehicles") %>%
    #dyAxis("x", label = "Time") %>%
    dyLimit(6, color = "red")
  })



######      DATA SUMMARIES      ######

### MAP case studies
output$mymap <- renderLeaflet({m})
  
clickonmap <- reactive({
  prova <- input$mymap_marker_click$id
  if (is.null(prova)) return (NULL) 
  prova
})
  
output$data_collection_location <- reactive({
  return(!is.null(clickonmap()))
})
  
output$clickme <- reactive({return(clickonmap())})
  
      
### ARRIVALS
arrivals_plot_data <- reactive({ ####
  input_interval <- input$input_interval
  input_date <- input$input_date
  arrivals_table <- data.frame(rep(NA, 
                        length(seq(from = as.POSIXct(paste(input_date[1], "06:00:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                   to = as.POSIXct(paste(input_date[1], "18:00:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                   by=input_interval*60))))
  for (i in 1:length(input_date)) {
    breaks_vector <- seq(from = as.POSIXct(paste(input_date[i], "06:00:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                         to = as.POSIXct(paste(input_date[i], "18:00:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"),
                         by=input_interval*60)
    temp <- northp[!is.na(northp[,"date"]) & !is.na(northp[,"arrival_time"]) & northp[,"date"]==input_date[i],1:5]
    vector_arrivals <- data.frame(table(cut(temp$arrival_time, breaks=breaks_vector)))[,2]
    if (length(vector_arrivals)<nrow(arrivals_table)) vector_arrivals <- c(vector_arrivals, rep(0, nrow(arrivals_table)-length(vector_arrivals)))
    arrivals_table <- cbind(arrivals_table, vector_arrivals)
    }
  arrivals_table[,1] <- as.POSIXct(seq(from = as.POSIXct(paste(input_date[1], "06:00:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                       to = as.POSIXct(paste(input_date[1], "18:00:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                       by=input_interval*60), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
  names(arrivals_table) <- c("aaa", format(as.Date(input_date, tz="America/Los_Angeles"), format="%B %d %Y"))
  rownames(arrivals_table) <- arrivals_table[,1]
  arrivals_table <- as.xts(arrivals_table)
  arrivals_table[,-1]
  })  

output$arrivals_plot <- renderDygraph({ ####
  title_plot <- paste("Number of arrivals per", input$input_interval, "minute intervals") 
  dygraph(arrivals_plot_data(), main=title_plot) %>% 
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    dyLegend(labelsSeparateLines=T, width=170) %>%
    dyAxis("y", label="No. vehicles observed") %>%
    dyAxis("x", label="Start of interval") %>% 
    dyOptions(useDataTimezone = TRUE)
  })



}) #END of shinyServer()

