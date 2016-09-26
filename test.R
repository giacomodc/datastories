dat <- read.csv('data/merged.csv',stringsAsFactors = FALSE, header=T)
dat[,"entry_time"] <- as.POSIXct(dat[,"entry_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
dat[,"exit_time"] <- as.POSIXct(dat[,"exit_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
input_date <- c('2016-01-21','2016-01-22')
input_time <- c('6:00:00','18:00:00')
time_filter <- c(6,18)
input_interval <- 50
park_location <- NULL
mall_filter <- 'Mall 2'

start <- Sys.time()
temp <- dat
temp <- temp[!is.na(temp[,"entry_time"]) & !is.na(temp[,"exit_time"]),c("park_location", "entry_time", "exit_time", "dtime")]
if (!is.null(park_location)){
  temp <- subset(temp,temp$park_location %in% park_location)
}
entries <- data.frame(cbind(as.character(temp[,"entry_time"]), rep(1,length(temp[,"entry_time"]))), stringsAsFactors=F)
exits <- data.frame(cbind(as.character(temp[,"exit_time"]), rep(-1,length(temp[,"exit_time"]))), stringsAsFactors=F)

no_agents <- rbind(entries, exits) 
names(no_agents)[1] <- "time"
no_agents[,"time"] <- as.POSIXct(no_agents[,"time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
no_agents <- no_agents[order(no_agents[,"time"] ),]
while (max(table(no_agents[,"time"])) >1) {
  for (i in 2:(nrow(no_agents)-1)) if (no_agents[i,"time"]==no_agents[i-1,"time"]) no_agents[i,"time"] <- no_agents[i,"time"]+1
  no_agents <- no_agents[order(no_agents[,"time"] ),]
}

no_agents[,2] <- as.numeric(no_agents[,2])
no_agents[,3] <- NA
no_agents[1,3] <- no_agents[1,2]
for (j in 2:nrow(no_agents)) no_agents[j,3] <- no_agents[j-1,3]+no_agents[j,2]

no_agents <- no_agents[,c(1,3)]
names(no_agents) <- c('time','number')


system_table <- data.frame(rep(NA, 
                               length(seq(from = as.POSIXct(paste(input_date[1], input_time[1]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                          to = as.POSIXct(paste(input_date[1], input_time[2]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                          by=1))))

names(system_table) <- 'time'

for (i in 1:length(input_date)) {
  system_table[,1] <- seq(from = as.POSIXct(paste(input_date[i], input_time[1]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                     to = as.POSIXct(paste(input_date[i], input_time[2]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                     by=1)
  system_table <- merge(system_table,no_agents,by='time',all.x=TRUE)
  system_table[1,i+1] <- 0
  system_table[i+1] <- na.locf(system_table[i+1])
}
names(system_table) <- c("aaa", format(as.Date(input_date, tz="Asia/Singapore"), format="%B %d %Y"))
mytime <- system_table[,1]
myvalue <- system_table[,c(2,3)]

system_table <- xts(myvalue,order.by=mytime,tz='Asia/Singapore')

stepplot <- dygraph(system_table, main="Total number of goods vehicles in the system") %>% 
  dyHighlight(highlightCircleSize = 3, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyLegend(labelsSeparateLines=T, width=170) %>%
  dyAxis("y", label="No. goods vehicles") %>%
  dyAxis("x", label="Time") %>% 
  dyOptions(useDataTimezone = T)
if (mall_filter=='Mall 1'){
  stepplot <- dyLimit(stepplot, 6, color = 'red')
}else if (mall_filter=='Mall 2'){
  stepplot <- dyLimit(stepplot, 18, color = 'green')
}else{
  stepplot <- dyLimit(stepplot, 6, color = 'red')
  stepplot <- dyLimit(stepplot, 18, color = 'green')
}
stepplot
Sys.time()-start
