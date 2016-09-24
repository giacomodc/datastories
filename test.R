dat <- read.csv('data/merged.csv',stringsAsFactors = FALSE, header=T)
dat[,"entry_time"] <- as.POSIXct(dat[,"entry_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
dat[,"exit_time"] <- as.POSIXct(dat[,"exit_time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
input_date <- c('2015-06-24','2015-06-25','2015-06-26')
input_time <- c('6:00:00','18:00:00')
input_interval <- 50
parking <- NULL

temp <- dat
temp <- subset(temp, temp$date %in% input_date)
temp <- subset(temp, hour(temp$entry_time)>=6&hour(temp$entry_time)<=18)
temp <- temp[!is.na(temp[,"entry_time"]) & !is.na(temp[,"exit_time"]),c("park_location", "entry_time", "exit_time", "dtime")]
if (!is.null(parking)){
  temp <- subset(temp,temp$park_location %in% parking)
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
no_agents[,"time"] <- as.POSIXct(no_agents[,"time"], format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")


arrivals_table <- data.frame(rep(NA, 
                                 length(seq(from = as.POSIXct(paste(input_date[1], input_time[1]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                            to = as.POSIXct(paste(input_date[1], input_time[2]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                            by=1))))

names(arrivals_table) <- 'time'

for (i in 1:length(input_date)) {
  arrivals_table[,1] <- as.POSIXct(seq(from = as.POSIXct(paste(input_date[i], input_time[1]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                       to = as.POSIXct(paste(input_date[i], input_time[2]), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore"), 
                                       by=1), format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
  arrivals_table <- merge(arrivals_table,no_agents,by='time',all.x=TRUE)
  arrivals_table[1,i+1] <- 0
  arrivals_table[i+1] <- na.locf(arrivals_table[i+1])
}
rownames(arrivals_table) <- as.POSIXct(arrivals_table[,1],format="%Y-%m-%d %H:%M:%S", tz="Asia/Singapore")
names(arrivals_table) <- c("aaa", format(as.Date(input_date, tz="Asia/Singapore"), format="%B %d %Y"))
arrivals_table <- as.xts(arrivals_table)
arrivals_table <- arrivals_table[,-1]

title_plot <- paste("Number of arrivals per", "minute intervals") 
dygraph(arrivals_table, main=title_plot) %>% 
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyLegend(labelsSeparateLines=T, width=170) %>%
  dyAxis("y", label="No. vehicles observed") %>%
  dyAxis("x", label="Start of interval") %>% 
  dyOptions(useDataTimezone = F)
