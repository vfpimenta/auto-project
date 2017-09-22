library(base)
library(ggmap)
library(ggplot2)

values.num <- function(list) {
  as.numeric(paste(unlist(list)))
}

add.days <- function(date,n) {
  seq(date, by = paste (n, "days"), length = 2)[2]
}

get.pos <- function(lb, up, dt) {
  i = 1
  for(date in seq(lb, up, by='days')) {
    if (dt == date) {
      return(i)
    }
    i = i + 1
  }

  stop('Date not in range!')
}

inc <- function(el) {
  if (typeof(el) == 'double') {
    return(el + 1)
  } else {
    return(1)
  }
}

get.day <- function(date){
  as.integer(substr(date,9,10))
}

get.month <- function(date) {
  as.integer(substr(date,6,7))
}

get.year <- function(date) {
  as.integer(substr(date,1,4))
}

plot.trend <- function(series, mode) {
  for (key in names(series)) {
    png(paste('./plots/',mode,'-trend-',key,'.png',sep=''))
    plot(decompose(series[[key]]$ts)$trend, ylab=key, 
      ylim=c(min(series[[key]]$ts),max(series[[key]]$ts)))
    dev.off()
  }
}

data <- read.csv('sample_car_dealer_visits.csv')
centers <- read.csv('cluster_centers.csv')

dates <- unique(as.Date(data$dt)[order(as.Date(data$dt))])
start.date <- head(dates, 1)
end.date <- tail(dates, 1)

series <- list()

for (name in unique(as.character(data$place_name))) {
  series[[name]] <- NULL
  series[[name]]$visits <- integer(length(seq(start.date,end.date,by='days')))
}

wd <- list("domingo"=0,"segunda"=0,"terça"=0,
  "quarta"=0,"quinta"=0,"sexta"=0,"sábado"=0)

md <- list()
for (i in 1:31) {
  md[[i]] = 0
}

for (i in 1:length(data$dt)) {
  name <- as.character(data$place_name[i])
  date <- as.Date(data$dt[i])
  series[[name]]$visits[get.pos(start.date, end.date, date)] <- 
    inc(series[[name]]$visits[get.pos(start.date, end.date, date)])

  series[[name]]$lat <- c(series[[name]]$lat, as.double(data$lat[i]))
  series[[name]]$lng <- c(series[[name]]$lng, as.double(data$lng[i]))

  wd[[weekdays(date)]] <- inc(wd[[weekdays(date)]])
  md[[get.day(date)]] <- inc(md[[get.day(date)]])
}

ts.means <- c()
lat.means <- c()
lng.means <- c()

for (name in names(series)) {
  series[[name]]$ts <- ts(series[[name]]$visits, frequency=365,
    start=c(get.year(start.date), as.numeric(format(start.date, "%j"))))

  ts.means <- c(ts.means, mean(series[[name]]$ts))
  lat.means <- c(lat.means, mean(series[[name]]$lat))
  lng.means <- c(lng.means, mean(series[[name]]$lng))
}

png('./plots/weekdays-visits.png', width=960, height=960)
barplot(values.num(wd), names.arg=names(wd))
dev.off()

png('./plots/monthdays-visits.png', width=960, height=960)
barplot(values.num(md), names.arg=as.character(1:31))
dev.off()

png('./plots/map-visits.png', width=960, height=1920)
pe <- c(left = -35, top = -7.85, right = -34.82, bottom = -8.2)
map <- get_stamenmap(pe, zoom = 12, maptype = "toner-background")
p <- ggmap(map)
d <- data.frame(lat=lat.means, lng=lng.means)
cc <- data.frame(lat=centers$lat, lng=centers$lng)
p <- p + geom_point(data=d, aes(x=lng, y=lat), size=ts.means*2, color='red')
p <- p + geom_point(data=cc, aes(x=lng, y=lat), size=100, color='red', 
  alpha=0.3)
ggplot_build(p)
dev.off()