library(base)

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

get.month <- function(date) {
  as.integer(substr(date,6,7))
}

get.year <- function(date) {
  as.integer(substr(date,1,4))
}

data <- read.csv('sample_car_dealer_visits.csv')

dates <- unique(as.Date(data$dt)[order(as.Date(data$dt))])
start.date <- head(dates, 1)
end.date <- tail(dates, 1)

series <- list()

for (name in unique(as.character(data$place_name))) {
  series[[name]] <- NULL
  series[[name]]$visits <- integer(length(seq(start.date,end.date,by='days')))
}

for (i in 1:length(data$dt)) {
  name <- as.character(data$place_name[i])
  date <- as.Date(data$dt[i])
  series[[name]]$visits[get.pos(start.date, end.date, date)] <- 
    inc(series[[name]]$visits[get.pos(start.date, end.date, date)])

  series[[name]]$lat <- c(series[[name]]$lat, as.double(data$lat[i]))
  series[[name]]$lng <- c(series[[name]]$lng, as.double(data$lng[i]))
}

for (name in names(series)) {
  series[[name]]$ts <- ts(series[[name]]$visits, frequency=365,
    start=c(get.year(start.date), as.numeric(format(start.date, "%j"))))
}

plot(1, xlim=c(-8.2,-7.85), ylim=c(-35,-34.82))
output <- list()
for (name in names(series)) {
  points(mean(series[[name]]$lat), mean(series[[name]]$lng))
  output[['lat']] <- c(output[['lat']], mean(series[[name]]$lat))
  output[['lng']] <- c(output[['lng']], mean(series[[name]]$lng))
}
write.csv(output, file='location-output.csv', row.names=FALSE)

# for (key in names(series)) {
#   plot(series[[key]]$ts, ylab=key)
# }

# avg <- list()
# for (key in names(series)) {
#   avg[[key]] <- mean(series[[key]]$ts)
# }
# avg <- avg[avg > 1.9]
# plot(values.num(avg), ylab='average')