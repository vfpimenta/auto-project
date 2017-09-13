library(base)
library(readxl)
library(tseries)

values.num <- function(list) {
  as.numeric(paste(unlist(list)))
}

file <- 'Auto Data.xlsx'

sheet.input <- read_excel(file, sheet=1)
sheet.vertical <- read_excel(file, sheet=2)
sheet.type <- read_excel(file, sheet=3)

sheet.vertical.length <- length(sheet.vertical$DATA)

# models <- unique(sheet.vertical$MODELO)
# series <- list()

# for (name in models) {
#   num <- c()
#   for (i in 1:sheet.vertical.length) {
#     if (sheet.vertical$MODELO[i] == name) {
#       num <- c(num,sheet.vertical$`EMPLACAMENTO NUMERO`[i])
#     }
#   }
#   num[is.na(num)] <- 0 
#   series[[name]] <- ts(rev(num), frequency=12, start=c(2015,1))
# }

# for (key in names(series)) {
#   plot(series[[key]], ylab=key)
# }

brands <- unique(sheet.vertical$MARCA)
series <- list()

for (name in brands) {
  vals <- list()
  for (i in 1:sheet.vertical.length) {
    if (sheet.vertical$MARCA[i] == name) {
      date <- as.character(sheet.vertical$DATA[i])
      val <- sheet.vertical$`EMPLACAMENTO NUMERO`[i]
      if (is.na(val)) {
        val <- 0
      }

      if (date %in% names(vals)) {
        vals[[date]] <- vals[[date]] + val
      } else {
        vals[[date]] <- val
      }
    }
  } 
  series[[name]] <- ts(rev(values.num(vals)), frequency=12, start=c(2015,1))
}

# Full series
# for (key in names(series)) {
#   plot(series[[key]], ylab=key)
# }

# Relative trend
for (key in names(series)) {
  plot(decompose(series[[key]])$trend, ylab=key, 
    ylim=c(min(series[[key]]),max(series[[key]])))
}