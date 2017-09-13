library(base)
library(readxl)
library(tseries)

values.num <- function(list) {
  as.numeric(paste(unlist(list)))
}

values.char <- function(list) {
  as.character(paste(unlist(list)))
}

get.brand <- function(series, model) {
  brand <- NULL
  for (name in names(series)) {
    if (model %in% series[[name]]$models) {
      brand <- name
    }
  }
  return(brand)
}

'%not in%' <- function (x, table){
  is.na(match(x, table, nomatch=NA_integer_))
}

# ##################################
# Status list defined by trend graph
# ##################################
status.stable <- 'STABLE'
status.increasing <- 'INCREASING'
status.decreasing <- 'DECREASING'
status.undefined <- 'UNDEFINED'

status <- list(GM=status.stable, HYUNDAI=status.stable, 
  FORD=status.decreasing, RENAULT=status.decreasing, 
  TOYOTA=status.increasing, JEEP=status.undefined,
  FIAT=status.decreasing, HONDA=status.decreasing,
  VOLKSWAGEN=status.decreasing, NISSAN=status.increasing,
  PEUGEOT=status.increasing, CITROEN=status.increasing, 
  KIA=status.undefined, `MERCEDES BENZ`=status.undefined, 
  AUDI=status.stable, MITSUBISHI=status.decreasing, 
  BMW=status.increasing, `LAND ROVER`=status.increasing, 
  Jeep=status.undefined, `MerCEDES BENZ`=status.undefined)
# ##################################

file <- 'Auto Data.xlsx'

sheet.input <- read_excel(file, sheet=1)
sheet.vertical <- read_excel(file, sheet=2)
sheet.type <- read_excel(file, sheet=3)

sheet.vertical.length <- length(sheet.vertical$DATA)

# Some workarounds to deal with malformed data on `tipos de veiculo` sheet
sheet.type.length <- length(sheet.type[1][[1]])+1
sheet.type.models <- c(names(sheet.type[1]),sheet.type[1][[1]])
sheet.type.types <- c(names(sheet.type[2]),sheet.type[2][[1]])

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
  models <- c()

  for (i in 1:sheet.vertical.length) {
    if (sheet.vertical$MARCA[i] == name) {
      date <- as.character(sheet.vertical$DATA[i])
      model <- sheet.vertical$MODELO[i]
      val <- sheet.vertical$`EMPLACAMENTO NUMERO`[i]

      if (is.na(val)) {
        val <- 0
      }

      if (date %in% names(vals)) {
        vals[[date]] <- vals[[date]] + val
      } else {
        vals[[date]] <- val
      }

      if (model %not in% models){
        models <- c(models, model)
      }
    }
  }

  series[[name]] <- NULL 
  series[[name]]$ts <- ts(rev(values.num(vals)), frequency=12, start=c(2015,1))
  series[[name]]$models <- models
}

# ################
# Plot full series
# ################
# for (key in names(series)) {
#   plot(series[[key]], ylab=key)
# }
# ################

# ###################
# Plot relative trend
# ###################
# for (key in names(series)) {
#   plot(decompose(series[[key]])$trend, ylab=key, 
#     ylim=c(min(series[[key]]),max(series[[key]])))
# }
# ###################

# ###############
# Avg brand value
# ###############
# avg <- list()
# for (key in names(series)) {
#   avg[[key]] <- mean(series[[key]])
# }
# avg.ordered <- avg[order(-unlist(avg))]
# plot(values.num(avg.ordered))
# lines(1:100,rep.int(7500,100),col='red')
# ###############

# #################################################################
# Split brands in top (bigger numbers) and bottom (smaller numbers)
# #################################################################
# top <- list()
# bottom <- list()

# for (i in 1:sheet.type.length) {
#   model <- sheet.type.models[i]
#   type <- sheet.type.types[i]

#   value <- avg[[get.brand(series,model)]]
#   if (value > 7500) {
#     top[[get.brand(series,model)]] <- 
#       c(top[[get.brand(series,model)]], type)
#   } else {
#     bottom[[get.brand(series,model)]] <- 
#       c(bottom[[get.brand(series,model)]], type)
#   }
# }

# top.types <- unique(values.char(top))
# bottom.types <- unique(values.char(bottom))
# diff <- setdiff(top.types, bottom.types)
# #################################################################

# ###################################################################
# Split values in solid investiment (either increasing or stable) 
# and risky investiments (decreasing or unstable/unpredictable trend)
# ###################################################################
risky <- list()
solid <- list()

for (i in 1:sheet.type.length) {
  model <- sheet.type.models[i]
  type <- sheet.type.types[i]
  brand <- get.brand(series,model)

  if (status[[brand]] == status.increasing || 
    status[[brand]] == status.stable) {
    solid[[brand]] <- c(solid[[brand]], type)
  } else {
    risky[[brand]] <- c(risky[[brand]], type)
  }
}

solid.types <- unique(values.char(solid))
risky.types <- unique(values.char(risky))
diff <- setdiff(solid.types, risky.types)
# ###################################################################