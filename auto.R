library(base)
library(readxl)
library(tseries)
library(optparse)

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

get.models <- function(map.type, type) {
  models = c()
  for (i in 1:length(map.type$models)) {
    if (map.type$types[i] == type) {
      models <- c(models,map.type$models[i])
    }
  }
  return(models)
}

plot.series <- function(series, mode) {
  for (key in names(series)) {
    png(paste('./plots/',mode,'-series-',key,'.png',sep=''))
    plot(series[[key]]$ts, ylab=key)
    dev.off()
  }
}

plot.trend <- function(series, mode) {
  for (key in names(series)) {
    png(paste('./plots/',mode,'-trend-',key,'.png',sep=''))
    plot(decompose(series[[key]]$ts)$trend, ylab=key, 
      ylim=c(min(series[[key]]$ts),max(series[[key]]$ts)))
    dev.off()
  }
}

plot.avg <- function(avg) {
  plot(values.num(avg), ylab='average')
  lines(1:100,rep.int(7500,100),col='red')
}

'%not in%' <- function (x, table){
  is.na(match(x, table, nomatch=NA_integer_))
}

# ################
# Argument parsing
# ################
option_list <- list(
  make_option(c('-m','--mode'), type="character", default=NULL, 
    help="Execution type {'brand','type'}", metavar="character"),
  make_option(c('-r','--printlv'), type="integer", default=0, 
    help="Print level {1:popuar vs luxury, 2:risky vs solid}", 
    metavar="number"),
  make_option(c('-l','--plotlv'), type="integer", default=0, 
    help="Plot level {1:series, 2:trend, 4:average}", 
    metavar="number")
);

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);
opt$printlv <- intToBits(opt$printlv)
opt$plotlv <- intToBits(opt$plotlv)
# ################

# ##################################
# Status list defined by trend graph
# ##################################
status.stable <- 'STABLE'
status.increasing <- 'INCREASING'
status.decreasing <- 'DECREASING'

status <- list(GM=status.stable, HYUNDAI=status.stable, 
  FORD=status.decreasing, RENAULT=status.decreasing, 
  TOYOTA=status.increasing, JEEP=status.increasing,
  FIAT=status.decreasing, HONDA=status.decreasing,
  VOLKSWAGEN=status.decreasing, NISSAN=status.increasing,
  PEUGEOT=status.increasing, CITROEN=status.increasing, 
  KIA=status.increasing, `MERCEDES BENZ`=status.increasing, 
  AUDI=status.stable, MITSUBISHI=status.decreasing, 
  BMW=status.increasing, `LAND ROVER`=status.increasing)
# ##################################

# ##########################
# Reading the data from file
# ##########################
file <- 'Auto Data.xlsx'

sheet.input <- read_excel(file, sheet=1)
sheet.vertical <- read_excel(file, sheet=2)
sheet.type <- read_excel(file, sheet=3)

# Some workarounds to deal with malformed data on `dados verticalizados` sheet
sheet.vertical.length <- length(sheet.vertical$DATA)
sheet.vertical$MARCA[sheet.vertical$MARCA == 'Jeep'] <- 'JEEP'
sheet.vertical$MARCA[sheet.vertical$MARCA == 'MerCEDES BENZ'] <- 
  'MERCEDES BENZ'

# Some workarounds to deal with malformed data on `tipos de veiculo` sheet
sheet.type.length <- length(sheet.type[1][[1]])+1
sheet.type.models <- c(names(sheet.type[1]),sheet.type[1][[1]])
sheet.type.types <- c(names(sheet.type[2]),sheet.type[2][[1]])
sheet.type.types[is.na(sheet.type.types)] <- 'DESCONHECIDO'
# ##########################

brands <- unique(sheet.vertical$MARCA)
types <- unique(sheet.type.types)
series <- list()

if (opt$mode == 'brand') {
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
    series[[name]]$ts <- ts(rev(values.num(vals)), 
      frequency=12, start=c(2015,1))
    series[[name]]$models <- models
  }
} else if (opt$mode == 'type') {
  map.type <- NULL
  map.type$models <- sheet.type.models
  map.type$types <- sheet.type.types

  for (name in types) {
    vals <- list()

    for (i in 1:sheet.vertical.length) {
      if (sheet.vertical$MODELO[i] %in% get.models(map.type,name)) {
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

    series[[name]] <- NULL 
    series[[name]]$ts <- ts(rev(values.num(vals)), 
      frequency=12, start=c(2015,1))
  }
}

avg <- list()
if(opt$plotlv[3] == 1 || (opt$mode == 'brand' && opt$printlv[1] == 1)){
  for (key in names(series)) {
    avg[[key]] <- mean(series[[key]]$ts)
  }
  avg.ordered <- avg[order(-unlist(avg))]
}

if(opt$plotlv[1] == 1){
  plot.series(series, opt$mode)
}

if (opt$plotlv[2] == 1) {
  plot.trend(series, opt$mode)
}

if (opt$plotlv[3] == 1) {
  plot.avg(avg)
}

# #####################################################################
# Split brands in popular (bigger numbers) and luxury (smaller numbers)
# #####################################################################
if(opt$mode == 'brand' && opt$printlv[1] == 1) {
  popular <- list()
  luxury <- list()

  for (i in 1:sheet.type.length) {
    model <- sheet.type.models[i]
    type <- sheet.type.types[i]

    value <- avg[[get.brand(series,model)]]
    if (value > 7500) {
      popular[[get.brand(series,model)]] <- 
        c(popular[[get.brand(series,model)]], type)
    } else {
      luxury[[get.brand(series,model)]] <- 
        c(luxury[[get.brand(series,model)]], type)
    }
  }

  popular.types <- unique(values.char(popular))
  luxury.types <- unique(values.char(luxury))
  diff.pl <- setdiff(popular.types, luxury.types)
  cat('Popular vehicle types:',popular.types,'\n')
  cat('Luxury vehicle types:',luxury.types,'\n')
  cat('Popular-exclusive vehicle types:',diff.pl,'\n')
}
# #####################################################################

# ###################################################################
# Split brands in solid investiment (either increasing or stable) 
# and risky investiments (decreasing or unstable/unpredictable trend)
# ###################################################################
if(opt$mode == 'brand' && opt$printlv[2] == 1) {
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
  diff.sr <- setdiff(solid.types, risky.types)
  diff.rs <- setdiff(risky.types, solid.types)
  cat('Solid investiment vehicle types:',diff.sr,'\n')
  cat('Risky investiment vehicle types:',diff.rs,'\n')
}
# ###################################################################