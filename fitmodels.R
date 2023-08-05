library(StMoMo); library(demography); library(tidyverse)

genderList <- c("male")
countryList <- c("BLR","AUT", "BEL", "FRATNP","IRL","ITA","JPN", "USA", "GBRTENW","CHE")

MorData <- lapply(countryList, function(x) hmd.mx(country = x, username = "salvatory@aims.ac.tz", password = "Salva=0606"))
DataStMoMo <- lapply(1:length(MorData), function(i) lapply(genderList, function(x) StMoMoData(MorData[[i]], x)))

agesFit <- 50:89; yearsFit <- 1960:1990
nAg <- length(agesFit); nYr <- length(yearsFit)

forRatesHor <- list()

for ( c in 1:length(countryList)) { for (g in 1:length(genderList))
  
{
  # Define the models
  
  LC <- lc(); APC <- apc(); CBD <- cbd(link = "log"); M7 <- m7(link = "log")
  RH <- rh(approxConst = TRUE); PLAT =  plat()
  models <- list(LC, RH, APC, CBD, M7, PLAT)
  modelNames <- c("LC","RH", "APC", "CBD", "M7", "PLAT")
  
  # fit and predict
  
h <- 26
maxYear <- 2016
yearFitEval <- max(yearsFit)
forRatesFun <- function(yearFitEval)
    
{
    
    h <- maxYear - yearFitEval
    
    forModels <- lapply(models, function(x) forecast(fit(x, data = DataStMoMo[[c]][[g]], ages.fit = agesFit, years.fit = yearsFit[1]:yearFitEval), h = h))
    
    forRates <-  lapply(forModels, function(x) {pivot_longer(bind_cols(age = agesFit, as.data.frame(x$rates)),
                                                             cols = 2:(h+1), names_to = "year", values_to = "rate") %>%
        mutate(year = as.numeric(year), h = year - min(year) + 1)})
    
       forRates <- bind_rows(lapply(1:length(forRates), function(x) mutate(forRates[[x]], model = modelNames[x], country = countryList[c], gender = genderList[g])))
}
  
  forRatesHor[[c]] <- dplyr::bind_rows(lapply(1990:2015, forRatesFun))
  
}
}

predDF <- bind_rows(forRatesHor)
write.csv(predDF, file = "predDF.csv")

