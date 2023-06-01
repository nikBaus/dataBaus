## Task 2


#Loading the required libraries

library(tidyr)
library(data.table)
library(ggplot2)

#Loading the data

data <- read.csv(file.choose())
head(data)
str(data)
setDT(data)
data[, YEARMONTH := year(DATE)*100 + month(DATE)]
head(data)

## Defining the measure calculation over time

measureoverT <- data[ , .(totalsales = sum(TOT_SALES),
                         nCustomers = uniqueN(LYLTY_CARD_NBR),
                         nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                         nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                         avgpricePerUn = sum(TOT_SALES)/sum(PROD_QTY)),
                         by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]
measureoverT

FullobsStores <- unique(measureoverT[, .N, STORE_NBR][N == 12, STORE_NBR])
FullobsStores
preTrialMeasures <- measureoverT[YEARMONTH < 201902 & STORE_NBR %in% FullobsStores, ]
preTrialMeasures

# Function for calculating correlation
calculateCor <- function(inputTable, metricCol, storeComp) {
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corrMeasure = numeric())
  
  storeNos <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNos){
    calcMeasure = data.table("Store1"= storeComp, "Store2"= i, "corrMeasure" = 
                               cor(inputTable[STORE_NBR == storeComp, eval(metricCol)],
                                   inputTable[STORE_NBR == i, eval(metricCol)]))
    
    calcCorrTable <- rbind(calcCorrTable, calcMeasure)
    
  
  }
  print(calcCorrTable)
}


##Function for calculating the 

Magnitude Distance

calculateMagDist <- function(inputTable, metricCol, storeComp){
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(),
                             measure = numeric())
  
  storeNos <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNos){
    calcMeasure = data.table("Store1"= storeComp, "Store2"= i, "YEARMONTH" =  
                               inputTable[STORE_NBR == storeComp, YEARMONTH], "measure" =
                                   abs(inputTable[STORE_NBR == storeComp, eval(metricCol)] -
                                         inputTable[STORE_NBR == i, eval(metricCol)]))
    
    calcDistTable <- rbind(calcDistTable, calcMeasure)
    }

minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = c("Store1", "YEARMONTH")]
distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))

distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]

finalDistTable <- distTable[, .(magMeasure = mean(magnitudeMeasure)),  by = .(Store1, Store2)]

return(finalDistTable)
}

trial_store <- 88
corr_nSales <- calculateCor(preTrialMeasures, quote(totalsales), trial_store)
corr_nCustomers <- calculateCor(preTrialMeasures, quote(nCustomers), trial_store)


magnitude_nSales <- calculateMagDist(preTrialMeasures, quote(totalsales), trial_store)
magnitude_nCustomers <- calculateMagDist(preTrialMeasures, quote(nCustomers), trial_store)

corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))[, scoreNsales := corrMeasure * corr_weight + magMeasure
                                                                                 * (1-corr_weight)]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))[, scoreNCust := corrMeasure * corr_weight + magMeasure
                                                                                 * (1-corr_weight)]

score_control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_control[, finalControlScore := scoreNsales * 0.5 + scoreNCust * 0.5]

control_store <- score_control[Store1 == trial_store, ][order(-finalControlScore)][2, Store2]
control_store


measureovertimeSales <- measureoverT

pastSales <- measureovertimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other Stores"))
                                  ][, totalsales := mean(totalsales), by = c("YEARMONTH", "Store_type")
                                    ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
                                                                          "%Y-%m-%d")][YEARMONTH < 201903, ]
pastSales


ggplot(pastSales, aes(TransactionMonth, totalsales, color= Store_type)) +
  geom_line() +
  labs(x = "Month of Operation", y = "Total Sales", title = ("Total Sales by Month"))

measureovertimeCusts <- measureoverT

pastCustomers <- measureovertimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other Stores"))
                                    ][, numberCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                     ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
                                "%Y-%m-%d")][YEARMONTH < 201903, ]
pastCustomers

ggplot(pastCustomers, aes(TransactionMonth, numberCusts, color= Store_type)) +
  geom_line() +
  labs(x = "Month of Operation", y = "Total Number of Customers", title = ("Total Customers by Month"))



scalingFactorforControlStores <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, 
                                  sum(totalsales)] / preTrialMeasures[STORE_NBR == control_store & 
                                                                        YEARMONTH < 201902, sum(totalsales)]



measureovertimeSales <- measureoverT

scaledcontrolsales <- measureovertimeSales[STORE_NBR == control_store, ][ , controlSales := totalsales * scalingFactorforControlStores]

percentageDiff <- merge(scaledcontrolsales[, c("YEARMONTH", "controlSales")],
                        measureoverT[STORE_NBR == trial_store, c("totalsales", "YEARMONTH")], by = "YEARMONTH")[, 
                        percentageDiff := abs(controlSales - totalsales)/controlSales]

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

percentageDiff[,tValue := (percentageDiff - 0)/stdDev][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
                               "%Y-%m-%d")][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth,tValue)]
qt(0.95, 7)


measureovertimeSales <- measureoverT

pastSales <- measureovertimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other Stores"))
][, totalsales := mean(totalsales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
                                "%Y-%m-%d")][Store_type %in% c("Trial", "Control") ]

pastSales_controls95 <- pastSales[Store_type == "Control", ][, totalsales := totalsales * (1 + stdDev * 2)][
                                                              , Store_type := "Control 95th % confidence"]

pastSales_controls5 <- pastSales[Store_type == "Control", ][, totalsales := totalsales * (1 - stdDev * 2)][
                                                               , Store_type := "Control 5th % confidence"]

trialAssessment <- rbind(pastSales, pastSales_controls95, pastSales_controls5)

ggplot(trialAssessment, aes(TransactionMonth, totalsales, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
            ymin = 0, ymax = Inf, color = NULL), show.legend = F) +
  geom_line() +
  labs(x = "Month of Operation", y = "Total Sales", title = "Total sales by month")



scalingFactorforControlCusts <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, 
                                                  sum(nCustomers)] / preTrialMeasures[STORE_NBR == control_store & 
                                                                                        YEARMONTH < 201902, sum(nCustomers)]



measureovertimeCusts <- measureoverT

scaledcontrolCustomers <- measureovertimeCusts[STORE_NBR == control_store, ][ , controlCustomers := nCustomers * 
                                              scalingFactorforControlCusts][, Store_type := ifelse(STORE_NBR == trial_store, "Trial", 
                                                                           ifelse(STORE_NBR == control_store, "Control", "Other Stores"))]
              

percentageDiff <- merge(scaledcontrolCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureoverT[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")], by = "YEARMONTH")[, 
                        percentageDiff := abs(controlCustomers - nCustomers)/controlCustomers]

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

percentageDiff[,tValue := (percentageDiff - 0)/stdDev][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
                                                                                     "%Y-%m-%d")][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth,tValue)]
qt(0.95, 7)

pastCustomers <- measureovertimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                      ][Store_type %in% c("Trial", "Control"), ]

pastCustomers_controls95 <- pastCustomers[Store_type == "Control", ][, nCusts := nCusts * (1 + stdDev * 2)][
  , Store_type := "Control 95th % confidence"]

pastCustomers_controls5 <- pastCustomers[Store_type == "Control", ][,  nCusts := nCusts * (1 - stdDev * 2)][
  , Store_type := "Control 5th % confidence"]

trialAssessment <- rbind(pastCustomers, pastCustomers_controls95, pastCustomers_controls5)

ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
                ymin = 0, ymax = Inf, color = NULL), show.legend = F) +
  geom_line() +
  labs(x = "Month of Operation", y = "Total Customers", title = "Total number of customers by month")

