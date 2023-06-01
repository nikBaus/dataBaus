
#installing and loading necessary libraries

library(ggmosaic)
library(readr)
library(ggforce)


#importing the data sets 

txndata <- read.csv(file.choose())
customerdata <- read.csv(file.choose())
head(customerdata)
head(txndata)

txndata$DATE <- as.Date(txndata$DATE, origin ="1899-12-30")

str(txndata)

head(txndata)
tail(txndata)
summary(txndata$PROD_NAME)

setDT(txndata)

#Data cleaning 
txndata[, .N, PROD_NAME]

prodWords <- data.table(unlist(strsplit(unique(txndata[,PROD_NAME]), " ")))
setnames(prodWords, 'words')

# Removing odd digits and special characters
prodWords <- prodWords[grepl("\\d", words) == F, ] ##for digits
prodWords <- prodWords[grepl("[:alpha:]", words), ] ## for special characters

prodWords[, .N, words][order(N, decreasing = T)]

prodWords

#Products that are not chips are removed

txndata[, SALSA := grepl("salsa", tolower(PROD_NAME))]
txndata <- txndata[SALSA == F, ][, SALSA := NULL]
summary(txndata)

#Checking for outlier transactions and removing them

txndata[PROD_QTY == 200, ]
txndata[LYLTY_CARD_NBR == 226000, ]

txndata <- txndata[LYLTY_CARD_NBR != 226000, ]

summary(txndata)

txndata[, .N, DATE]
txndata[, "DATE"][order(format(as.Date(DATE), "%m%d"))[decreasing = T]]

#creating the date sequence

new_date <- data.table(seq(as.Date("2018-07-01"), 
                           as.Date("2019-06-30"),"days"))
setnames(new_date, "DATE")


new_date

txnbd <- merge(new_date, txndata[, .N, DATE], all.x = T)

txnbd

#setting theme

theme_set(theme_bw(10))
theme_update(plot.title = element_text(hjust = 0.5))


#creating visuals(Transactions by day)

base_plot <- ggplot(data=txnbd, aes(x = DATE, y = N))
base_plot + geom_line(colour = "Red") +
  labs(x = "Day", y = "Number of Transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle=90,vjust=0.5)) +
  coord_cartesian(ylim=c(500,900))

# Zooming out December period

ggplot(data=txnbd[month(DATE) == 12], aes(x = DATE, y = N)) + 
  geom_line(colour = "Red") +
  labs(x = "Day", y = "Number of Transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle=90,vjust=0.5)) +
  coord_cartesian(ylim=c(500,900))

# Pack size analysis

txndata[, PACK_SIZE := parse_number(PROD_NAME)]
 
txndata[, .N, PACK_SIZE][order(PACK_SIZE)]

txndata

#Histogram

ggplot(data=txndata[, PROD_NAME, PACK_SIZE], aes(x= PACK_SIZE)) +
  geom_histogram(binwidth =10, fill= "Green")

hist(txndata[, PACK_SIZE])

#Checking out the brands

txndata$brand <- str_extract(txndata$PROD_NAME, "(\\w+)")

txndata[, .N, brand][order(N, decreasing = T)]

txndata[brand == "Dorito", brand := "Doritos"]
txndata[brand == "RRD", brand := "Red"]
txndata[brand == "Smith", brand := "Smiths"]
txndata[brand == "WW", brand := "Woolworths"]
txndata[brand == "GrmWves", brand := "Grain"]
txndata[brand == "NCC", brand := "Natural"]
txndata[brand == "Infzns", brand := "Infuzions"]


head(customerdata)
str(customerdata)
setDT(customerdata)
summary(customerdata)

#merging the transaction and purchase behaviour data together

Cdata <- merge(txndata, customerdata, all.x = T)
txndata

Cdata[is.null(LIFESTAGE), .N]

Cdata[is.null(PREMIUM_CUSTOMER), .N]

fwrite(Cdata, paste0("C:/Users/user/Desktop/R Programs", "QUantiumWork.csv"))

Cdata


sales <- Cdata[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]



p <- ggplot(data = sales) +
    geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                    fill = PREMIUM_CUSTOMER)) +
    labs(x = "Lifestage", y = "Premium customer", title = "Sales by categories") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 
 #### Plot and label with proportion of sales

  p + geom_text(data = ggplot_build(p)$data[[1]],
                aes(x = (xmin + xmax)/2 , y =
                 (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))

  
  customers <- Cdata[,.(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]

  
  p <- ggplot(data = customers) +
    geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                    fill = PREMIUM_CUSTOMER)) +
    labs(x = "Lifestage", y = "Premium customer", title = "Customers' Proportion") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  #### Plot and label with proportion of sales
  
  p + geom_text(data = ggplot_build(p)$data[[1]],
                aes(x = (xmin + xmax)/2 , y =
                      (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))
  
  
  #### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
  average_units <- Cdata[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)),
                    .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
  
  #### Create plot
 
   ggplot(data = average_units, aes(weight = AVG, x = LIFESTAGE, fill =
                                PREMIUM_CUSTOMER)) +
    geom_bar(position = position_dodge()) +
    labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  
   average_price <- Cdata[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)),
                          .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
   
   #### Create plot
   
   ggplot(data = average_price, aes(weight = AVG, x = LIFESTAGE, fill =
                                      PREMIUM_CUSTOMER)) +
     geom_bar(position = position_dodge()) +
     labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
   
   pricePerUnit <- Cdata[, price := TOT_SALES/PROD_QTY]
   t.test(Cdata[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
               & PREMIUM_CUSTOMER == "Mainstream", price]
          , Cdata[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
                 & PREMIUM_CUSTOMER != "Mainstream", price]
          , alternative = "greater")
   
  
   
  