#MODEL FOR CAR PRICES BY PRECIOUS AJAYI

#Loading relevant libraries
library(data.table)
library(ggplot2)
library(janitor)
library(broom)
library(tidyverse)
library(ggforce)

#Loading the dataset
car <- read.csv("car_data.csv")

#Exploratory Data Analysis(EDA)
head(car)
str(car)
summary(car)
nrow(car)
setDT(car)

#Checking for missing values
mv = is.na(car)
colSums(mv)
#**there are no missing values in the dataframe**

#Working on the Model to extract the brand for easy analysis
car$Model
car$Brand <- substr(car$Model, 0,4)
car
car[Brand == "Maru", Brand := "Maruti"]
car[Brand == "Niss", Brand := "Nissan"]
car[Brand == "Toyo", Brand := "Toyota"]
car[Brand == "Merc", Brand := "Mercedez"]
car[Brand == "Hond", Brand := "Honda"]
car[Brand == "Mahi", Brand := "Mahindra"]
car[Brand == "Hyun", Brand := "Hyundai"]
car[Brand == "Rena", Brand := "Renault"]
car[Brand == "BMW3", Brand := "BMW"]
car[Brand == "BMWX", Brand := "BMW"]
car[Brand == "BMW5", Brand :="BMW"]
car[Brand == "Dats", Brand := "Datsun"]
car[Brand == "Skod", Brand := "Skoda"]
car[Brand == "Chev", Brand := "Chevrolet"]
car[Brand == "MGHE", Brand := "MGHECTOR"]
car[Brand == "Volk", Brand := "Volkswagen"]
car[Brand == "Ssan", Brand := "Ssangyong"]


#Exploring the Kilometers Driven column to check for outliers and assess the distribution
hist(car$Kilometers.Driven)
summary(car$Kilometers.Driven)
new_car <- car[Kilometers.Driven < 855881, ]
hist(new_car$Kilometers.Driven)

#Dependent Variable: Selling Price
#Independent variable: Brand, Year, Kilometers Driven, Car Condition,
#                     Owner, Fuel Type, Transmission.

#All independent variable must be mapped to fit multiple linear regression. Also,
#the regression assumption must be checked.

#Brand
str(new_car$Brand)
unique(new_car$Brand)
new_car[ ,MBrand := ifelse(Brand == "Maruti", 1,
                          ifelse(Brand == "Nissan",2,
                          ifelse(Brand == "Toyota",3,
                          ifelse(Brand == "Mercedez", 4,
                          ifelse(Brand == "Honda", 5,
                          ifelse(Brand == "Mahindra", 6,
                          ifelse(Brand == "Hyundai", 7,
                          ifelse(Brand == "Renault", 8,
                          ifelse(Brand == "BMW", 9,
                          ifelse(Brand == "Datsun", 10,
                          ifelse(Brand == "Skoda", 11,
                          ifelse(Brand == "Chevrolet", 12,
                          ifelse(Brand == "MGHECTOR", 13,
                          ifelse(Brand == "Volkswagen", 14,
                          ifelse(Brand == "Ssangyong", 15,
                          ifelse(Brand == "Tata", 16,
                          ifelse(Brand == "Fiat", 17,
                          ifelse(Brand == "Audi", 18, 
                          ifelse(Brand == "Ford",19, 20
                                 )))))))))))))))))))]
new_car[MBrand == 20, ]
hist(new_car$MBrand)
new_car
new_car[MBrand == 1, ]

plot(Selling.Price~MBrand, data = new_car)

#Year
hist(new_car$Year)
plot(Selling.Price~Year, data = new_car)
cor.test(new_car$Selling.Price, new_car$Year)

#Mileage
hist(new_car$Kilometers.Driven)
plot(Selling.Price~Kilometers.Driven, data = new_car)
cor.test(new_car$Selling.Price, new_car$Kilometers.Driven)

#Owner
new_car[ ,Gradeowner := ifelse(Owner == "First Owner", 1, ifelse(Owner == "Second Owner"
                                                             , 2, 3))]
hist(new_car$Gradeowner)
plot(Selling.Price~Gradeowner, data = new_car)

#Car Condition
str(new_car$Car.Condition)
hist(new_car$Car.Condition)
plot(Selling.Price~Car.Condition, data = new_car)

#Fuel Type
str(new_car$Fuel.Type)
unique(new_car$Fuel.Type)
new_car[ , MFuel := ifelse(Fuel.Type == "Petrol + CNG", 1,
                    ifelse(Fuel.Type == "Petrol", 2,
                    ifelse(Fuel.Type == "Diesel",3, 4
                           )))]
plot(Selling.Price~MFuel, data = new_car)

#Transmission
unique(new_car$Transmission)
new_car[, MTrans := ifelse(Transmission == "MANUAL", 1, 2)]
hist(new_car$MTrans)
plot(Selling.Price~MTrans, data = new_car)
cor.test(new_car$Selling.Price, new_car$MTrans)

#Checking for multicollinearity
library(ggcorrplot)
r_car <- new_car[,c('Selling.Price','Year','Kilometers.Driven','Gradeowner',
                    'Car.Condition','MTrans','MFuel','MBrand')]

corr = round(cor(r_car), 2)

ggcorrplot(corr, hc.order = T, type = "lower", lab = T)
ggcorrplot

#Splitting data for training and testing
library(caTools)
sample <- sample.split(new_car$Selling.Price, SplitRatio = 0.8)
train <- subset(new_car, sample == T)
test <- subset(new_car, sample == F)

train
dim(train)
dim(test)
nrow(new_car)

#Creating the multiple linear regression model
regression <- lm(Selling.Price~MBrand+Year+Gradeowner+Kilometers.Driven+Car.Condition
                 +MFuel+MTrans, data = train)
summary(regression)

#First regression indicates that Gradeowner is insignificant, therefore will be dropped

regression2 <- lm(Selling.Price~MBrand+Year+Kilometers.Driven+Car.Condition
                 +MFuel+MTrans, data = train)
summary(regression2)
hist(regression2$residuals)

#Testing the model
testing <- predict(regression2, newdata = test)
round(testing, 0)

plot(testing, test$Selling.Price, xlab = "Predicted", ylab = "Actual")
abline(a = 0, b=1, col = "red", lwd = 2)

result <- data.table(round(testing,0), test$Selling.Price)
result[, difference:= V1-V2]
result[, error := ((V2-V1)/V2) * 100]
result



rmse_val <- sqrt(mean(testing-test$Selling.Price)^2)
rmse_val

SSE = sum((testing-test$Selling.Price)^2)
SST = sum((testing-mean(test$Selling.Price))^2)
r2_test = 1 - SSE/SST
print(r2_test)
qstr(car)


