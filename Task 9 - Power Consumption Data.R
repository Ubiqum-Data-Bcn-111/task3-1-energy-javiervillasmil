install.packages("cowplot")
install.packages("dygraphs")
install.packages("xts")
install.packages("fpp2")
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)
library(cowplot)
library(dygraphs)
library(xts)
library(imputeTS)
library(forecast)
library(zoo)
library(fma)
library(expsmooth)
library(fpp2)


household_power_consumption <- read_delim("Desktop/Ubiqum/Task 09 - Define a Data Science Process/household_power_consumption.txt", ";", 
                                          escape_double = FALSE, 
                                          trim_ws = TRUE)

originalhpc <- read_delim("Desktop/Ubiqum/Task 09 - Define a Data Science Process/household_power_consumption.txt", ";", 
                          escape_double = FALSE, 
                          trim_ws = TRUE)

#join the data and time column as.POSIXct class. change the time zone so all time saving values are taken into account
household_power_consumption$DateTime <- as.POSIXct(paste(household_power_consumption$Date, household_power_consumption$Time), format="%d/%m/%Y %H:%M:%S", tz = "GMT")
#switch the order of columns
household_power_consumption <- household_power_consumption[,c(10,1:9)]
#change the $date column class to "date"
household_power_consumption$Date <- as.Date(household_power_consumption$Date, "%d/%m/%Y")

#inspect the data frame.
head(household_power_consumption)
summary(household_power_consumption)

#remove 2006 values
index2006 <- which(year(household_power_consumption$DateTime)== 2006) 
household_power_consumption <- household_power_consumption[-index2006,]
glimpse(household_power_consumption)

#count registers per each year
df1 <- count(household_power_consumption,year(DateTime))

#count NA's per each year
df2 <- household_power_consumption %>%
  filter(is.na(household_power_consumption$Global_active_power)) %>%
  count(year(DateTime))

df1 
df2
sum(df1$n)
sum(df2$n)

#add one hour for the Daylight Saving Time for each year
onehour <- hours(1)

###################################
###################################
#2007#
x2k7index <- which(household_power_consumption$DateTime >= as.POSIXct("2007-03-25 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2007-10-28 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k7index] <- household_power_consumption$DateTime[x2k7index] + onehour
#averaging the values#
x2k7newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2007-10-28 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2007-10-28 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k7overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2007-10-28 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2007-10-28 03:00:00",tz = "GMT"))
x2k7overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k7overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k7newvalues)
###################################
###################################
ds1 <- household_power_consumption %>%
  filter(year(DateTime)==2007)
View(ds1)


#2008#
x2k8index <- which(household_power_consumption$DateTime >= as.POSIXct("2008-03-30 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2008-10-26 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k8index] <- household_power_consumption$DateTime[x2k8index] + onehour
#averaging the values#
x2k8newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2008-10-26 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2008-10-26 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k8overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2008-10-26 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2008-10-26 03:00:00",tz = "GMT"))
x2k8overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k8overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k8newvalues)
###################################
###################################
ds2 <- household_power_consumption %>%
  filter(year(DateTime)==2008)
View(ds2)


#2009
x2k9index <- which(household_power_consumption$DateTime >= as.POSIXct("2009-03-29 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2009-10-25 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k9index] <- household_power_consumption$DateTime[x2k9index] + onehour
#averaging the values#
x2k9newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2009-10-25 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2009-10-25 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k9overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2009-10-25 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2009-10-25 03:00:00",tz = "GMT"))
x2k9overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k9overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k9newvalues)
###################################
###################################
ds3 <- household_power_consumption %>%
  filter(year(DateTime)==2009)
View(ds3)


#2010
x2k10index <- which(household_power_consumption$DateTime >= as.POSIXct("2010-03-28 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2010-10-31 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k10index] <- household_power_consumption$DateTime[x2k10index] + onehour
#averaging the values#
x2k10newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2010-10-31 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2010-10-31 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k10overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2010-10-31 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2010-10-31 03:00:00",tz = "GMT"))
x2k10overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k10overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k10newvalues)
###################################
###################################
ds4 <- household_power_consumption %>%
  filter(year(DateTime)==2010)
View(ds4)


household_power_consumption$DateTime <- force_tz(household_power_consumption$DateTime, "Europe/Paris")
statsNA(household_power_consumption$Global_active_power)

naindex <- which(is.na(household_power_consumption$Global_active_power)==TRUE)
str(naindex)

############################################################################################
Breaks <- c(0, which(diff(naindex) != 1), length(naindex)) 
listNA <- sapply(seq(length(Breaks) - 1), function(i) naindex[(Breaks[i] + 1):Breaks[i+1]]) 

i = 1
j = 1
vector1 <- c()
vector2 <- c()
vector3 <- c()

for (i in 1:length(listNA)){
  if (length(listNA[[i]]) <= 60){
    h <- length(listNA[[i]])
    for (j in 1:h){
      vector1 <- c(vector1,listNA[[i]][j])
    }
  } else if (length(listNA[[i]]) > 60 & length(listNA[[i]]) <= 1440){
    h <- length(listNA[[i]])
    for (j in 1:h){
      vector2 <- c(vector2,listNA[[i]][j])
      }
    } else {
      h <- length(listNA[[i]])
      for (j in 1:h){
        vector3 <- c(vector3,listNA[[i]][j])
        }
      }
    }

# FROM 1 to 60 Na's
vector1
# FROM 60 to 1440 Na's
vector2
# OVER 1440 Na's
vector3

##################################################################
#IMPUTE NA'S FOR GROUP VECTOR 1 - USING LAST OBSRVATION CARRIED FORWARD
for (i in 4:10){
  household_power_consumption[vector1,i] <- na.locf(unname(unlist(household_power_consumption[,i])))[vector1]
}
summary(household_power_consumption)

##################################################################
#IMPUTE NA'S FOR GROUP VECTOR 2 - AVERAGE OF THE WEEK BEFORE AND AFTER
for (i in 4:10){
  household_power_consumption[vector2,i] <- (household_power_consumption[vector2+10080,i] + 
                                            household_power_consumption[vector2+20160,i] +
                                            household_power_consumption[vector2-10080,i] +
                                            household_power_consumption[vector2-20160,i])/4
}
summary(household_power_consumption)
#IMPUTE NA'S FOR GROUP VECTOR 3  - AVERAGE OF THE WEEK BEFORE AND AFTER
for (i in 4:10){
    household_power_consumption[vector3,i] <- min(household_power_consumption[vector3+10080,i],
                                                  household_power_consumption[vector3-10080,i])
}


#household_power_consumption$Date <- NULL
#household_power_consumption$Time <- NULL
summary(household_power_consumption)
#calculate the energy consumed by other equipments
household_power_consumption$Other_Equipment <- apply(household_power_consumption[,c('Global_active_power', 'Sub_metering_1',"Sub_metering_2",'Sub_metering_3')], 1, function(x) { (x[1] * 1000 / 60) - x[2] - x[3] - x[4] } )
household_power_consumption$EnergyConsumed <- apply(household_power_consumption[,c('Global_active_power')], 1, function(x) { (x[1] * 1000) / 60})
#DATA CLEANED 

#FORECASTING
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################

#grouping by 15min
groupMINUTES <- household_power_consumption %>%
  mutate(dategroup = floor_date(DateTime,"15 minutes")) %>%
  group_by(dategroup) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupMINUTES <- groupMINUTES[,c(1,5:9)]

#Grouping by DAY - ENERGY
groupDAYS <- household_power_consumption %>%
  group_by(date(Date)) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupDAYS <- mutate_at(groupDAYS,funs(./1000), .vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))

#Grouping by WEEK - ENERGY
groupWEEK <- household_power_consumption %>%
  group_by(year(DateTime),month(DateTime),week(DateTime)) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupWEEK <- mutate_at(groupWEEK,funs(./1000), .vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))

#grouping by MONTH - ENERGY
groupMONTH <- household_power_consumption %>%
  group_by(year(DateTime),month(DateTime)) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
###arrange(year(DateTime),month(DateTime))#
groupMONTH <- mutate_at(groupMONTH,funs(./1000), .vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupMONTH <- groupMONTH[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed")]

#timeSeries
dailyTS <- ts(groupDAYS,frequency = 365.25, start = c(2007,1))
weeklyTS <- ts(groupWEEK,frequency = 52.18, start = c(2007,1))
monthlyTS <- ts(groupMONTH, frequency = 12, start = c(2007,1))

autoplot(monthlyTS)


#methods forecasting - MONTHLY
monthlyDC <- decompose(monthlyTS[,"EnergyConsumed"])
plot(monthlyDC)


monthlyTSLM <- tslm(monthlyTRAIN[,"EnergyConsumed"]~season+trend)
forecastTSLM <- forecast(monthlyTSLM)
autoplot(forecastTSLM) + autolayer(monthtlyTEST[,"EnergyConsumed"])

accuracy(forecastTSLM,monthtlyTEST[,"EnergyConsumed"])



monthlyARIMA <- auto.arima(monthlyTS[,"EnergyConsumed"],seasonal = TRUE)
forecastARIMA <- forecast(monthlyARIMA,h=12)
autoplot(forecastARIMA)



plot(forecastTSLM)



#TrainingSets
dailyTRAIN <- window(dailyTS,2007,2010)
weeklyTRAIN <- window(weeklyTS)
monthlyTRAIN <- window(monthlyTS,2007,c(2009,12))

#testSets
monthtlyTEST <- window(monthlyTS,c(2010,1))





tail(dailyTRAIN)
head(dailyTEST)


plot(dailyTS)

########################################
par(mfrow=c(2,2))
autoplot(dailyTS[,2:5] , facets = TRUE)
autoplot(dailyTS[,2:5], facets = FALSE)

autoplot(weeklyTS[,2:5] , facets = TRUE)
autoplot(weeklyTS[,2:5], facets = FALSE)

autoplot(monthlyTS[,2:5] , facets = TRUE)
autoplot(monthlyTS[,2:5], facets = FALSE)


ggseasonplot(weeklyTS[,3])
ggseasonplot(dailyTS[,3])

par(mfrow=c(1,1))

#SEASONAL PATTERNS
ggAcf(dailyTS[,1])
ggAcf(weeklyTS[,1])
pew <- ggAcf(monthlyTS[,1])

##########################################
dailymodel <- forecast(monthlyTS,h=12)

autoplot(dailymodel)


plot.ts(dailyTS)
plot.ts(weeklyTS)
plot.ts(monthlyTS)

autoplot(monthlyTS, facets = 2)




taylor <- msts(grupedDAYS, seasonal.periods=c(7,365.25))
taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit))

plot.ts(taylor)

View(groupMONTH)
summary(household_power_consumption)


any(is.na(household_power_consumption))


summary(household_power_consumption)
















household_power_consumption$DateTime[vector2]-weeks(1) != household_power_consumption$DateTime[vector2]-10080

household_power_consumption[190498,]

hours(household_power_consumption[190498,1]) == hours(baseday)

# Mean Imputation
plot(na.mean(tsdaily, option = "mean") - tsdaily$Global_active_power, ylim = c(-mean(tsdailySPLINE), mean(tsdaily$Global_active_power,na.rm = TRUE)), ylab = "Difference", main = "Mean")

mean((na.mean(tsdaily$Global_active_power, option = "mean") - tsdaily$Global_active_power)^2)


don=xts(x = tsdaily[,c("ActiveEnergyConsumed","Sub_metering_1","Sub_metering_2","Sub_metering_3")], order.by = tsdaily$DateTime)
dygraph(don) %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE,stackedGraph = FALSE,includeZero = TRUE) %>%
  dyAxis("y", label = "Active Energy (Watt/hour)",valueRange = c(0, 120)) %>%
  dySeries("ActiveEnergyConsumed", label = "Other Electrical Equipment") %>%
  dySeries("Sub_metering_1", label = "Kitchen") %>%
  dySeries("Sub_metering_2", label = "Laundry Room") %>%
  dySeries("Sub_metering_3", label = "Heating Devices") %>%
  dyRangeSelector()



plotNA.distribution(household_power_consumption$Global_active_power, colPoints = "steelblue",
                    colBackgroundMV = "indianred2", main = "Distribution of NAs",
                    xlab = "Time", ylab = "Value", pch = 20, cexPoints = 0.8,
                    col = "black")

x <- ts(household_power_consumption$Global_active_power, frequency = 12)
x.withoutNA <- na.kalman(x)





taylor <- msts(household_power_consumption, seasonal.periods=c(10080,525960))
taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit))










reviewtime <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2007-03-25 01:00:00",tz = "GMT") & DateTime <= as.POSIXct("2007-03-25 04:00:00",tz = "GMT"))
View(reviewtime)

reviewtime <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2007-10-28 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2007-10-28 03:00:00",tz = "GMT"))
view(reviewtime)


groupeddates <- (group_by(reviewtime,DateTime))
groupeddates

overlaping <- summarise_all(groupeddates, funs(mean))
overlaping


x2k7 <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2007-06-26 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2007-10-29 02:00:00",tz = "GMT"))

x2k7$DateTime <- x2k7$DateTime + onehour







mutate(household_power_consumption, Baka = replace(household_power_consumption$DateTime, household_power_consumption$DateTime >=  as.POSIXct("2007-06-26 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2007-10-29 02:00:00",tz = "GMT", 34234234)))

?replace()






which(leap_year(household_power_consumption$DateTime) == TRUE)

#missing valudes for DateTime column
#missingval <- which(is.na(household_power_consumption$DateTime) == TRUE)
#missingdf <- head(household_power_consumption[c(missingval),],n = 240)

#household_power_consumption$DateTime[c(missingval)] <- as.POSIXct(paste(household_power_consumption$Date[c(missingval)], household_power_consumption$Time[c(missingval)]), format="%d/%m/%Y %H:%M")

missingval <- which(is.na(household_power_consumption$Global_active_power) == TRUE)
missingdf <-  household_power_consumption[c(missingval),]

hist(year(missingdf$Date), breaks = 6)


# graph by month:
p <- ggplot(missingdf, aes(missingdf$DateTime, ..count..)) + 
  geom_histogram(bins = 365) +
  theme_bw() + xlab("2006") +
  scale_x_datetime(breaks = date_breaks("1 months"),
                   labels = date_format("%b"),
                   limits = c(as.POSIXct("2006-01-01"), 
                              as.POSIXct("2006-12-31")) )

o <- ggplot(missingdf, aes(missingdf$DateTime, ..count..)) + 
  geom_histogram(bins = 365) +
  theme_bw() + xlab("2007") +
  scale_x_datetime(breaks = date_breaks("1 month"),
                   labels = date_format("%b"),
                   limits = c(as.POSIXct("2007-01-01"), 
                              as.POSIXct("2007-12-31")) )

q <- ggplot(missingdf, aes(missingdf$DateTime, ..count..)) + 
  geom_histogram(bins = 365) +
  theme_bw() + xlab("2008") +
  scale_x_datetime(breaks = date_breaks("1 month"),
                   labels = date_format("%b"),
                   limits = c(as.POSIXct("2008-01-01"), 
                              as.POSIXct("2008-12-31")) )

r <- ggplot(missingdf, aes(missingdf$DateTime, ..count..)) + 
  geom_histogram(bins = 365) +
  theme_bw() + xlab("2009") +
  scale_x_datetime(breaks = date_breaks("1 month"),
                   labels = date_format("%b"),
                   limits = c(as.POSIXct("2009-01-01"), 
                              as.POSIXct("2009-12-31")) )

s <- ggplot(missingdf, aes(missingdf$DateTime, ..count..)) + 
  geom_histogram(bins = 365) +
  theme_bw() + xlab("2010") +
  scale_x_datetime(breaks = date_breaks("1 month"),
                   labels = date_format("%b"),
                   limits = c(as.POSIXct("2010-01-01"), 
                              as.POSIXct("2010-12-31")) )

plot_grid(p, o, q,r,s, labels = "AUTO")

      # GET THE FREQUENCY VALUES FOR THE GIVEN BINS #
#############################################################
get_hist <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}

options(max.print=999999)
pewpew <- get_hist(o)
#############################################################

# CHECK THE MISSING VALUES FROM 04/27 TO 04/30 2007 #
v <- ggplot(missingdf, aes(missingdf$DateTime, ..count..)) + 
  geom_histogram(bins = 31) +
  theme_bw() + xlab("2007") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   labels = date_format("%d"),
                   limits = c(as.POSIXct("2007-04-27"), 
                              as.POSIXct("2007-04-30")) )

#FOR THE PROJECT I WOULD LIKE TO PLOT A TIME SERIES FOR THE YEAR 2008, LESS MISSING VALUES.
x2008global <- household_power_consumption %>%
  select(DateTime, Global_active_power, ActiveEnergyConsumed, Sub_metering_1,Sub_metering_2,Sub_metering_3) %>%
  filter(year(DateTime)==2008)
  

x2008global[,c("ActiveEnergyConsumed","Sub_metering_1","Sub_metering_2","Sub_metering_3")]

don=xts(x = x2008global[,c("ActiveEnergyConsumed","Sub_metering_1","Sub_metering_2","Sub_metering_3")], order.by = x2008global$DateTime)
dygraph(don) %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE,stackedGraph = FALSE,includeZero = TRUE) %>%
  dyAxis("y", label = "Active Energy (Watt/hour)",valueRange = c(0, 120)) %>%
  dySeries("ActiveEnergyConsumed", label = "Other Electrical Equipment") %>%
  dySeries("Sub_metering_1", label = "Kitchen") %>%
  dySeries("Sub_metering_2", label = "Laundry Room") %>%
  dySeries("Sub_metering_3", label = "Heating Devices") %>%
  dyRangeSelector()




#END OF CODE END OF CODE END OF CODE END OF CODE END OF CODE END OF CODE END OF CODE END OF CODE END OF CODE END OF CODE 
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
don=xts(x = household_power_consumption$Global_active_power, order.by = household_power_consumption$DateTime)
dygraph(don) %>%
  dyRangeSelector()


#if (year(household_power_consumption$DateTime[vector3]) == 2007){

#yourdataframe <-cbind(yourdataframe,paste(yourdataframe$Date,yourdataframe$Time), stringsAsFactors=FALSE)
#colnames(yourdataframe)[10] <-"DateTime"
#yourdataframe <- yourdataframe[,c(ncol(yourdataframe), 1:(ncol(yourdataframe)-1))]
#head(yourdataframe)

#yourdataframe$DateTime <- strptime(yourdataframe$DateTime, "%d/%m/%Y %H:%M:%S")
#yourdataframe$Date <- as.Date(yourdataframe$Date, "%d/%m/%Y")
#str(yourdataframe)


#household_power_consumption[170784,1]
#household_power_consumption[170784+(525960),1]

#household_power_consumption[vector2+10080,1]
#household_power_consumption[vector2,1]


#collect1 <- data.frame(id = character(0), max1 = numeric(0), max2 = numeric(0))

#collect1$date1 <- household_power_consumption[vector2+20160,1]
#collect1$date2 <- household_power_consumption[vector2,4]


#test <- household_power_consumption %>%
 # group_by(month(DateTime),day(DateTime),hour(DateTime),minute(DateTime)) %>%
#  summarise(avg=mean(Global_active_power))



#household_power_consumption[vector3,i] <- household_power_consumption[vector3,1]
