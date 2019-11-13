# Load library
library(RMySQL)
library(dplyr)
library(lubridate)

# Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
#The 9 variables available in the data set are (as described in the UCI website):
  
  #Date: Date in format dd/mm/yyyy
#Time: time in format hh:mm:ss
#Global_active_power: household global minute-averaged active power (in kilowatt)
#Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#Voltage: minute-averaged voltage (in volt)
#Global_intensity: household global minute-averaged current intensity (in ampere)
#Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

# List the tables contained in the database 
dbListTables(con)
## Lists attributes contained in a table
dbListFields(con,'yr_2006')

## Use asterisk to specify all attributes for download
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

#Investigate each new data frame

# ------------------------------------yr_2006--------------------------------
summary(yr_2006)
str(yr_2006)
head(yr_2006)
tail(yr_2006)
#----------------------------------------------------------------------------

# ------------------------------------yr_2007--------------------------------
summary(yr_2007)
str(yr_2007)
head(yr_2007)
tail(yr_2007)
#----------------------------------------------------------------------------

# ------------------------------------yr_2008--------------------------------
summary(yr_2008)
str(yr_2008)
head(yr_2008)
tail(yr_2008)
#----------------------------------------------------------------------------

# ------------------------------------yr_2009--------------------------------
summary(yr_2009)
str(yr_2009)
head(yr_2009)
tail(yr_2009)
#----------------------------------------------------------------------------

# ------------------------------------yr_2010--------------------------------
summary(yr_2010)
str(yr_2010)
head(yr_2010)
tail(yr_2010)
#----------------------------------------------------------------------------
## Combine tables into one dataframe using dplyr
#create a primary data frame that ONLY includes the data frames that span an entire year.
newDF <- bind_rows(yr_2007, yr_2008, yr_2009)
#newDF$Date <- format(as.Date(newDF$Date, format = "%Y-%m-%d"), "%d-%m-%Y")


# ------------------------------------newDF--------------------------------
summary(newDF)
str(newDF)
head(newDF)
tail(newDF)
#----------------------------------------------------------------------------
  
## Combine Date and Time attribute values in a new attribute column
newDF<-mutate(newDF, DateTime = paste(newDF$Date,newDF$Time))



head(newDF)


## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)

## Convert DateTime from POSIXlt to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")

head(newDF)
## Add the time zone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(newDF)
head(newDF)
## Create "year" "quarter" "month" "week" "weekday" "day" "hour" and "minute" attribute with lubridate



newDF$year <- year(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$quarter <- quarter(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)


head(newDF)
