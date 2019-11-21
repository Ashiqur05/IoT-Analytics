# Load Packages-------------------------------------------------
library(RMySQL)
library(dplyr)
library(lubridate)
library(VIM)        #Aids visualization and imputing of missing values
library(funModeling)
library(ggplot2)
library(reshape2)
library(caret)      #R modeling workhorse & ggplot2
library(tidyverse)  #Package for tidying datalibrary(magrittr)   #Enables piping
library(GGally)

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
newDF <- bind_rows(yr_2007, yr_2008, yr_2009,yr_2010)
#newDF$Date <- format(as.Date(newDF$Date, format = "%Y-%m-%d"), "%d-%m-%Y")


# ------------------------------------newDF--------------------------------
summary(newDF)
str(newDF)
head(newDF)
tail(newDF)
dim(newDF)
#----------------------------------------------------------------------------
  
## Combine Date and Time attribute values in a new attribute column
newDF<-mutate(newDF, DateTime = paste(newDF$Date,newDF$Time))



head(newDF)
tail(newDF)
dim(newDF)


## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)
tail(newDF)
dim(newDF)

##Rename submeter

newDF<-newDF%>% dplyr::rename(Kitchen_stuff=Sub_metering_1, Laundry=Sub_metering_2, w_heater_AC=Sub_metering_3) 

## Convert DateTime from POSIXlt to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")

class(newDF$DateTime)
tz(newDF$DateTime)
head(newDF$DateTime)

head(newDF)
tail(newDF)
dim(newDF)
## Add the time zone----------
attr(newDF$DateTime, "tzone") <- "Europe/Paris"
class(newDF$DateTime)
tz(newDF$DateTime)

## Inspect the data types-----------
str(newDF)
head(newDF)
tail(newDF)
dim(newDF)


#-check range of time covered by data set
range(newDF$DateTime)


## Create "year" "quarter" "month" "week" "weekday" "day" "hour" and "minute" attribute with lubridate



newDF$year <- year(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$quarter <- quarter(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)


MonthLst <- c('Jan', 'Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
              'Oct', 'Nov','Dec', 'Jan')

WkdayLst <- c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri')

WkLst <- c('Sun','Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat', 'Sun' )

WkndList <- c('Sat', 'Sun', 'Mon')


# Assess missing values
aggr_plot <- aggr(newDF, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(newDF),cex.axis=.7,
                  gap=3, ylab=c("Histogram of missing data","Pattern"), digits=2)
newDF_NA <- newDF[rowSums(is.na(newDF))>0,]
newDF <- na.omit(newDF)
sum(is.na(newDF))

# Add feature representing remaining active energy consumed every minute (watt hour)
newDF <- newDF %>%
  mutate(Engy_remain=(Global_active_power*1000/60)-
           `Kitchen_stuff` - `Laundry` - `w_heater_AC`)
head(newDF)
as_tibble(newDF)
str(newDF)

# Create tidy tibble
newDF_tidy <- newDF %>%
  gather(Meter, Watt_hr,  `Kitchen_stuff`,`Laundry`,`w_heater_AC`,`Engy_remain`)

newDF_tidy %>% as_tibble(newDF_tidy)
is_tibble(newDF_tidy)

newDF_tidy$Meter <- factor(newDF_tidy$Meter)
newDF_tidy %>% as_tibble(newDF_tidy)

newDF_tidy$id<-NULL
newDF_tidy$Date<-NULL
newDF_tidy$Time<-NULL
newDF_tidy$year<-NULL
newDF_tidy$month<-NULL
newDF_tidy$quarter<-NULL
newDF_tidy$week<-NULL
newDF_tidy$day<-NULL
newDF_tidy$hour<-NULL
newDF_tidy$minute<-NULL

glimpse(newDF_tidy)
tail(newDF_tidy)

house_pwrMtrs <- select(newDF, DateTime, `Kitchen_stuff`, `Laundry`, `w_heater_AC`, `Engy_remain`) %>%
  group_by(year(DateTime), day(DateTime), month(DateTime), hour(DateTime), minute(DateTime))

# Exploratory Data Analysis
# Proportional and Line Plots across sub-metered zones---
###- Year-Proportional Plot of total energy across sub-metered zones

# Year_Proportional Plot
Year_Proportional<-newDF_tidy %>%
  group_by(year(DateTime), Meter) %>% 
  summarise(sum=sum(Watt_hr)) %>% 
  ggplot(aes(x=factor(`year(DateTime)`), sum, group=Meter,fill=Meter)) +
  labs(x='Year', y='Proportion of Energy Usage') +
  ggtitle('Proportion of Total Yearly Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black') 
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))
Year_Proportional

##-Year_Line Plot
Year_line<-newDF_tidy %>%
  group_by(year(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), sum, group=Meter, colour=Meter)) +
  labs(x='Year', y='kWh') +
  ggtitle('Total Yearly Energy Consumption') +
  geom_line(size=1)
Year_line

##-Year Bar Char
Year_bar<-newDF_tidy %>%
  group_by(year(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr/1000),3)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), y=sum)) +
  labs(x='Year', y='kWh') +
  ggtitle('Total Energy Useage by Year') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')
Year_bar

#Quarter bar  plot
Quarter_bar<-newDF_tidy %>%

  filter(year(DateTime)<2010) %>% #some missing value in 2010
  #mutate(quarter=quarter(DateTime)) %>%
  group_by(quarter(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr/1000),3)) %>%
  ggplot(aes(x=factor(`quarter(DateTime)`), y=sum)) +
  labs(x='Quarter of the Year', y='kWh') +
  ggtitle('Total Quarterly Energy Consumption') +
  geom_bar(position="dodge",stat='identity', aes(fill = Meter), color='black')
Quarter_bar


#Quarter Proportion plot
Quarter_Proportion<-newDF_tidy %>%
  
  group_by(quarter(DateTime), Meter) %>%
  #filter(quarter(DateTime)<3) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3, na.rm=TRUE) %>%
  ggplot(aes(x=factor(`quarter(DateTime)`), sum, group = Meter, fill=Meter)) +
  labs(x='Quarter of the Year', y='kWh') +
  ggtitle('Proportion of Total Quarterly Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')
Quarter_Proportion

###-Month- Proportional Plot
Month_Proportional<-newDF_tidy %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Month), sum, group=Meter,fill=Meter)) +
  labs(x='Month of the Year', y='Proportion of Monthly Energy Useage') +
  ggtitle('Proportion of Total Monthly Energy Useage') +
  geom_bar(stat='identity', position='fill', color='black')

Month_Proportional

###-Month- Line Plot
Month_line<-newDF_tidy %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Month), sum, group=Meter, colour=Meter)) +
  labs(x='Month of the Year', y='kWh') +
  ggtitle('Average Monthly Energy Useage') +
  geom_line(size=1) +
  geom_line()
Month_line

###-Month bar chart
Month_bar<-newDF_tidy %>%
 
  #filter(year(DateTime)<2010) %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(Month), y=sum)) +
  labs(x='Month of the Year', y='kWh') +
  ggtitle('Total Energy Useage by Month of the Year') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')
Month_bar

#Week of the year- line plot
week_line<-newDF_tidy %>%
 
  group_by(week(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(`week(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='Week of the Year', y='kWh') +
  ggtitle('Total Energy Usage by Week of the Year') +
  theme(axis.text.x = element_text(angle=90)) +
  geom_line(size=1) +
  geom_line()
week_line
#---------------------------------------------------############-----------------------------------------------
hour_line<-newDF_tidy %>%
  
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='houre of the day', y='kWh') +
  ggtitle('Total Energy Usage by houre of the day') +
  theme(axis.text.x = element_text(angle=90)) +
  geom_line(size=1) +
  geom_line()
hour_line

x<-filter(newDF,year(DateTime) == 2008 & month(DateTime) == 3 & day(DateTime) == 31)
hour_bar_h <- 
  x %>%
  group_by(hour = hour(DateTime)) %>% 
  summarise(w_heater_AC = sum(w_heater_AC)) %>% 
  ggplot(aes(x = hour, w_heater_AC)) +
  labs(x = '09/07/2008', y = 'wh') +
  ggtitle('energy consumption for each day') +
  geom_bar(stat = "identity",colour='red',fill='blue')
hour_bar_h

hour_bar_l <- 
  x %>%
  group_by(hour = hour(DateTime)) %>% 
  summarise(w_heater_AC = sum(Laundry)) %>% 
  ggplot(aes(x = hour, w_heater_AC)) +
  labs(x = '09/07/2008', y = 'wh') +
  ggtitle('energy consumption for each day') +
  geom_bar(stat = "identity",colour='red',fill='blue')
hour_bar_l

hour_bar_k <- 
  x %>%
  group_by(hour = hour(DateTime)) %>% 
  summarise(w_heater_AC = sum(Kitchen_stuff)) %>% 
  ggplot(aes(x = hour, w_heater_AC)) +
  labs(x = '09/07/2008', y = 'wh') +
  ggtitle('energy consumption for each day') +
  geom_bar(stat = "identity",colour='red',fill='blue')
hour_bar_k

hour_line <- 
  x %>%
  ggplot(aes(DateTime)) +
  labs(x = '09/07/2008', y = 'wh') +
  ggtitle('energy consumption for each day') +
  theme(axis.text.x = element_text(angle=90)) +
  geom_line(aes(y=w_heater_AC)) +
  geom_line()
hour_line

y<-filter(x,hour(DateTime) == 0 | hour(DateTime) == 2 | hour(DateTime) == 3)

hour_line_3 <- 
  y %>%
  ggplot(aes(DateTime, w_heater_AC)) +
  labs(x = '09/07/2008', y = 'wh') +
  ggtitle('energy consumption for each day') +
  geom_line()
hour_line_3

hour_line_4 <- 
  y %>%
  ggplot(aes(DateTime, Kitchen_stuff)) +
  labs(x = '09/07/2008', y = 'wh') +
  ggtitle('energy consumption for each day') +
  geom_line()
hour_line_4


hour_line_min <- 
  x %>%
  filter(hour(DateTime) == c(18:24)) %>%
  #ggplot(aes(hour(DateTime), w_heater_AC))
  #filter(Meter == 'w_heater_AC') %>% 
  #group_by(hour = hour(DateTime)) 
  ggplot(aes(hour(DateTime), w_heater_AC)) +
  labs(x = '09/07/2008', y = 'wh') +
  ggtitle('energy consumption for each day') +
  geom_line()
hour_line_min


ww <- newDF_tidy %>%
  filter(week(DateTime) == c(18:25)) %>%
  filter(Meter == 'w_heater_AC') %>% 
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000))
ww

Specific_day<-newDF %>%
  filter(year(DateTime) == 2008 & month(DateTime) == 7 & day(DateTime) == 9) %>% 

  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`day(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='Day of the Month', y='Avg Watt Hour Useage') +
  ggtitle('Average Daily Watt Hour Useage') +
  geom_line(size=1) +
  geom_line()
day_line



#-----------------------------------------------#####################-----------------------------------------

#Week of the year- bar plot
week_bar<-newDF_tidy %>%
  #filter(year(DateTime)<2006) %>%
  group_by(week(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`week(DateTime)`), y=sum)) +
  labs(x='Week of the Year', y='kWh') +
  ggtitle('Total Energy Usage by Week of the Year') +
  theme(axis.text.x = element_text(angle=90)) +
  geom_bar(stat='identity', aes(fill=Meter), colour='black')
week_bar

### Day of the Month- Line Plot
#Not informative
day_line<-newDF_tidy %>%
  group_by(day(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`day(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='Day of the Month', y='Avg Watt Hour Useage') +
  ggtitle('Average Daily Watt Hour Useage') +
  geom_line(size=1) +
  geom_line()
day_line

###-Day of Week- Porportional Plot
D_W_portion<-newDF_tidy %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), sum, group=Meter,fill=Meter)) +
  labs(x='Day of the Week', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Total Daily Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')
D_W_portion


###-Day of Week- Line Plot
D_W_line<-newDF_tidy %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), sum, group=Meter, colour=Meter)) +
  labs(x='Day of the Week', y='kWh') +
  ggtitle('Average Daily Energy Consumption') +
  geom_line(size=1) +
  geom_line()
D_W_line

###-Day of Week bar chart
D_W_bar<-newDF_tidy %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), y=sum)) +
  labs(x='Day of the Week', y='kWh') +
  ggtitle('Total Energy Useage by Day of Week') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')
D_W_bar

#Weekend bar chart
Weekend_bar <-newDF_tidy %>%
  mutate(weekend=lubridate::wday(DateTime, label=TRUE, abbr=FALSE)) %>%
  filter(weekend==c('Saturday','Sunday')) %>%
  group_by(weekend, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(weekend), y=sum, group=Meter,fill=Meter)) +
  labs(x='Weekend Day', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Energy Consumption by Weekend Day') +
  geom_bar(stat='identity', position='fill', color='black')
Weekend_bar


###-Hour of the Day- Proportional Plot
Hour_day<-newDF_tidy %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), sum, group=Meter,fill=Meter)) +
  labs(x='Hour of the Day', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Hourly Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')
Hour_day

###-Hour of the Day-Line Plot
Hour_day_line<-newDF_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=round(mean(Watt_hr), 3)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='Hour of the Day', y='Wh') +
  ggtitle('Average Hourly Energy Consumption') +
  geom_line(size=1) +
  geom_line()
Hour_day_line



#Hour of day bar chart
Hour_day_bar<-newDF_tidy %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), y=sum)) +
  labs(x='Hour of the Day', y='kWh') +
  ggtitle('Total Energy Useage by Hour of the Day') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')
Hour_day_bar

# Correlation plot
ggcorr(newDF) +
  ggtitle('Correlation Plot of Energy Consumption Dataset')

#Winter
#-Filter and plot data for weeks 1-8
#-------------need to think----
winter_plot<-newDF_tidy %>%
  filter(week(DateTime) == c(1:8)) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), y=sum)) +
  labs(x='Day of the Week', y='kWh') +
  ylim(0,200) +
  ggtitle('Total Energy Usage by Day for Weeks of \nHigh Consumption in Winter Months') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))
winter_plot

#Summer
#-Filter and plot data for weeks 18-25
summer_plot<-newDF_tidy %>%
  filter(week(DateTime) == c(18:25)) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), y=sum)) +
  labs(x='Day of the Week', y='kWh') +
  #ylim(0,85) +
  ggtitle('Total Energy Usage by Day for Weeks of \nHigh Consumptionin Summer Months') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))
summer_plot

sum(is.na(newDF_tidy$Watt_hr))

#-Subset data for weeks 1-8 and assign to variable w
w <- newDF_tidy %>%
  filter(week(DateTime) == c(1:8)) %>%
  filter(Meter == 'w_heater_AC') %>% 
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000))
w

#-Subset data for weeks 18-25 and assign to variable ww
ww <- newDF_tidy %>%
  filter(week(DateTime) == c(18:25)) %>%
  filter(Meter == 'w_heater_AC') %>% 
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000))
ww

#-Overlay line plots of the two 8-week time periods
ggplot(w) +
  labs(x='Day of the Week', y='kWh') +
  ylim(0,65) +
  ggtitle('Total Energy Usage on Submeter 3 for High\n Consumption Period in Winter and Summer Months') +
  geom_line(aes(x=Day, y=sum, group=1,colour='winter')) +
  geom_line(data = ww, aes(x=Day, y=sum, group=1, color='summer')) +
  scale_colour_manual(values=c('winter'='blue', 'summer'='red')) +
  labs(colour='Season') +
  guides(colour=guide_legend(reverse=TRUE)) +
  theme(panel.border=element_rect(colour='black', fill=NA))+
  theme(text = element_text(size = 14))

#-Subset data by quarter and summarise total usage across the 3 submeters
housePWR_qtr <- newDF %>%
  group_by(year(DateTime), quarter(DateTime)) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000), 3),
            Laundry =round(sum(`Laundry`/1000), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000), 3))
housePWR_qtr
housePWR_mnth <- newDF %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000), 3),
            Laundry=round(sum(`Laundry`/1000), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000), 3))

#-Look at top several rows of new monthly data set
head(housePWR_mnth)
--------------------------------------------------------------
  #-Subset Year
  housePWR_yr <- newDF %>%

  #filter(year(DateTime)<2010) %>%
  group_by(year(DateTime)) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000, na.rm=TRUE), 3),
            Laundry=round(sum(`Laundry`/1000, na.rm=TRUE), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))
housePWR_yr

#-Subset Semester
housePWR_semstr <- newDF %>%

  group_by(year(DateTime),semester(DateTime)) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000, na.rm=TRUE), 3),
            Laundry=round(sum(`Laundry`/1000, na.rm=TRUE), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))
housePWR_semstr

#-Subset Week of year
housePWR_wkofYr <- newDF %>%
  #filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(year(DateTime),week(DateTime)) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000, na.rm=TRUE), 3),
            Laundry=round(sum(`Laundry`/1000, na.rm=TRUE), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))
housePWR_wkofYr

#-Subset by day of week
housePWR_dofWk <- newDF %>%
  #filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(week(DateTime),DofWk) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000, na.rm=TRUE), 3),
            Laundry=round(sum(`Laundry`/1000, na.rm=TRUE), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime),
            last_DateTime = last(DateTime))
housePWR_dofWk

#-Subset hour of day
housePWR_hofDay <- newDF %>%

  #filter((minute(DateTime) %% 5) == 0) %>%
  group_by(wday(DateTime), hour(DateTime)) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000, na.rm=TRUE), 3),
            Laundry=round(sum(`Laundry`/1000, na.rm=TRUE), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))
housePWR_hofDay

#-Subset by Weekends
housePWR_wknd <- newDF %>%
  #filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(Wknd=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  filter(Wknd == c('Sat', 'Sun')) %>%
  group_by(Wknd, hour(DateTime)) %>%
  summarise(Kitchen_stuff=round(sum(`Kitchen_stuff`/1000, na.rm=TRUE), 3),
            Laundry=round(sum(`Laundry`/1000, na.rm=TRUE), 3),
            w_heater_AC=round(sum(`w_heater_AC`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime)) %>%
  arrange(desc(Wknd))
housePWR_wknd



#Convert to Time Series and Plot---------------------------------------------------------------------------
#-Create quarterly time series 
housePWR_qtrTS <- ts(housePWR_qtr[,3:5],
                     frequency=4,
                     start=c(2007,1),
                     end=c(2010,3))

#-Plot quarterly time series
plot(housePWR_qtrTS, 
     plot.type='s',
     col=c('red', 'green', 'blue'),
     main='Total Quarterly kWh Consumption (2007-2010)',
     xlab='Year', ylab = 'kWh')
minor.tick(nx=4)
#-Create legend
b <- c('Kitchen_stuff', 'Laundry', ' w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#-Create monthly time series
housePWR_mnthTS <- ts(housePWR_mnth[,3:5],
                      frequency = 12,
                      start=c(2007,1),
                      end=c(2010,11))

#-Plot monthly time series
plot(housePWR_mnthTS, 
     plot.type='s',
     xlim=c(2007, 2011),
     col=c('red', 'green', 'blue'),
     main='Total Monthly kWh Consumption',
     xlab='Year/Month', ylab = 'kWh')
minor.tick(nx=12)
#-Create legend
b <- c('Kitchen_stuff', 'Laundry', 'w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

#Semester_TS
housePWR_semstrTS <- ts(housePWR_semstr[,3:5], 
                        frequency = 2, start=c(2007,1), 
                        end=c(2010,1))

plot(housePWR_semstrTS, plot.type='s', #xaxt='n',
     #xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Energy Consumption by Semester',
     xlab='Year', ylab = 'kWh')
b <- c('Kitchen_stuff', 'Laundry', 'w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

# Year_TS
housePWR_yrTS <- ts(housePWR_yr[,2:4], frequency = 1, start=c(2007))
plot(housePWR_yrTS, plot.type='s', #xaxt='n',
     #xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Yearly kWh Consumption (2007-2010)',
     xlab='Year', ylab = 'kWh')
b <- c('Kitchen_stuff', 'Laundry', 'w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')




# Week of the year_TS
housePWR_wkofYrTS <- ts(housePWR_wkofYr[,3:5], frequency =53, start=c(2006,51), end=c(2010,47))
plot(housePWR_wkofYrTS, plot.type='s', #xaxt='n',
     #xaxp = c(1, 13, 12),
     col=c('red', 'green', 'blue'),
     xlab ='Year', ylab = 'kWh',
     #ylim=c(0,120),
     main='Total Energy Consumption by Week of the Year')
#axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12,13), labels=MonthLst)
minor.tick(nx=52)
b <- c('Kitchen_stuff', 'Laundry', 'w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#Day of Week_TS
housePWR_dofWkTS <- ts(housePWR_dofWk[,3:5], frequency =7, start=c(1,1))
plot(housePWR_dofWkTS, plot.type='s', #xaxt='n',
     col=c('red', 'green', 'blue'),
     xlab ='Week of Year', ylab = 'kWh',
     xlim = c(1,53) , ylim=c(0,75),
     main='Total Energy Consumption by Day of Week')
minor.tick(nx=7)
b <- c('Kitchen_stuff', 'Laundry', 'w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Hour of Day_TS
housePWR_hofDayTS <- ts(housePWR_hofDay[,3:5], frequency=24, start=c(0,0))
plot(housePWR_hofDayTS, plot.type='s',
     #xaxp = c(0,48, 47),
     col=c('red', 'green', 'blue'),
     xlab='Day', ylab = 'kWh',
     main='Total kWh Consumption by Hour of the Day')
minor.tick(nx=24)
b <- c('Kitchen_stuff', 'Laundry', 'w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

# Hour_Weekend_TS
housePWR_wkndTS <- ts(housePWR_wknd[,3:5], frequency=24, end(1,23))
plot(housePWR_wkndTS, plot.type='s', xaxt='n',
    # xaxp = c(0, 3,2),
     xlim=c(1,3),
     col=c('red', 'green', 'blue'),
     xlab='Weekend Day', ylab = 'kWh',
     ylim=c(0,90),
     main='Total Weekend Energy Consumption by Hour')
axis(side = 1, at = c(1,2,3), labels = WkndList)
minor.tick(nx=24)
b <- c('Kitchen_stuff', 'Laundry', 'w_heater_AC')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

