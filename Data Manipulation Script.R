library(readxl)
library(writexl)
library(lubridate)
library(zoo)
library(tidyverse)

#Data Manipulation#
###################

#Reading in Data
monthly_df = read_excel("Monthly Data.xlsx")
pwm_df = read_excel("Pre-Weaning Mortality.xlsx")
prices_df = read_excel("Weaned Pig Prices.xlsx")
exports_df = read_excel("Book1.xlsx")
temps_df = read_excel("Temps.xlsx")

#Splitting Date and Adding Quarters
temps_df  = temps_df %>%
  mutate(date = as.Date(date), 
         Day = day(date), Month = month(date), Year = year(date)) %>%
  mutate(Quarter = case_when((Month ==01 | Month==02 | Month==03) ~"Q1",
                             (Month ==04 | Month==05 | Month==06) ~"Q2",
                             (Month ==07 | Month==08 | Month==09) ~"Q3",
                             (Month ==10 | Month==11 | Month==12) ~"Q4"))

#Getting Max & Min Temp for each Month and Avg Temp & S.D. for Month
temp_sum_df  = temps_df %>%
  group_by(Month) %>%
  summarise(max_temp = max(max_temp_f), min_temp = min(min_temp_f), avg_max = mean(max_temp_f), avg_min = mean(min_temp_f), avg_temp = mean(avg_feel), sd_avg_temp = sd(avg_feel))

#Merging
temps_df  = merge(temps_df, temp_sum_df,by="Month")

#Creating Z-Scores
temps_df = temps_df %>%
  mutate(dmin_zscore = (min_temp_f-avg_temp)/sd_avg_temp)

temps_df = temps_df %>%
  mutate(dmax_zscore = (max_temp_f-avg_temp)/sd_avg_temp)

temps_df = temps_df %>%
  mutate(min_zscore = (min_temp_f-avg_min)/sd_avg_temp)

temps_df = temps_df %>%
  mutate(max_zscore = (max_temp_f-avg_max)/sd_avg_temp)

temps_df = temps_df %>%
  mutate(sum_zscore = abs(min_zscore) + abs(max_zscore)) 

#Creating Variables for Extreme Days
temps_df = temps_df %>%
  mutate(extreme_day = case_when((Month == 1 | Month == 2 | Month == 3 | Month == 11 | Month == 12  ~ ifelse(min_zscore<=-1,1,0)),
                                 (Month == 5 | Month == 6 | Month == 7 | Month == 8 ~ ifelse(max_zscore>=1,1,0)),
                                 (Month == 4 | Month == 9 | Month == 10 ~ ifelse(max_zscore>=2 | min_zscore<=-2,1,0))))
         
temps_df = temps_df %>%
  mutate(hot_cold = ifelse(max_temp_f > 100 | min_temp_f < 0,1,0))

ext_temps_df  = temps_df %>%
  group_by(Month, Quarter, Year) %>%
  summarise(temp_zscore = mean(sum_zscore), extreme_days = sum(extreme_day), hot_cold = sum(hot_cold), num_days = length(Month))%>%
  group_by(Quarter, Year) %>%
  summarise(temp_zscore = max(temp_zscore), extreme_days = sum(extreme_days), hot_cold = sum(hot_cold), hc_percent = sum(hot_cold)/sum(num_days))



#Adding Quarters
monthly_df = monthly_df %>%
  mutate(Quarter = case_when((Month ==1 | Month==2 | Month==3) ~"Q1",
                             (Month ==4 | Month==5 | Month==6) ~"Q2",
                             (Month ==7 | Month==8 | Month==9) ~"Q3",
                             (Month ==10 | Month==11 | Month==12) ~"Q4"))

#Getting Averages for Each Quarter by Year
monthly_df = monthly_df%>%
  group_by(Year,Quarter) %>%
  summarise(litter_rate = mean(`Litter Rate`), feed_price_ratio = mean(`Feed Price Ratio`), Unemployment = mean(`Unemployment Rate`))


#Splitting Date's from Week Data into Separate Columns
prices_df = prices_df %>%
  mutate(Year = year(Date), 
         Month = month(Date), 
         Day = day(Date))

#Adding Quarterly Variable to the Week Data
prices_df = prices_df %>%
  mutate(Quarter = case_when((Month ==1 | Month==2 | Month==3) ~"Q1",
                             (Month ==4 | Month==5 | Month==6) ~"Q2",
                             (Month ==7 | Month==8 | Month==9) ~"Q3",
                             (Month ==10 | Month==11 | Month==12) ~"Q4"))

#Getting Weighted Average of Weaned Pig Prices for Each Quarter by Year
prices_df = prices_df %>%
  group_by(Year,Quarter) %>%
  summarise(wean_price = mean(`Wtd Avg`))

#Merging Data
lr_df = merge(pwm_df,monthly_df, by=c("Year","Quarter"))
lr_df = merge(lr_df,prices_df, by=c("Year","Quarter"))
lr_df = merge(lr_df,exports_df, by=c("Year","Quarter"))
lr_df = merge(lr_df, ext_temps_df, by=c("Year","Quarter"))

lr_df$Date = paste0(lr_df$Year,"-", lr_df$Quarter)

#Adding Trend Variable
lr_df = lr_df %>%
  mutate(Trend = row_number())

#Creating Lagged Dependent Variable
lr_df = lr_df%>%
  mutate(lagged_litter_rate = lag(lr_df$litter_rate, n=1))

#Renaming Exports
lr_df = lr_df %>%
  rename(exports = `U.S. Pork  Exports (fr/ch/fz , $)`)

#Reordering
order = c("Trend","Date","Year","Quarter","litter_rate","lagged_litter_rate","Pre-weaning mortality","wean_price","feed_price_ratio","Unemployment","exports","temp_zscore","extreme_days", "hot_cold", "hc_percent")

lr_df = lr_df %>%
  dplyr::select(order)

#Converting Unemployment to decimal
lr_df$Unemployment = lr_df$Unemployment/100

#Renaming Pwm
lr_df = lr_df %>%
  rename(pwm = `Pre-weaning mortality`)

write_xlsx(lr_df,"lr_df.xlsx")