## This script serves as a guideline to clean diary entries and Telegram
## User can modify accordingly based on the format and structure of different input data
## This script infers the dates of bedtime from participant's Wake Time Diary's Selected Date
## This script infers the dates of naps from participant's Bedtime Diary's Selected Date

library(tidyverse)
library(lubridate)

################################### User's input needed ###################################

rm(list = ls())
svfile = 1 ## Do you want to save the output?

## Modify working directory
Dir <- "./"

setwd(Dir) ## Set working directory

dataWTraw <- read.csv(paste0(Dir,"ExampleData/", "WT.csv"))
dataBTraw <- read.csv(paste0(Dir,"ExampleData/", "BT.csv"))
dataTelraw <- read.csv(paste0(Dir,"ExampleData/", "TELE.csv"))

################################### Wake Time Diary Cleaning ###################################

####### Clean up Wake Time Diary
## Fill empty cells with Subject Code
dataWT <- dataWTraw %>% mutate(Subj = ifelse(Subj == "", NA, Subj)) %>% fill(Subj)

## Convert "WTSelectedDate" to date
dataWT <- dataWT %>%
  mutate(WTSelectedDate = dmy(WTSelectedDate))

## Convert AM/PM to 24Hr time
dataWT <- dataWT %>% 
  mutate(bt = paste0(Bedtime, " ", BedtimeAMPM),
         wt = paste0(WakeTime, " ", WakeTimeAMPM)) %>%
  mutate(bt = format(strptime(bt, "%I:%M %p"), "%H:%M"),
         wt = format(strptime(wt, "%I:%M %p"), "%H:%M"))

## Infer the dates for bedtime
## Arbitrarily set cutoff at 1700, any bedtime > 1700, participant selected WT Date will minus 1 day
dataWT <- dataWT %>%
  mutate(btt = hm(bt),wtt = hm(wt)) %>% ## convert character to time 
  mutate(Datebt = ifelse(btt > hms::as_hms(61200), WTSelectedDate - days(1), WTSelectedDate)) %>%
  mutate(Datebt = as_date(Datebt))

## Create date-time object for bedtime and wake time
dataWT <- dataWT %>% 
  mutate(sleep = ymd_hm(paste0(Datebt, " ", bt), tz = "Asia/Singapore")) %>%
  mutate(wake = ymd_hm(paste0(WTSelectedDate, " ", wt), tz = "Asia/Singapore"))

## Retain selected columns
dataWT <- dataWT %>% select(Subj,WTSelectedDate,sleep,wake)


################################### BedTime Diary Cleaning ###################################
## Remove unwanted columns
dataBT <- dataBTraw %>% select(Subj, BTSelectedDate, matches(".+Nap[1-5]{1}$"))

## Fill empty cells with Subject Code
dataBT <- dataBT %>% mutate(Subj = ifelse(Subj == "", NA, Subj)) %>% fill(Subj)

## Convert "BTSelectedDate" to date
dataBT <- dataBT %>%
  mutate(BTSelectedDate = dmy(BTSelectedDate))

## Convert AM/PM to 24Hr time
dataBT <- dataBT %>% mutate(across(matches(".+Nap[1-5]{1}$"), function(x)
  format(strptime(x, "%I:%M %p"), "%H:%M")
))

## make nap df into long form
dataBT <- dataBT %>%
  select(-where(function(x) sum(is.na(x)) == length(x))) %>% ## Delete empty NAPs columns
  pivot_longer(matches(".+Nap[1-5]{1}$"), names_to = c(".value", "NapOrder"), names_pattern = "(.+Nap)([[:digit:]])")

## Remove days where there is no 2nd/3rd/4th naps and so on
dataBT <- dataBT %>%
  filter(!is.na(StartNap)&!is.na(EndNap))

## convert character to time (period)
dataBT <- dataBT %>% mutate( 
  across(c(StartNap, EndNap), function(x) 
    hm(x),
    .names = "{.col}_tt")  
)

## Infer the dates for naps
## For past midnight nap, add one day to the date, assume an arbitrary offset of 05:00 in the morning
dataBT <- dataBT %>% mutate(
  NapStartDate = ifelse(StartNap_tt < hms::as_hms(18000), BTSelectedDate + days(1), BTSelectedDate),
  NapEndDate = ifelse(EndNap_tt < hms::as_hms(18000), BTSelectedDate + days(1), BTSelectedDate)
) %>% mutate(
  NapStartDate = as_date(NapStartDate),
  NapEndDate = as_date(NapEndDate)
)

## Convert nap timings into Date Time object
dataBT <- dataBT %>% mutate(
  sleep = ymd_hm(paste0(NapStartDate, " ", StartNap), tz = "Asia/Singapore"),
  wake = ymd_hm(paste0(NapEndDate, " ", EndNap), tz = "Asia/Singapore")
)

## Remove unwanted columns 
dataBT <- dataBT %>% select(Subj, BTSelectedDate, sleep, wake)

################################### Telegram Cleaning ###################################
## Convert Telegram timings into Date Time object
## Convert nap timings into Date Time object
dataTel <- dataTelraw %>% mutate(
  sleep = dmy_hm(paste0(Date_Sleep_telegram, " ", Sleep_telegram), tz = "Asia/Singapore"),
  wake = dmy_hm(paste0(Date_Wake_telegram, " ", Wake_telegram), tz = "Asia/Singapore")
)

## Remove unwanted columns 
dataTel <- dataTel %>% select(Subj, sleep, wake)

################################### Save files ###################################

## Save the cleaned Diary
if (svfile == 1) {
  write.table(dataWT, paste0(Dir, "ExampleData/", "WT_Timestamp.csv"), sep = ",", row.names = FALSE)
  write.table(dataBT, paste0(Dir, "ExampleData/", "BT_Timestamp.csv"), sep = ",", row.names = FALSE)
  write.table(dataTel, paste0(Dir, "ExampleData/", "TELE_Timestamp.csv"), sep = ",", row.names = FALSE)
}
