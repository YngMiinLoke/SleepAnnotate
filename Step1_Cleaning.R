## This script serves as a guideline to clean diary entries and Telegram
## User can modify accordingly based on the format and structure of different input data
## This script assumes that the participants are nocturnal-sleepers, i.e. not shift workers 
## and infer the dates of naps, bedtime and wake time

## This script infers the dates of bedtime from participant's Wake Time Diary's Selected Date
## This script infers the dates of naps from participant's Bedtime Diary's Selected Date

## 13MAR2022 
## Nocturnal Sleep - Added whether participant slept that night, added sleep latency

library(tidyverse)
library(lubridate)

################################### User's input needed ###################################

rm(list = ls())
svfile = 1 ## Do you want to save the output?

## Modify working directory
Dir <- "./"

setwd(Dir) ## Set working directory

outDate <- Sys.Date()

subjrange <- "S001-S002"

dataWTraw <- read.csv(paste0(Dir,"ExampleData/", "WakeTimeDiary.csv"))
dataBTraw <- read.csv(paste0(Dir,"ExampleData/", "BedtimeDiary.csv"))
dataTelraw <- read.csv(paste0(Dir,"ExampleData/", "Telegram.csv"))

################################### Wake Time Diary Cleaning ###################################

####### Clean up Wake Time Diary
## Fill empty cells with Subject Code
dataWT <- dataWTraw %>% mutate(Subject = ifelse(Subject == "", NA, Subject)) %>% fill(Subject)

## Convert "WTSelectedDate" to date
dataWT <- dataWT %>%
  mutate(WTSelectedDate = dmy(WTSelectedDate))

## Convert AM/PM to 24Hr time
dataWT <- dataWT %>%
  mutate(bt = format(strptime(Bedtime, "%I:%M %p"), "%H:%M"),
         wt = format(strptime(WakeTime, "%I:%M %p"), "%H:%M"))

## Infer the dates for bedtime
## Arbitrarily set cutoff at 1700, any bedtime > 1700, subtract 1 day from participant selected WT Date
dataWT <- dataWT %>%
  mutate(btt = hm(bt),wtt = hm(wt)) %>% ## convert character to time 
  mutate(Datebt = ifelse(btt > hms::as_hms(61200), WTSelectedDate - days(1), WTSelectedDate)) %>%
  mutate(Datebt = as_date(Datebt))

## Create date-time object for bedtime and wake time
dataWT <- dataWT %>% 
  mutate(sleep = ymd_hm(paste0(Datebt, " ", bt), tz = "Asia/Singapore")) %>%
  mutate(wake = ymd_hm(paste0(WTSelectedDate, " ", wt), tz = "Asia/Singapore"))

## Retain selected columns
dataWT <- dataWT %>% select(Subject,WTSelectedDate, AnySleep, sleep, wake, SleepLatency)

################################### BedTime Diary Cleaning ###################################
## Remove unwanted columns
dataBT <- dataBTraw %>% select(Subject, BTSelectedDate, matches(".+Nap[1-5]{1}$"), ClassMode, FirstClassStart, Tardiness)

## Fill empty cells with Subject Code
dataBT <- dataBT %>% mutate(Subject = ifelse(Subject == "", NA, Subject)) %>% fill(Subject)

## Convert "BTSelectedDate" to date
dataBT <- dataBT %>%
  mutate(BTSelectedDate = dmy(BTSelectedDate))

################################### BedTime Diary - First Class Start - Cleaning ###################################
## Process Class Start Time
## Convert AM/PM to 24Hr time 
dataClass <- dataBT %>% mutate(
  FirstClassStart = format(strptime(FirstClassStart, "%I:%M %p"), "%H:%M")
)

## Combine BTSelectedDate and First Class Start Time and then convert it into a Date-Time Object
dataClass <- dataClass %>% mutate(
  FirstClassStart = ymd_hm(paste0(BTSelectedDate, " ", FirstClassStart), tz = "Asia/Singapore")
)

## Remove unwanted columns 
dataClass <- dataClass %>% select(Subject, ClassMode, FirstClassStart, Tardiness)

## Remove FirstClassStart == NA
dataClass <- dataClass %>% filter(!is.na(FirstClassStart))

################################### BedTime Diary - Naps - Cleaning ###################################
## Process Naps
dataNap <- dataBT %>% select(Subject, BTSelectedDate, matches(".+Nap[1-5]{1}$"))

## Convert AM/PM to 24Hr time
dataNap <- dataNap %>% mutate(across(matches(".+Nap[1-5]{1}$"), function(x)
  format(strptime(x, "%I:%M %p"), "%H:%M")
))

## make nap df into long form
dataNap <- dataNap %>%
  select(-where(function(x) sum(is.na(x)) == length(x))) %>% ## Delete empty NAPs columns
  pivot_longer(matches(".+Nap[1-5]{1}$"), names_to = c(".value", "NapOrder"), names_pattern = "(.+Nap)([[:digit:]])")

## Remove days where there is no 2nd/3rd/4th naps and so on
dataNap <- dataNap %>%
  filter(!is.na(StartNap)&!is.na(EndNap))

## convert character to time (period)
dataNap <- dataNap %>% mutate( 
  across(c(StartNap, EndNap), function(x) 
    hm(x),
    .names = "{.col}_tt")  
)

## Infer the dates for naps
## For past midnight nap, add one day to the date, assume an arbitrary offset of 05:00 in the morning
dataNap <- dataNap %>% mutate(
  NapStartDate = ifelse(StartNap_tt < hms::as_hms(18000), BTSelectedDate + days(1), BTSelectedDate),
  NapEndDate = ifelse(EndNap_tt < hms::as_hms(18000), BTSelectedDate + days(1), BTSelectedDate)
) %>% mutate(
  NapStartDate = as_date(NapStartDate),
  NapEndDate = as_date(NapEndDate)
)

## Convert nap timings into Date Time object
dataNap <- dataNap %>% mutate(
  sleep = ymd_hm(paste0(NapStartDate, " ", StartNap), tz = "Asia/Singapore"),
  wake = ymd_hm(paste0(NapEndDate, " ", EndNap), tz = "Asia/Singapore")
)

## Remove unwanted columns 
dataNap <- dataNap %>% select(Subject, BTSelectedDate, sleep, wake)

################################### Telegram Cleaning ###################################
## Convert Telegram timings into Date Time object
## Convert nap timings into Date Time object
dataTel <- dataTelraw %>% mutate(
  sleep = dmy_hm(paste0(Date_Sleep_telegram, " ", Sleep_telegram), tz = "Asia/Singapore"),
  wake = dmy_hm(paste0(Date_Wake_telegram, " ", Wake_telegram), tz = "Asia/Singapore")
)

## Remove unwanted columns 
dataTel <- dataTel %>% select(Subject, sleep, wake)

################################### Save files ###################################

## Save the cleaned Diary
if (svfile == 1) {
  write.table(dataWT, paste0(Dir, "ExampleData/", "NocSleep_Timestamp_", subjrange, "_", outDate, ".csv"), sep = ",", row.names = FALSE)
  write.table(dataClass, paste0(Dir, "ExampleData/", "Class_Timestamp_", subjrange, "_", outDate, ".csv"), sep = ",", row.names = FALSE)
  write.table(dataNap, paste0(Dir, "ExampleData/", "Nap_Timestamp_", subjrange, "_", outDate, ".csv"), sep = ",", row.names = FALSE)
  write.table(dataTel, paste0(Dir, "ExampleData/", "Telegram_Timestamp_", subjrange, "_", outDate, ".csv"), sep = ",", row.names = FALSE)
}
