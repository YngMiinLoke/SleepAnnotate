## This script annotate participant-reported (Diary/Telegram) sleep/wake timings into the figures of his/her activity counts 

library(tidyverse)
library(lubridate)
library(ggpubr)

rm(list = ls())

################################### User's input needed ###################################

svfile = 1 ## Do you want to save the output?

## Modify working directory
Dir <- "./"

## Modify output directory
outDir <- "./ExampleOutput/"

## Read in cleaned BT & WT Diary with timestamps from Step 1
dataBTOri <- read.csv(paste0(Dir, "ExampleData/", "BT_Timestamp_2022-02-14.csv"))
dataWTOri <- read.csv(paste0(Dir, "ExampleData/", "WT_Timestamp_2022-02-14.csv"))

## Standardize naming of the column, rename({NewName} = {OldName}), (comment this if your column is already named as "sleep" and "wake")
dataBTOri <- dataBTOri %>% rename(Subject = Subject,
                                  sleep = sleep,
                                  wake = wake)
dataWTOri <- dataWTOri %>% rename(Subject = Subject,
                                  sleep = sleep,
                                  wake = wake)

######## ------- Comment this out if there is no Telegram input) ------- ########
## Read in Telegram csv with timestamps from Step 1
dataTELEOri <- read.csv(paste0(Dir, "ExampleData/", "TELE_Timestamp_2022-02-14.csv"))

## (Comment this out if there is no Telegram input)
dataTELEOri <- dataTELEOri %>% rename(sleep = sleep,
                                      wake = wake)
######## ------- Comment this out if there is no Telegram input) ------- ########

## Input a list of subjects
subjList <- paste0("S", str_pad(c(1:2), width = 3, pad = "0"))

## Set activity count cutoff
ActiDir <- "./ExampleData/"
acticutoff <- 4000

## Set colours of annotation
sleepcolor <- "#c91230"
napcolor <- "#2e514d"
telcolor <- "#e28743"

################################### Directory ###################################

setwd(Dir) ## Set working directory

## Create the output directory
dir.create(path = outDir, showWarnings = FALSE)

################################### Make DateTime Object ###################################
dataBTOri <- dataBTOri %>%
  mutate(sleep = ymd_hms(sleep, tz = "Asia/Singapore"),
         wake = ymd_hms(wake, tz = "Asia/Singapore"))
 
dataWTOri <- dataWTOri %>%
  mutate(sleep = ymd_hms(sleep, tz = "Asia/Singapore"),
         wake = ymd_hms(wake, tz = "Asia/Singapore"))

if(exists("dataTELEOri")) {
  dataTELEOri <- dataTELEOri %>%
  mutate(sleep = ymd_hms(sleep, tz = "Asia/Singapore"),
         wake = ymd_hms(wake, tz = "Asia/Singapore"))
}

#Subj <- "S002" #For script testing only
for (Subj in subjList) {
  tryCatch({
    print(paste0("Making figures for subject ", Subj))
    rm(list=setdiff(ls(), c("acticutoff","ActiDir","dataBTOri","dataWTOri", "dataTELEOri", "Dir", "outDir", "napcolor","sleepcolor","Subj","subjList","svfile","telcolor")))
    
    ################################### Read in actigraphy data for this subject ###################################
    dataActi <- read.csv(paste0(ActiDir, Subj, "_all epoch.csv"), skip = 3)
    
    ## Format Date and Time and date-time object
    dataActi <- dataActi %>% mutate(DateTime = paste0(Date, " ", Time)) %>% mutate(DateTime = dmy_hm(DateTime, tz = "Asia/Singapore")) 
    
    # ################################### subset diaries ###################################
    dfsleep <- dataWTOri %>% filter(Subject == Subj)
    dfnap <- dataBTOri %>% filter(Subject == Subj)
    
    if(exists("dataTELEOri")) {
      dftele <- dataTELEOri %>% filter(Subject == Subj)
    }
 
    offset <- dminutes(30) ## offset of time labels location. 
    
    #################### Process sleep dataframe ####################
      if(dim(dfsleep)[1] > 0) {
        
        ##keep only these two columns
        dfsleep <- dfsleep %>%
          select(sleep, wake) 
        
        dfsleep <- dfsleep %>% rename(
          sleep.d.datetime = sleep,
          wake.d.datetime = wake
        )
  
        ## Create time labels (hh:mm)
        dfsleep <- dfsleep %>%
          mutate(sleep.d.Lab = str_extract(as.character(sleep.d.datetime), "[[:digit:]]{2}:[[:digit:]]{2}"),
                 wake.d.Lab = str_extract(as.character(wake.d.datetime), "[[:digit:]]{2}:[[:digit:]]{2}"))
        
        ## Determine label's location by subtracting the offset (30 mins) to prevent labels from overlapping with the time annotation
        dfsleep <- dfsleep %>% mutate(sleep.d.Loc = sleep.d.datetime - offset,
                                      wake.d.Loc = wake.d.datetime + offset)
        
      }
 
    #################### Process Tele dataframe ####################
    if(exists("dftele")) {
      if(dim(dftele)[1] > 0) {
        
        ##keep only these two columns
        dftele <- dftele %>%
          select(sleep, wake)
        
        dftele <- dftele %>% rename(
          sleep.t.datetime = sleep,
          wake.t.datetime = wake
        )
        
        ## Create time labels (hh:mm)
        dftele <- dftele %>%
          mutate(sleep.t.Lab = str_extract(as.character(sleep.t.datetime), "[[:digit:]]{2}:[[:digit:]]{2}"),
                 wake.t.Lab = str_extract(as.character(wake.t.datetime), "[[:digit:]]{2}:[[:digit:]]{2}"))
        
        ## Determine label's location by subtracting the offset (30 mins) to prevent labels from overlapping with the time annotation
        dftele <- dftele %>% mutate(sleep.t.Loc = sleep.t.datetime - offset,
                                    wake.t.Loc = wake.t.datetime + offset)
      }
    }
    
    if (exists("dftele")) { ## If both Diary and Telegram are available
      if(dim(dfsleep)[1] > 0 & dim(dftele)[1] > 0) {
        
        dfsleep <- dfsleep %>% mutate(
          linkdate = date(wake.d.datetime)
        )
        
        dftele <- dftele %>% mutate(
          linkdate = date(wake.t.datetime)
        )
        
        ## Merge Diary and Telegram
        dfDT <- dfsleep %>% full_join(dftele, by = "linkdate")
        
        ## Determine label's location by subtracting or adding the offset (30 mins) to prevent labels from overlapping with the time annotation
        dfDT <- dfDT %>%
          mutate(sleep.d.Loc = ifelse(sleep.d.datetime > sleep.t.datetime, sleep.d.datetime + offset, sleep.d.datetime - offset),
                 wake.d.Loc = ifelse(wake.d.datetime > wake.t.datetime, wake.d.datetime + offset, wake.d.datetime - offset),
                 sleep.t.Loc = ifelse(sleep.t.datetime < sleep.d.datetime, sleep.t.datetime - offset, sleep.t.datetime + offset),
                 wake.t.Loc = ifelse(wake.t.datetime < wake.d.datetime, wake.t.datetime - offset, wake.t.datetime + offset)) %>%
          mutate(sleep.d.Loc = ifelse(is.na(sleep.d.Loc) & !is.na(sleep.d.datetime), sleep.d.datetime - offset, sleep.d.Loc),
                 wake.d.Loc = ifelse(is.na(wake.d.Loc) & !is.na(wake.d.datetime), wake.d.datetime + offset, wake.d.Loc),
                 sleep.t.Loc = ifelse(is.na(sleep.t.Loc) & !is.na(sleep.t.datetime), sleep.t.datetime - offset, sleep.t.Loc),
                 wake.t.Loc = ifelse(is.na(wake.t.Loc) & !is.na(wake.t.datetime), wake.t.datetime + offset, wake.t.Loc)) %>%
          mutate(sleep.d.Loc = as_datetime(sleep.d.Loc, tz = "Asia/Singapore"),
                 wake.d.Loc = as_datetime(wake.d.Loc, tz = "Asia/Singapore"),
                 sleep.t.Loc = as_datetime(sleep.t.Loc, tz = "Asia/Singapore"),
                 wake.t.Loc = as_datetime(wake.t.Loc, tz = "Asia/Singapore")) #%>%
       
        dfDT <- dfDT %>% select(-linkdate) %>%
          pivot_longer(cols = everything(), names_to = c("Genre", "Channel", ".value"), names_pattern = "(.+)\\.(.+)\\.(.+)")

      }
    } 
    
    if (!exists("dfDT")) {
      ## Pivot Longer
      dfsleep <- dfsleep %>%
        pivot_longer(cols = everything(), names_to = c("Genre", "Channel", ".value"), names_pattern = "(.+)\\.(.+)\\.(.+)")
      
    } else {
      dfsleep <- dfDT
    }
  
      ## add one column to indicate type of sleep period
      dfsleep <- dfsleep %>% mutate(type = "noc")

    #################### Process nap dataframe ####################
    if(dim(dfnap)[1] > 0) {
      
      ## keep only these two columns
      dfnap <- dfnap %>%
        select(sleep, wake)
      
      dfnap <- dfnap %>% rename(
        sleep.d.datetime = sleep,
        wake.d.datetime = wake
      )
      
      ## Create time labels (hh:mm)
      dfnap <- dfnap %>% mutate(sleep.d.Lab = str_extract(as.character(sleep.d.datetime), "[[:digit:]]{2}:[[:digit:]]{2}"),
                                wake.d.Lab = str_extract(as.character(wake.d.datetime), "[[:digit:]]{2}:[[:digit:]]{2}"))
      
      ## Determine label's location by subtracting the offset (30 mins) to prevent labels from overlapping with the time annotation
      dfnap <- dfnap %>% mutate(sleep.d.Loc = sleep.d.datetime - offset,
                                wake.d.Loc = wake.d.datetime + offset)

      dfnap <- dfnap %>% pivot_longer(cols = everything(), names_to = c("Genre", "Channel", ".value"), names_pattern = "(.+)\\.(.+)\\.(.+)")
      
      ## add one column to indicate type of sleep period
      dfnap <- dfnap %>% mutate(type = "nap")
    
    }
    
    #################### Combine dataframes ####################
    
    dfSW <- rbind(if(dim(dfsleep)[1] > 0) dfsleep,
                  if(dim(dfnap)[1] > 0) dfnap)
      
      dfSW <- dfSW %>% mutate(fillcolor = case_when(
        Channel == "d" & type == "noc" ~ "noc",
        Channel == "t" & type == "noc" ~ "tel",
        Channel == "d" & type == "nap" ~ "nap"
      ))
      
    ## Set colors
    colCode = c("noc" = sleepcolor, "nap" = napcolor, "tel" = telcolor)
    
    #################### Process activity counts ####################
    dfacti <- dataActi

    ## retain sleep and wake columns
    dfacti <- dfacti %>%
      rename(datetime = DateTime,
             acticount = Axis1) %>%
      select(datetime, acticount)
    
    ## Curb the activity count at the cutoff specified (default = 4000)
    dfacti <- dfacti %>% mutate(acticountadj = ifelse(acticount > acticutoff, acticutoff, acticount))
    
    #################### Prepare time period for each strip of figure ####################
    ## Pad the dataActi to always start and end at 12:00 (the previous day/ the next day) to facilitate plotting
    uu <- unique(date(dfacti$datetime))
    
    if (hour(dfacti$datetime[1]) < 12) {
      sStart <- date(dfacti$datetime[1]) - days(1)
      uu <- c(sStart, uu)
    }
    
    if (hour(dfacti$datetime[length(dfacti$datetime)]) >= 12) {
      sEnd <- date(dfacti$datetime[length(dfacti$datetime)]) + days(1)
      uu <- c(uu, sEnd)
    }
    
    ss <- rep(uu, each = 2)
    ss <- ss[2:(length(ss)-1)]
    ss <- paste0(ss, " ", "12:00:00")
    ssm <- matrix(ss, nrow = 2)
    ssl <- as.list(as.data.frame(ssm)) ## list of date ranges to be filtered
    
    #################### Plotting starts now ####################
    #j <- ssl[[3]] #For testing
    pActi <- lapply(ssl, function(j) {
      
      ## filter each date range from the ssl list
      dfacti <- dfacti %>%
        filter(datetime >= ymd_hms(j[1],  tz = "Asia/Singapore") & datetime <= ymd_hms(j[2], tz = "Asia/Singapore"))
      
      if(!is.null(dfSW) & dim(dfSW)[1] > 0) {
        dfSW <- dfSW %>%
          filter(datetime >= ymd_hms(j[1],  tz = "Asia/Singapore") & datetime <= ymd_hms(j[2], tz = "Asia/Singapore"))
      }
      
      ## Determine the x-axis limits for plotting
      xlm1 <- ymd_hms(j[1], tz = "Asia/Singapore")
      xlm2 <- ymd_hms(j[2], tz = "Asia/Singapore")
      
      ## Put the day of the week to facilitate reading
      dd <- str_to_upper(as.character(wday(j[1], label = TRUE)))
      dd.loc <- ymd_hms(j[1],  tz = "Asia/Singapore") - dminutes(40)
      
      pActi <-  ggplot() +
        geom_bar(data = dfacti, mapping = aes(x = datetime, y = acticountadj),
                 stat="identity", fill = "#0f02ff") +
        annotate("text", x = dd.loc, y = 2500, label = dd, size = 2, colour = "purple")
      
      if(!is.null(dfSW)) {
        if (dim(dfSW)[1] >= 1) {
          pActi <- pActi +
            geom_tile(data = dfSW, mapping = aes(x = datetime, y = 2500, height = 5000, width = 150, fill = fillcolor)) +
            geom_text(data = dfSW, mapping = aes(x = Loc, y = 4600, label = Lab, colour = fillcolor), size = 2.5)
        }
      }
      
      ## Add layers of other styling
      pActi <- pActi + scale_fill_manual(values = colCode) +
        scale_color_manual(values = colCode) +
        theme_gray() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(color = "black", size = 8),
              axis.text.y = element_text(color = "black", size = 8),
              legend.position = "none",
              panel.grid.minor = element_blank()#,
              #plot.margin = unit(c(0,5,0,5), "lines")
        ) +
        scale_y_continuous(limits = c(0,5000), breaks = seq(0,4000,4000)) +
        coord_cartesian(xlim = c(xlm1,xlm2), clip = 'off')
      
    })
    
    ## Combine every 7 days into 1 figure
    i <- 1
    while (i < ceiling(length(pActi)/7) + 1) {
      assign(paste0("pWeek",i), do.call(ggarrange, c(pActi[(i*7-6):(i*7)], nrow = 7)))
      
      if (svfile == 1) {
        jpeg(paste0(outDir, Subj, "_Week", i, ".jpg"), units="in", width=10, height=5.625, res=300)
        print(get(paste0("pWeek",i)))
        dev.off()
      }
      i = i + 1
    }

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
