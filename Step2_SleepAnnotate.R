## This script annotate participant-reported (Diary/Telegram) sleep/wake timings into the figures of his/her activity counts 

## 13MAR2022 
## Added SOL
## 16MAR2022
## Let user decide the range of day to be printed, annotate Subject Number and Week Number on top of the figures

## Pending
## whether participant slept that night, ## Did not sleep how to structure it in?
## Added First Class Start Time and Tardiness


library(tidyverse)
library(lubridate)
library(ggpubr)

rm(list = ls())

################################### User's input needed ###################################

svfile = 0 ## Do you want to save the output?

## Modify working directory
Dir <- "./"

## Modify output directory
outDir <- "./ExampleOutput/"

## Read in cleaned timestamps from Step 1
dataNapOri <- read.csv(paste0(Dir, "ExampleData/", "Nap_Timestamp_S001-S002_2022-03-20.csv"))
dataClassOri <- read.csv(paste0(Dir, "ExampleData/", "Class_Timestamp_S001-S002_2022-03-20.csv"))
dataNocSleepOri <- read.csv(paste0(Dir, "ExampleData/", "NocSleep_Timestamp_S001-S002_2022-03-20.csv"))

## Standardize naming of the column, rename({NewName} = {OldName}), (comment this if your column is already named as "sleep" and "wake")
dataNapOri <- dataNapOri %>% rename(Subject = Subject,
                                    BTSelectedDate = BTSelectedDate,
                                    sleep = sleep,
                                    wake = wake)

dataNocSleepOri <- dataNocSleepOri %>% rename(Subject = Subject,
                                              WTSelectedDate = WTSelectedDate,
                                              AnySleep = AnySleep,
                                              sleep = sleep,
                                              wake = wake,
                                              SleepLatency = SleepLatency ## Comment this line and the comma above if you don't have Sleep Latency information
)  

######## ------- Comment this section if there is no Telegram input) ------- ########
## Read in Telegram csv with timestamps from Step 1
dataTELEOri <- read.csv(paste0(Dir, "ExampleData/", "Telegram_Timestamp_S001-S002_2022-03-20.csv"))

## (Comment this out if there is no Telegram input)
dataTELEOri <- dataTELEOri %>% rename(sleep = sleep,
                                      wake = wake)
######## ------- Comment this section if there is no Telegram input) ------- ########

## Input a list of subjects
subjList <- paste0("S", str_pad(c(1:2), width = 3, pad = "0"))

## Input activity count cutoff
ActiDir <- "./ExampleData/"
acticutoff <- 4000

## Input colours of annotation
sleepcolor <- "#c91230" #carmine red
napcolor <- "#2e514d" #pine green
telcolor <- "#e28743" #vibrant honey

## Input cutoff of plot (For ActiGraph, each "strip" of plot starts from 12:00:00 of one day to 12:00:00 of the next day)
plotcutoff <- 12 # Please only input integer (hour)

## Input the range of day to be printed
## For example, if you initialize the watches one day before data collection, you can start from day 2
## A day starts and ends at plotcutoff input above (e.g. plotcutoff = 12, then a day will start at 12:00:00 and ends at 12:00:00 the next day)

stripstart <- 1  # Please only input integer
stripend <- NULL ## Leave stripend as NULL if you want to print till the end of the activity data.

################################### Directory ###################################

setwd(Dir) ## Set working directory

## Create the output directory
dir.create(path = outDir, showWarnings = FALSE)

################################### Make DateTime Object ###################################
dataNapOri <- dataNapOri %>%
  mutate(sleep = ymd_hms(sleep, tz = "Asia/Singapore"),
         wake = ymd_hms(wake, tz = "Asia/Singapore"))

dataNocSleepOri <- dataNocSleepOri %>%
  mutate(sleep = ymd_hms(sleep, tz = "Asia/Singapore"),
         wake = ymd_hms(wake, tz = "Asia/Singapore"))

if(exists("dataClassOri")) {
  dataClassOri <- dataClassOri %>%
    mutate(FirstClassStart = ymd_hms(FirstClassStart, tz = "Asia/Singapore"))
  
  dataClassOri <- dataClassOri %>% mutate(
    ClassMode = factor(ClassMode, ordered = FALSE, levels = c("InPerson", "Online"))
  )

}

if(exists("dataTELEOri")) {
  dataTELEOri <- dataTELEOri %>%
    mutate(sleep = ymd_hms(sleep, tz = "Asia/Singapore"),
           wake = ymd_hms(wake, tz = "Asia/Singapore"))
}

Subj <- "S002" #For script testing only
for (Subj in subjList) {
  tryCatch({
    print(paste0("Making figures for subject ", Subj))
    rm(list=setdiff(ls(), c("acticutoff","ActiDir","dataClassOri","dataNapOri","dataNocSleepOri","dataTELEOri","Dir","napcolor","outDir",
                            "plotcutoff","sleepcolor","stripend","stripstart","Subj","subjList","svfile","telcolor")))
    
    ################################### Read in actigraphy data for this subject ###################################
    dataActi <- read.csv(paste0(ActiDir, Subj, "_all epoch.csv"), skip = 3)
    
    ## Format Date and Time and date-time object
    dataActi <- dataActi %>% mutate(DateTime = paste0(Date, " ", Time)) %>% mutate(DateTime = dmy_hm(DateTime, tz = "Asia/Singapore")) 
    
    # ################################### subset diaries ###################################
    dfsleep <- dataNocSleepOri %>% filter(Subject == Subj)
    dfnap <- dataNapOri %>% filter(Subject == Subj)
    
    if(exists("dataTELEOri")) {
      dftele <- dataTELEOri %>% filter(Subject == Subj)
    }
    
    offset <- dminutes(30) ## offset of time labels location. 
    
    #################### Process sleep dataframe ####################
    if(dim(dfsleep)[1] > 0) {
      
      ##keep only relevant columns
      if("SleepLatency" %in% colnames(dfsleep)) {
        dfsleep <- dfsleep %>%
          select(sleep, wake, SleepLatency) 
      } else {
        dfsleep <- dfsleep %>%
          select(sleep, wake) 
      }
      
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
      
      if("SleepLatency" %in% colnames(dfsleep)) {
        ## Output SleepLatency as timestamps
        dfsleep <- dfsleep %>% mutate(SOLstart = ifelse(!is.na(SleepLatency), sleep.d.datetime, NA),
                                      SOLend = ifelse(!is.na(SleepLatency), sleep.d.datetime + dminutes(SleepLatency), NA)
        )
        
        dfsleep <- dfsleep %>% mutate(SOLstart = as_datetime(SOLstart, tz = "Asia/Singapore"),
                                      SOLend = as_datetime(SOLend, tz = "Asia/Singapore")
        )
        
        ## SleepLatency as a separate dataframe for later use
        dfSOL <- dfsleep %>% select(SOLstart,SOLend)
        dfSOLl <- dfSOL %>% rownames_to_column() %>% pivot_longer(!rowname, names_to = "type", values_to = "datetime")
        
        dfSOLl <- dfSOLl %>% mutate(type = factor(type, levels = c("SOLstart", "SOLend")))
        
        dfsleep <- dfsleep %>% select(-c(SOLstart,SOLend,SleepLatency))
      }
      
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
    
    #################### Process class start time ####################
    dfclass <- dataClassOri %>% filter(Subject == Subj)
    
    ## Set linetypes
    linetypeCode = c("InPerson" = "solid", "Online" = "dotted")
    
    
    #################### Prepare time period for each strip of figure ####################
    ## Pad the dataActi to always start and end at plotcutoff (the previous day/ the next day) to facilitate plotting
    uu <- unique(date(dfacti$datetime))
    
    if (hour(dfacti$datetime[1]) < plotcutoff) {
      sStart <- date(dfacti$datetime[1]) - days(1)
      uu <- c(sStart, uu)
    }
    
    if (hour(dfacti$datetime[length(dfacti$datetime)]) >= plotcutoff) {
      sEnd <- date(dfacti$datetime[length(dfacti$datetime)]) + days(1)
      uu <- c(uu, sEnd)
    }
    
    plotcutoffchar <- str_pad(plotcutoff, width = 2, pad = "0")
    
    ss <- rep(uu, each = 2)
    ss <- ss[2:(length(ss)-1)]
    ss <- paste0(ss, " ", plotcutoffchar, ":00:00")
    ssm <- matrix(ss, nrow = 2)
    ssl <- as.list(as.data.frame(ssm)) ## list of date ranges to be filtered
    
    #################### Plotting starts now ####################
    j <- ssl[[7]] #For testing only
    pActi <- lapply(ssl, function(j) {
      
      ## filter each date range from the ssl list
      dfacti <- dfacti %>%
        filter(datetime >= ymd_hms(j[1],  tz = "Asia/Singapore") & datetime <= ymd_hms(j[2], tz = "Asia/Singapore"))
      
      if(exists("dfSW")) {
        if(dim(dfSW)[1] > 0) {
          dfSW <- dfSW %>%
            filter(datetime >= ymd_hms(j[1],  tz = "Asia/Singapore") & datetime <= ymd_hms(j[2], tz = "Asia/Singapore"))
        }
      }
      
      if(exists("dfSOLl")) {
        if(dim(dfSOLl)[1] > 0) {
          dfSOLlf <- dfSOLl %>%
            filter(datetime >= ymd_hms(j[1],  tz = "Asia/Singapore") & datetime < ymd_hms(j[2], tz = "Asia/Singapore"))
          
          if(dim(dfSOLlf)[1] > 0) {
            dfSOLw <- dfSOLlf %>% pivot_wider(names_from = type, names_expand = TRUE, values_from = datetime)
            
            ## pat with plotcutoff for NA values (SOL that cut across plotcutoff)
            dfSOLw <- dfSOLw %>% mutate(
              SOLstart2 = ifelse(is.na(SOLstart), ymd_hms(paste0(date(SOLend), " ", plotcutoffchar, ":00:00"), tz = "Asia/Singapore") ,SOLstart),
              SOLend2 = ifelse(is.na(SOLend), ymd_hms(paste0(date(SOLstart + days(1)), " ", plotcutoffchar, ":00:00"), tz = "Asia/Singapore") ,SOLend)
            ) %>% mutate(
              SOLstart2 = as_datetime(SOLstart2, tz = "Asia/Singapore"),
              SOLend2 = as_datetime(SOLend2, tz = "Asia/Singapore")
            )
          }
        }
        
      }
      
      if(exists("dfclass")) {
        if(dim(dfclass)[1] > 0) {
          dfclass <- dfclass %>%
            filter(FirstClassStart >= ymd_hms(j[1],  tz = "Asia/Singapore") & FirstClassStart <= ymd_hms(j[2], tz = "Asia/Singapore"))
        }
      }
      
      ## Determine the x-axis limits for plotting
      xlm1 <- ymd_hms(j[1], tz = "Asia/Singapore")
      xlm2 <- ymd_hms(j[2], tz = "Asia/Singapore")
      
      ## Put the day of the week to facilitate reading
      ddl <- str_to_upper(as.character(wday(j[1], label = TRUE)))
      ddl.loc <- ymd_hms(j[1],  tz = "Asia/Singapore") - dminutes(40)
      
      ddr <- str_to_upper(as.character(wday(j[2], label = TRUE)))
      ddr.loc <- ymd_hms(j[2],  tz = "Asia/Singapore") + dminutes(40)
      
      pActi <-  ggplot() +
        geom_bar(data = dfacti, mapping = aes(x = datetime, y = acticountadj),
                 stat="identity", fill = "#0f02ff") +
        annotate("text", x = ddl.loc, y = 2500, label = ddl, size = 2, colour = "purple") +
        annotate("text", x = ddr.loc, y = 2500, label = ddr, size = 2, colour = "purple")
      
      if(exists("dfSOLw")) {
        if (dim(dfSOLw)[1] > 0) {
          pActi <- pActi +
            geom_rect(data = dfSOLw, mapping = aes(xmin = SOLstart2, xmax = SOLend2, ymin = 4001, ymax = 5000), fill = "#F781BF", alpha = 0.3)
        }
      }
      
      if(exists("dfSW")) {
        if (dim(dfSW)[1] > 0) {
          pActi <- pActi +
            geom_tile(data = dfSW, mapping = aes(x = datetime, y = 2500, height = 5000, width = 150, fill = fillcolor)) +
            geom_text(data = dfSW, mapping = aes(x = Loc, y = 4600, label = Lab, colour = fillcolor), size = 2.5)
        }
      }
      
      if(exists("dfclass")) {
        if (dim(dfclass)[1] > 0) {
          pActi <- pActi +
            geom_segment(data = dfclass, mapping = aes(x = FirstClassStart, y = 4000, xend = FirstClassStart, yend = 5000, linetype = ClassMode), size = 0.5)
        }
      }
      
      
      ## Add layers of other styling
      pActi <- pActi + scale_fill_manual(values = colCode) +
        scale_color_manual(values = colCode) +
        scale_linetype_manual(values = linetypeCode) +
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
    
    if (is.null(stripend)) {
      stripend = length(pActi)
    }
    
    pActi_userSelect <- pActi[stripstart:stripend]
    
    i <- 1
    while (i < ceiling(length(pActi_userSelect)/7) + 1) {
      assign(paste0("pWeek",i), do.call(ggarrange, c(pActi_userSelect[(i*7-6):(i*7)], nrow = 7)))
      
      assign(paste0("pWeek",i), annotate_figure(get(paste0("pWeek",i)), 
                                                top = text_grob(paste0(Subj, "  Week ",i))))
      
      if (svfile == 1) {
        jpeg(paste0(outDir, Subj, "_Week", i, ".jpg"), units="in", width=10, height=5.625, res=200)
        print(get(paste0("pWeek",i)))
        dev.off()
      }
      i = i + 1
    }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}