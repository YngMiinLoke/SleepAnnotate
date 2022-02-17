# SleepAnnonate
This script helps to annotate participant-reported sleep/wake timings into their activity count figures

In sleep research, we often employ activity monitors to record participants' activity patterns throughout the day. To calculate various sleep parameters, we will need to mark participant-reported sleep/wake periods in the sleep scoring software for it to output the sleep parameters. However, self-reported sleep/wake timings may not always be accurate, and researchers may decide to ignore one's self-reported timings and follow the change in activity patterns instead. Apart from reporting their sleep/wake timings, participants are sometimes asked to send a text message (e.g. Telegram) when they go to bed and when they wake up. These timestamps from text messages can also help researchers to decide where the sleep period(s) should be marked. 

Our lab usually holds a consensus meeting to decide whether we should use mark a specific sleep period using participant's self-reported timing, text message, or change in activity count. The *SleepAnnotate* reads in csv files of participants' activity count, sleep-wake diaries, and telegrams; process the timestamps using lubridates package in R; and plot the sleep/wake timings onto the activity plots. This way, the lab members can visualize all the sleep/wake timings together with the underlying activity patterns to make a decision on where the actual sleep period should be.

### How to use this script

First, clean up diary data and save the sleep and wake timestamps in csv files using *Step1_Cleaning.R*. Edit this script to fit the date and time formats in the diary. Secondly, run *Step2_SleepAnnotate.R* to read in the csv files containing sleep/wake timestamps produced in the first step. The figures containing activity counts and sleep/wake timings for each participants will be printed in the output directory.
