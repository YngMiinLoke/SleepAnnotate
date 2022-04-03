# SleepAnnonate
This script helps to annotate participant-reported sleep/wake timings into their activity count figures

In sleep research, we often employ activity monitors to record participants' activity patterns throughout the day. Here's an example of the activity counts collected by actigraphy watch:

<img width="652" alt="img01" src="https://user-images.githubusercontent.com/99003122/161433468-c62f0d98-a7fa-458b-b83e-0755d42f86b7.png">

To calculate various sleep parameters, we will need to mark participant-reported sleep/wake periods in the sleep scoring software for it to output the sleep parameters. Our lab instructed participants to fill up their sleep and wake time in daily diary. Apart from the daily diary, we also asked the participants to send a text message (Telegram) when they go to bed and when they wake up. However, self-reported sleep/wake timings may not be always accurate, and researchers may decide the sleep-wake periods based on a combination of participant's self-reported timing, Telegram timestamps, or change in activity counts.

The *SleepAnnotate* reads in csv files of participants' activity count, sleep-wake diaries, and telegrams; process the timestamps using lubridates package in R; and plot the sleep/wake timings onto the activity plots:

<img width="646" alt="img02" src="https://user-images.githubusercontent.com/99003122/161433876-dd30f604-3344-447b-9f27-d46ea9bedcc2.png">
In the above image, red markings denote the diary-reported sleep/wake timings, while the orange markings denote the Telegram sleep/wake timestamps.

The *SleepAnnotate* also plots the nap timings and first class start time which participants reported in diary:
<img width="652" alt="img03" src="https://user-images.githubusercontent.com/99003122/161434075-ff66e0b3-f976-4a90-a210-283d5ff0e9fa.png">

Our lab usually holds a consensus meeting to decide on participants' sleep-wake timings. Using the *SleepAnnotate*, the lab members can visualize all the sleep/wake timings together with the underlying activity patterns to make a decision on where the actual sleep period should be.

### How to use this script

First, clean up diary data and save the sleep and wake timestamps in csv files using *Step1_Cleaning.R*. Edit this script to fit the date and time formats in the diary. Secondly, run *Step2_SleepAnnotate.R* to read in the csv files containing sleep/wake timestamps produced in the first step. The figures containing activity counts and sleep/wake timings for each participants will be printed in the output directory.

