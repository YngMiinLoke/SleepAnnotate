# SleepAnnonate
This script helps to annotate participant-reported sleep/wake timings onto their activity count figures

In sleep research, we often employ activity monitors to record participants' activity patterns throughout the day. Here's an example of the activity counts collected by an actigraphy watch:

<img width="652" alt="img01" src="https://user-images.githubusercontent.com/99003122/161433468-c62f0d98-a7fa-458b-b83e-0755d42f86b7.png">

To calculate various sleep parameters, we will need to mark participant-reported sleep/wake periods in the sleep scoring software to calculate the sleep parameters within the sleep periods. To collect sleep/wake timings, our lab instructed participants to fill up their sleep and wake times in a daily diary. Apart from the daily diary, we also asked participants to send a text message (Telegram) when they go to bed and when they wake up. However, self-reported sleep/wake timings may not be always accurate, and researchers may decide on the sleep-wake periods based on a combination of participant's self-reported timing, Telegram timestamps, or change in activity counts.

The *SleepAnnotate* reads in csv files of participants' activity counts, sleep-wake diaries, and Telegram messages. It then processes the timestamps using lubridate package in R, and plot the sleep/wake timings onto the activity plots using the ggplot2 package:

<img width="646" alt="img02" src="https://user-images.githubusercontent.com/99003122/161433876-dd30f604-3344-447b-9f27-d46ea9bedcc2.png">
In the above image, red markings denote the diary-reported sleep/wake timings, while the orange markings denote the Telegram sleep/wake timestamps. The translucent red patch that follows the diary-reported sleep time (red marking on the left) denotes the participant's sleep onset latency.


The *SleepAnnotate* also plots the nap timings (markings in pine green) and first class start time (markings in purple; solid line represents in-person class, dotted line represents online class) which participants reported in their diary:

<img width="652" alt="img03" src="https://user-images.githubusercontent.com/99003122/161434075-ff66e0b3-f976-4a90-a210-283d5ff0e9fa.png">

Our lab usually holds a consensus meeting to decide on participants' sleep-wake timings. Using *SleepAnnotate*, the lab members can visualize diary- and Telegram-reported sleep/wake timings overlaid on participant's activity patterns to decide on where the actual sleep period should be.

### How to use this script

You will need to provide four csv files as input to the *Step2_SleepAnnotate.R*

1. NocSleep_Timestamp.csv
2. Nap_Timestamp.csv
3. Class_Timestamp.csv
4. Telegram_Timestamp.csv

You can omit no. 3 and no. 4 if your lab doesn't collect first class start time and Telegram timestamps. Please refer to the "ExampleData" folder to see how the input csv should be structured. You can also refer to the *Step1_Cleaning.R* script to see how to clean up Bedtime and Wake Time Diaries to produce these input spreadsheets.
