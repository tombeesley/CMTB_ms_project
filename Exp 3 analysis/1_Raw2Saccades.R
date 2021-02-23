# here's this message  - what does it do?

rm(list=ls())
library(tidyverse)
library(saccades)
library(zoo) # used for the na.spline function

Experiment <- 3
rawDir <- str_c("Exp ", Experiment, " Raw Data CSV")

patternTypes <- c("dec","fb") # used for reading the 2 different types of EG csv files, one type for each experimental period

fnams <- list.files(rawDir, pattern = "dec", full.names = TRUE) # list of file names of one type (i.e., the unique participants)

# these are the 3 main arrays which will contain all the data from all participants
# these are saved at the end of this program
allEGData <- NULL 
allResponseData <- NULL
allQuality <- NULL

for (s in 1:length(fnams)) { # run through the files from each participant
  
  # creates variable to extract file name (counting backwards from file name characters to get participant number)
  subNum <- substr(fnams[s], start = nchar(fnams[s])-12, stop = nchar(fnams[s])-4)
  subFiles <- list.files(rawDir, pattern = subNum, full.names = TRUE) # gets files associated with that participant
  
  subData <- NULL 
  subQuality <- NULL
  print(subNum)
  
  for (p in 1:2) { # for each of the two periods of EG data: "dec" and "fb"
    
    print(p)
    
    curFile <- subFiles[str_detect(subFiles,pattern=patternTypes[p])] # pulls out the relevant file, based on patternTypes
    
    periodRaw <- read_csv(curFile, col_types = cols(), col_names = FALSE) # read csv
    colnames(periodRaw) <- c("time","x","y","trial") # new colnames which fit required variables for "detect.fixations"
    
    # evaluate prop missing data (NaN) for data quality 
    periodQuality <- periodRaw %>% 
      group_by(trial) %>% 
      summarise(samples = n(),
                raw_NA = sum(is.nan(x)==TRUE))

    periodRaw <- periodRaw %>% 
      mutate_at(vars(x,y), ~ifelse(is.nan(.), NA, .)) %>% # replace NaN with NA
      mutate_at(vars(x,y), na.spline, maxgap = 25) %>% # interpolate
      mutate_at(vars(x,y), ~ifelse(is.na(.), 0, round(.))) # turn any remaining NAs into 0
    
    # evaluate prop missing data (0) post interpolation for data quality 
    periodQuality <- periodRaw %>% 
      group_by(trial) %>% 
      summarise(post_interp_NA = sum(x==0)) %>% 
      right_join(periodQuality, by = "trial") %>% # join to pre interpolation dqc
      mutate(subj = subNum,
             period = p) %>% 
      select(subj, trial, period, samples, raw_NA, post_interp_NA)
    
    subQuality <- rbind(subQuality, periodQuality) # combine period data quality array for subject
    
    startTimes <- periodRaw %>% 
      group_by(trial) %>% 
      summarise(startTime=min(time)) # this gets the time of the first sample in each period
    
    # run "detect.fixations" from the saccades package
    periodData <- subset(detect.fixations(periodRaw), event=="fixation") # extract those fixations
    
    periodData <- periodData %>% 
      mutate(subj = subNum, period = p) %>% 
      left_join(startTimes, by = "trial") # add the startTimes column to periodFix

    subData <- rbind(subData,periodData) # combine period data for subject
    
  }
  
  subData <- subData %>% 
    mutate(periodLatency = start-startTime) %>% # work out fixation latency from start of period.
    select(subj,trial,period,startTime,start,end,dur,periodLatency,x:peak.vy) # reorder the columns
  
  allEGData <- rbind(allEGData, subData) # combine EG data with other subjects
  
  allQuality <- rbind(allQuality, subQuality) # combine data quality data with other subjects
  
  curFile <- subFiles[str_detect(subFiles,pattern="trial")] # detect trial data file (i.e., trial events, responses)
  respData <- read_csv(curFile, col_types = cols(X3 = col_character()), col_names = FALSE) # read the data from csv
  respData$X1 <- subNum # replace the subNum column with the full subject number from the file name
  allResponseData <- rbind(allResponseData,respData) # combine with the other subjects
  
}

# rename variables in response data, recode sex variable values for consistency
colnames(allResponseData) <- c("subj","age","sex","trial","cue","resp","out","numResp","RT")
allResponseData$sex <- recode(allResponseData$sex, 'f' = "F", 'm' = "M")

outFileName <- str_c("Exp_", Experiment, "_data.RData")
save(list=c("allEGData", "allResponseData", "allQuality"), file=outFileName) # save RData file containing 3 main dataframes
