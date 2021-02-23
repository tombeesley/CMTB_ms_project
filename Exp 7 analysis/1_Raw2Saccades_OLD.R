rm(list=ls())
library(tidyverse)
library(saccades)
library(zoo) # used for the na.spline function

Experiment <- 7
rawDir <- str_c("Exp ", Experiment, " Raw Data CSV")

patternTypes <- c("dec","fb") #variable that differentiates between the 2 different csv files for the different experimental periods

fnams <- list.files(rawDir, pattern = "dec", full.names = TRUE) # needed for reading data


allEGData <- NULL # empty array to fill with processed data from all participants' raw data
allResponseData <- NULL

for (s in 1:length(fnams)) { # telling it to run through all the "fix" period files in the raw data file
  
  # creates variable to extract file name (counting backwards from file name characters to get participant number)
  subNum <- substr(fnams[s], start = nchar(fnams[s])-13, stop = nchar(fnams[s])-4)
  subFiles <- list.files(rawDir, pattern = subNum, full.names = TRUE) # needed for reading data
  
  subData <- NULL 
  print(subNum)
  
  for (p in 1:2) { #for loop that goes through all files pertaining to periods "dec","fb"
    
    print(p)
    
    curFile <- subFiles[str_detect(subFiles,pattern=patternTypes[p])] #variable that detects files of the relevant period (dec,fb)
    
    periodRaw <- read_csv(curFile, col_types = cols(), col_names = FALSE) # read the data from csv
    colnames(periodRaw) <- c("time","x","y","trial")
    periodRaw <- periodRaw %>% 
      mutate_at(vars(x,y), ~ifelse(is.nan(.), NA, .)) %>% # replace NaN with NA
      mutate_at(vars(x,y), na.spline, maxgap = 25) %>% # interpolate
      mutate_at(vars(x,y), ~ifelse(is.na(.), 0, round(.))) # turn any remaining NAs into 0
    
    startTimes <- periodRaw %>% 
      group_by(trial) %>% 
      summarise(startTime=min(time)) # this gets the time of the first sample in each period
    
    ## ---- run_saccades_program
    periodFix <- subset(detect.fixations(periodRaw), event=="fixation") # extract those fixations
    
    periodFix <- periodFix %>% 
      mutate(subj = subNum,
             period = p) %>% 
      left_join(startTimes, by = "trial") # add the startTimes column to periodFix

    subData <- rbind(subData,periodFix) 
    
  }
  
  subData <- subData %>% mutate(periodLatency = start-startTime) %>% # work out fixation latency from start of period.
    select(subj,trial,period,startTime,start,end,dur,periodLatency,x:peak.vy) # reorder the columns
  
  allEGData <- rbind(allEGData, subData)
  
  curFile <- subFiles[str_detect(subFiles,pattern="trial")] # detect trial data file
  respData <- read_csv(curFile, col_types = cols(X3 = col_character()), col_names = FALSE) # read the data from csv
  respData$X1 <- subNum #replace the subNum with the complete one from the file name
  allResponseData <- rbind(allResponseData,respData) # great large dataframe
  
}

# rename variables, recode sex variable values for consistency
colnames(allResponseData) <- c("subj","age","sex","trial","cue","resp","out","numResp","RT")
allResponseData$sex <- recode(allResponseData$sex, 'f' = "F", 'm' = "M")

outFileName <- str_c("Exp_", Experiment, "_data_Experimental.RData")
save(list=c("allEGData", "allResponseData"), file=outFileName)
