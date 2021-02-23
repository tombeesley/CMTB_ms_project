rm(list=ls())
library(tidyverse)

load("Exp_7_data_Experimental.RData")

# Define AOIs
cue_AOI_dimensions = c(235,780,1685,1030)
resp_AOI_dimensions = c(235,400,1685,650)
out_AOI_dimensions = c(235,50,1685,300)

checkXYinAOI <- function(x, y, dur, AOI) { # passes the function 3 variables: x,y,and AOI
  return((x>=AOI[1] & y>=AOI[2] & x<=AOI[3] & y<=AOI[4])*dur) # 
}

# add columns to the EG data which state whether each fixation was in the defined AOIs
allEGData <- allEGData %>% 
  rowwise() %>% 
  mutate(AOI_cue = checkXYinAOI(x,y,dur,cue_AOI_dimensions),
         AOI_resp = checkXYinAOI(x,y,dur,resp_AOI_dimensions),
         AOI_out = checkXYinAOI(x,y,dur,out_AOI_dimensions)) %>% 
  ungroup()

# summarise AOI time and nFix per period at the trial level  
trialEG <- allEGData %>% 
  group_by(subj,trial,period) %>%
  summarise(AOI_cue = sum(AOI_cue),
            AOI_resp = sum(AOI_resp),
            AOI_out = sum(AOI_out),
            nFix = n()) %>% # number of fixations per period
  pivot_wider(names_from = period, values_from = AOI_cue:nFix) %>% 
  ungroup() %>% 
  # next line fills in missing trial numbers. 
  # N.B. it assumes all trial numbers (1:72) are present in the set
  complete(subj,trial) %>% 
  arrange(subj,trial)

# summarise time of first AOI_out fixation during feedback, at the trial level
first_out_fix <- allEGData %>% 
  filter(period == 2, AOI_out > 0) %>% 
  group_by(subj,trial) %>%
  slice(1) %>% 
  summarise(time = periodLatency) %>% 
  ungroup() %>% 
  # next line fills in missing trial numbers. 
  # N.B. it assumes all trial numbers (1:72) are present in the set
  complete(subj,trial) %>% 
  arrange(subj,trial)

# combine the two EG arrays with the response data
combinedData <- allResponseData %>% 
  left_join(trialEG, by = c("subj", "trial")) %>% 
  mutate(tOutFix = first_out_fix$time)

finalData <- combinedData %>% 
  mutate(phase = ceiling(trial/24),block=ceiling(trial/4)) %>% 
  select(subj:sex,phase,block,trial:AOI_out_2, tOutFix, nFix_1, nFix_2)

# add cu traits from CSV file
cu_traits <- read_csv('Exp7_CU.csv') %>% 
  mutate(subj = str_c("P", subj, "_Exp7")) %>% # make subj column compatible with the other dataframe
  select(subj, ICU) # order variables

finalData <- finalData %>% 
  left_join(cu_traits, by = 'subj') %>% # merge cu values with final dataframe
  mutate(ICU_medSplit = factor(ICU<=median(ICU))) %>% # make a medium split factor
  mutate(ICU_medSplit = recode_factor(ICU_medSplit, "TRUE"="Low","FALSE"="High")) # recode that factor to levels "High" and "Low"

write_csv(finalData, "Exp_7_proc_data.csv")

