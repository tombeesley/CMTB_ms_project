rm(list=ls())
library(tidyverse)

load("Exp_6_data.RData")

# this limits feedback period to the first 1 second
allEGData <- allEGData %>% 
  filter(period == 1 | (period == 2 & (periodLatency + dur) < 1000))

# make new "condition" variable based on the subj variable.
allEGData <- allEGData %>% 
  separate(subj, into = c("condition", "subj"), sep = "_", extra = "merge") %>% 
  select(subj, everything())

# Define AOIs
cue_AOI_dimensions = c(135,880,1785,1030)
resp_AOI_dimensions = c(135,600,1785,750)
out_AOI_dimensions = c(135,50,1785,200)
out_AOI_C2_dimensions = c(135,450,1785,600)

checkXYinAOI <- function(x, y, dur, AOI) { # passes the function 3 variables: x,y,and AOI
  return((x>=AOI[1] & y>=AOI[2] & x<=AOI[3] & y<=AOI[4])*dur) # 
}

# add columns to the EG data which state whether each fixation was in the defined AOIs
allEGData <- allEGData %>% 
  rowwise() %>% 
  mutate(AOI_cue = checkXYinAOI(x,y,dur,cue_AOI_dimensions),
         AOI_resp = checkXYinAOI(x,y,dur,resp_AOI_dimensions),
         AOI_out = if_else(condition %in% c("C1", "C3"),
                           true = checkXYinAOI(x,y,dur,out_AOI_dimensions),
                           false = checkXYinAOI(x,y,dur,out_AOI_C2_dimensions))) %>% 
  ungroup()

# summarise AOI time and nFix per period at the trial level  
trialEG <- allEGData %>% 
  group_by(subj,condition,trial,period) %>%
  summarise(AOI_cue = sum(AOI_cue),
            AOI_resp = sum(AOI_resp),
            AOI_out = sum(AOI_out),
            nFix = n()) %>% # number of fixations per period
  pivot_wider(names_from = period, values_from = AOI_cue:nFix, names_prefix = "p") %>% 
  ungroup() %>% 
  # next line fills in missing trial numbers. 
  # N.B. it assumes all trial numbers (1:72) are present in the set
  complete(subj,trial) %>%
  # next 3 lines fills in missing condition values
  group_by(subj) %>% 
  fill(condition, .direction = "updown") %>% 
  ungroup() %>% 
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

# pivot wider the quality data, so that each row is one trial
allQuality <- allQuality %>% 
  mutate(subj = str_sub(subj,4,-1)) %>% # remove the condition part of the subj name
  pivot_wider(names_from = period, values_from = samples:post_interp_NA, names_prefix = "p")

# combine the three arrays together
combinedData <- allResponseData %>% 
  mutate(subj = str_sub(subj,4,-1)) %>% # remove the condition part of the subj name
  left_join(trialEG, by = c("subj", "trial")) %>% 
  mutate(tOutFix = first_out_fix$time) %>% 
  left_join(allQuality, by = c("subj", "trial"))
  
# create new phase and block variables, and arrange variables
finalData <- combinedData %>% 
  mutate(phase = ceiling(trial/24),block=ceiling(trial/4)) %>% 
  select(subj,condition,age,sex,phase,block,trial:AOI_out_p2, tOutFix, nFix_p1, nFix_p2, samples_p1:post_interp_NA_p2)

# add cu traits from CSV file
cu_traits <- read_csv('Exp6_CU.csv') %>% 
  mutate(subj = str_c("P", ID, "_Exp6")) %>%# make subj column compatible with the other dataframe
  select(subj, ICU) # order variables

# join CU traits to the final data, and create split variable
finalData <- finalData %>% 
  left_join(cu_traits, by = 'subj') %>% # merge cu values with final dataframe
  mutate(ICU_medSplit = factor(ICU<=median(ICU, na.rm = TRUE))) %>% # make a medium split factor
  mutate(ICU_medSplit = recode_factor(ICU_medSplit, "TRUE"="Low","FALSE"="High")) # recode that factor to levels "High" and "Low"

write_csv(finalData, "Exp_6_proc_data_1s.csv")

