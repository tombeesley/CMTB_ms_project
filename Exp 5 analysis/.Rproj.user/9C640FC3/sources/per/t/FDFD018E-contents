library(tidyverse)
library(saccades)

data <- read_csv("input.csv")

# this gives warnings about returning -Inf
# and all events are given as type NA
detect.fixations(data) %>% view()

# but then if a single trial with some data is selected
# we get some fixations (they are all very short, however)
data_T48 <- filter(data, trial == 46)
detect.fixations(data_T48) %>% view()

# question - why does the saccades algorithm not produce the fixations in the first example?

  
