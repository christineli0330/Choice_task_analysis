## file for calculate demographic info

library(tidyverse)
library(plyr)

### choice task
dat = read.csv("C:/Users/Christine/Box Sync/data/mem_dm/full_data_dm_3_12.csv")

#create unique id for each subject
dat$ID = cumsum(!duplicated(dat[1:2]))
#get code
code = dat %>% filter(completioncode !='"')
age = dat %>% filter(ttype =='completion', trial_type == 'survey-html-form')
#separate age input from string variables in responses
age = age %>% mutate(age = as.numeric(substr(responses, 9, 10)))
#calculate mean and sd
mean(age$age) #a kind of large mean
sd(age$age)

#probably should calculate inputted mean?



### memorability task
mem = read.csv("C:/Users/Christine/Box Sync/data/mem_dm/all_mem_exclude_mat.csv")

table(mem$Answer.age)
table(mem$Answer.ethnicity)

