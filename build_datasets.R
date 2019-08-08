# Author: Jeremy Boyd (jeremyboyd@micron.com)
# Description: Build train, validation, test datasets to run through
# deepmatcher.

# Packages
library(tidyverse)
library(caret)
library(HRWFIDB)
HRWFIDBInit()

df <- ExecSQLFile(filename = "get_school_name_data.sql",
                  server = "Production") %>%
    as_tibble()

df0<-subset(df[sample(nrow(df)),],!duplicated(left_School_Name))
# Table of matches
df2 <- df %>%
     mutate(label = 1) %>%
     select(label, left_School_Name, right_School_Name)
library(foreach)
newdf =df
newdf$label = 1
newdf$train = sample(c(0,1,2),size=nrow(newdf),replace=TRUE,prob=c(0.2,0.2,0.6))

newdf_add= foreach(i = 1:nrow(newdf),.combine=rbind) %dopar% {
    library(dplyr)
    message(i)
    schools = unique(filter(newdf,right_School_Name != newdf$right_School_Name[i])$right_School_Name)
    newids = sample(schools,100,replace=FALSE)
    add = data.frame(right_School_Name =  newids)
    add$left_School_Name= newdf$left_School_Name[i]
    add$train= newdf$train[i]
    add$label = 0
    add = select_at(add,names(newdf))
   return(add)
}
newdf = rbind(newdf,newdf_add)


# Table of non-matches
df3 <- df2 %>%
     mutate(label = 0) %>%
     select(label, left_School_Name, right_School_Name)
     
# In the table of non-matches, randomly order right school names. This ensures
# that what we're calling non-matches (label == 0) are indeed (mostly)
# non-matches.
set.seed(1)
df3$right_School_Name <- df3$right_School_Name[sample(1:length(df3$right_School_Name))]

# Combine random 1K samples from the match and non-match tables; add in id col
df4 <- rbind(df2 %>% sample_n(10000), df3 %>% sample_n(10000)) %>%
    mutate(id = row_number()) %>%
    select(id, label, left_School_Name, right_School_Name)

# Create training set
train.index <- createDataPartition(df4$label, p = 0.95, list = FALSE)
train <- df4[train.index,]

# Non-training rows go into this temp table, which is then subdivided into test
# and validation sets.
temp <- df4[-train.index,]
test.index <- createDataPartition(temp$label, p = .5, list = FALSE)
test <- temp[test.index,]
validation <- temp[-test.index,]

# Write to file
write_csv(train, "school_data/train.csv")
write_csv(test, "school_data/test.csv")
write_csv(validation, "school_data/validation.csv")



SQLcode="select school_name_location, s.school_conformed_name, s.prod_sf_picklist_school_id   

from hrdw.d_schools s

where school_conformed_name != '' and prod_sf_picklist_school_id is not null

order by school_conformed_name" # Query

library(keras)
library(psych)# for dummy code
library(tidyverse) # for pipe
library(stringi) # for cleaning wierd characters
library(stringr)
library(HRWFIDB)
DB=HRWFIDB::ExecSQL(SQLcode) # one x variable 2 y variables (actually 1)
DB$school_name_location <-stri_trans_general(DB$school_name_location,'Latin-ASCII') 
# erasing wierd characters

rows <- sample(nrow(DB)) #shuffling data order
DB <- DB[rows, ]       #shuffling data order

#word level

#library(keras)
samples <-DB$school_name_location

#####data preprocessin for unidentifable characters
samples <- stringr::str_replace_all(samples, "[[:punct:]]", " ")
samples<-str_replace_all(samples,'[^[:alpha:]]','') 



right_1<-unique(DB$school_conformed_name)
unlabel=expand.grid(test$left_School_Name,right_1)
unlabel=cbind(1:dim(unlabel)[1],unlabel)
names(unlabel)<-c("id","left_School_Name","right_School_Name")
write.csv(unlabel,file="unlabeled.csv",row.names=FALSE)
