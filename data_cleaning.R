library(tidyverse)
library(dplyr)


data_all <- read_csv("1_Data/data_all.csv")

data_comments <- data_all %>% 
  select(-reply) %>% 
  distinct(comment, .keep_all=TRUE)
write_csv(data_comments, "1_Data/Clean/comments.csv")

data_replies <- data_all[
  !(is.na(data_all$reply) | trimws(data_all$reply)==""),
]
write_csv(data_replies, "1_Data/Clean/replies.csv")

------------------------------------------------------------------
  data_comments %>%
  duplicated() %>%
  sum()

data_comments %>%
  count(comment) %>%
  filter(n > 1)

nrow(data_comments)
nrow(data_replies)
1301+627 
----------------------------------------------------------------
  
  #dataset_comments_validation <- read_csv("1_Data/data_comments_validation.csv") 
  
  
  #data_comments_postval <- data_comments %>% 
  #filter(!row_id %in% dataset_comments_validation$row_id)
  #nrow(data_comments_postval) # = 1108
  #1301-194 # = 1107 where did that other one go?
  
  #length(intersect(
  # data_comments$row_id,
  #dataset_comments_validation$row_id
  #))
  
  #don't forget to re-include these ones in analysis!
  #intersect(data_comments$comment, dataset_comments_validation$comment) #overlap of comments
  #intersect(data_comments_postval$comment, dataset_comments_validation$comment) #overlap got removed, now produces 0
  
  #nrow(data_comments_postval)
  #nrow(dataset_comments_validation)
  #nrow(data_replies)
  #nrow(data_all)
  #a+b+c
  -------------------------------------------------------------------------------------
  #managing missing cases
  intersect(data_comments$row_id, data_replies$row_id)

missing_rows <- data_all %>%
  filter(
    !row_id %in% data_comments$row_id &
      !row_id %in% data_replies$row_id
  )  

nrow(missing_rows)
missing_rows
matches <- c(676,791,796,783,783,783,783,676,827,827,783,783,783,783,783,1155,1946)
length(matches)
missing_rows_match <- missing_rows %>% 
  mutate(match = matches) %>% 
  relocate(match, .after = row_id)

write_csv(missing_rows_match, "1_Data/Clean/missing_rows.csv")

#-----------------------------------------------------------------------------------

full_data <- comments %>% 
  bind_rows(missing_rows) %>% 
  bind_rows(replies)

write_csv(full_data, "1_Data/Clean/full_data.csv")


full_data %>%
  distinct(article_title, .keep_all = TRUE) %>% 
  group_by(news_source) %>%
  summarise(article_title = n()) %>%
  arrange(desc(article_title))

