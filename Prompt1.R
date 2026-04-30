library(tidyverse)
library(dplyr)

comments <- read_csv("1_Data/Clean/comments.csv")
replies <- read_csv("1_Data/Clean/replies.csv")
missing_rows <- read_csv("1_Data/Clean/missing_rows.csv")
full_data <- read_csv("1_Data/Clean/full_data.csv")

library(ellmer)

prompt_topic_template_c <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels. Only use labels provided. Do not make up new ones.
•	Do not remove duplicates

Labels
NNS : is referring to non-nutritive sweeteners
Sugar : is referring to sugar
No : is not referring to either

Expected Output Format
Print the row id with a list of labels separated by commas.

Example 1
Row ID: 83
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Cada quien debería decidir lo que consume.

Example 1 Output: 83: NNS, Sugar

Example 2
Row ID: 478
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Los transgenéticos son puro veneno.

Example 2 Output: 478: NNS

---
"


n <- nrow(comments)

response_topic_c <- vector("character", n)


for (i in seq_len(n)) {
  # creating and labeling the chat to interact with openai api
  chat_topic_c <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                              model = "gpt-5-mini")
  prompt_topic_c <- paste0(
    prompt_topic_template_c, "\n",
    "Row ID: ", comments$row_id[i], "\n",
    "Article Title: ", comments$article_title[i], "\n",
    "Article Caption: ", comments$caption[i], "\n",
    "Comment: ", comments$comment[i]
  )
  response_topic_c[i] <- chat_topic_c$chat(prompt_topic_c, echo = "none")
  cat(sprintf("\rProcessing %d / %d", i, n))
  flush.console()
}


response_topic_comments <- tibble (label=response_topic_c)
write_csv(response_topic_comments, "3_Responses/v2/response_topic_comments.csv")

---------------------------------------------------------------------------------
  #analysing missing rows
  
n_m <- nrow(missing_rows)

response_topic_cm <- vector("character", n_m)


for (i in seq_len(n_m)) {
  # creating and labeling the chat to interact with openai api
  chat_topic_cm <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                              model = "gpt-5-mini")
  prompt_topic_cm <- paste0(
    prompt_topic_template_c, "\n",
    "Row ID: ", missing_rows$row_id[i], "\n",
    "Article Title: ", missing_rows$article_title[i], "\n",
    "Article Caption: ", missing_rows$caption[i], "\n",
    "Comment: ", missing_rows$comment[i]
  )
  response_topic_cm[i] <- chat_topic_cm$chat(prompt_topic_cm, echo = "none")
  cat(sprintf("\rProcessing %d / %d", i, n_m))
  flush.console()
}


response_topic_comments_m <- tibble (label=response_topic_cm)
write_csv(response_topic_comments_m, "3_Responses/v2/response_topic_comments_m.csv")

---------------------------------------------------------------------------------
#replies only

library(ellmer)

prompt_topic_template_r <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title and comment as context for the reply.
•	Then, assign any applicable labels from the options. A reply may fit into multiple labels. Only use labels provided. Do not make up new ones.
•	Do not remove duplicates

Labels
NNS : is referring to non-nutritive sweeteners
Sugar : is referring to sugar
No : is not referring to either

Expected Output Format
Print the row id with a list of labels separated by commas.

Example 1
Row ID: 83
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Cada quien debería decidir lo que consume.
Reply: No estoy de acuerdo, necesitamos ayuda del gobierno para tomar mejores decisiones con el consumo de azucar.

Example 1 Output: 83: Sugar

Example 2
Row ID: 478
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Los transgenéticos son puro veneno.
Reply: Si, pero son menos adictivos que el azucar!

Example 2 Output: 478: NNS, Sugar

---
"


n_r <- nrow(replies)

response_topic_r <- vector("character", n_r)


for (i in seq_len(n_r)) {
  # creating and labeling the chat to interact with openai api
  chat_topic_r <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                              model = "gpt-5-mini")
  prompt_topic_r <- paste0(
    prompt_topic_template_r, "\n",
    "Row ID: ", replies$row_id[i], "\n",
    "Article Title: ", replies$article_title[i], "\n",
    "Article Caption: ", replies$caption[i], "\n",
    "Comment: ", replies$comment[i], "\n",
    "Reply: ", replies$reply[i]
  )
  response_topic_r[i] <- chat_topic_r$chat(prompt_topic_r, echo = "none")
  cat(sprintf("\rProcessing %d / %d", i, n_r))
  flush.console()
}


response_topic_replies <- tibble (label=response_topic_r)
write_csv(response_topic_replies, "3_Responses/v2/response_topic_replies.csv")

#---------------------------------------------------------------------------------

  
library(ggplot2)
library(tidyr)
library(tidyverse)  # dplyr, tidyr, ggplot2, readr
library(janitor)    # frequency tables
library(scales)     # percentages in plots

topic_comments_raw <- read_csv("3_Responses/v2/response_topic_comments.csv")
topic_replies_raw <- read_csv("3_Responses/v2/response_topic_replies.csv")
topic_missing_rows_raw <- read_csv("3_Responses/v2/response_topic_comments_m.csv")

topic_all_raw <- topic_comments_raw %>% 
  bind_rows(topic_missing_rows_raw) %>% 
  bind_rows(topic_replies_raw)

topic_all_clean <- topic_all_raw %>% 
  separate(label, into = c("row_id", "label"), sep = ":", extra = "merge") %>% 
  separate_rows(label, sep = ",") %>% 
  mutate(label = str_replace_all(label, " ", "")) %>% 
  mutate(label = if_else(is.na(label) | label == "", "__EMPTY__", label))%>%
  mutate(
    row_id = as.numeric(row_id),
    label = str_trim(label),
    value = 1
  ) %>% 
  pivot_wider(
    names_from = label,
    values_from = value,
    values_fill = 0
  ) %>% 
  select(-any_of("__EMPTY__"))

nrow(topic_all_clean)

topic_full <- left_join(topic_all_clean, full_data, by="row_id")

nrow(topic_full)

install.packages("rstatix")
library(rstatix)

sugar_counts <- topic_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    Sugar_yes = sum(Sugar, na.rm = TRUE),
    Total = n(),
    Sugar_no = Total - Sugar_yes,
    .groups = "drop"
  )

sugar_counts

sugar_table <- sugar_counts %>%
  select(country, Sugar_yes, Sugar_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

sugar_table
chisq.test(sugar_table)
cramer_v(sugar_table)


NNS_counts <- topic_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    NNS_yes = sum(NNS, na.rm = TRUE),
    Total = n(),
    NNS_no = Total - NNS_yes,
    .groups = "drop"
  )

NNS_counts

NNS_table <- NNS_counts %>%
  select(country, NNS_yes, NNS_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

NNS_table
chisq.test(NNS_table)
cramer_v(NNS_table)

No_counts <- topic_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    No_yes = sum(No, na.rm = TRUE),
    Total = n(),
    No_no = Total - No_yes,
    .groups = "drop"
  )

No_counts

No_table <- No_counts %>%
  select(country, No_yes, No_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

No_table
chisq.test(No_table)
cramer_v(No_table)
