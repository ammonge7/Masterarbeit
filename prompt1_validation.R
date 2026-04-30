#load relevant libraries for data management
library(tidyverse)
library(dplyr)

#read in comments collected from FaceBook
data_all <- read_csv("1_Data/data_all.csv")

#Separate comments as the prompt context will be different for replies
data_comments <- data_all %>% 
  select(-reply) %>% 
  distinct()

#randomly select a sample of comments to use for validation, 
#10% of total data set
data_comments_validation <- slice_sample(data_comments, 
                                         n = 194, replace = FALSE)

#save as a subset of data
write_csv(data_comments_validation, file = "1_Data/data_comments_validation.csv")

#load library for interaction with OpenAI's API
library(ellmer)

#prompt for the LLM
prompt_topic_template_c <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels. Only use labels provided. Do not make up new ones.

Labels
NNS : is referring to non-nutritive sweeteners
Sugar : is referring to sugar
No : is not referring to either

Expected Output Format
Print a list of labels separated by commas.

Example 1
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Cada quien debería decidir lo que consume.

Example 1 Output: NNS, Sugar

Example 2
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Los transgenéticos son puro veneno.

Example 2 Output: NNS

---
"

#identifying number of rows of data to expect, to prepare for the data to be collected
n <- nrow(data_comments_validation)

#creating a vector for the LLM responses to be stored
response_val_topic_c <- vector("character", n)

#the following loop will process each comment separately as a fresh request to
#avoid the LLM using past requests to inform its analysis
for (i in seq_len(n)) {
  # creating and labeling the chat to interact with openai api
  chat_val_topic_c <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                            model = "gpt-5-mini")
  prompt_val_topic_c <- paste0(
    prompt_topic_template_c, "\n",
    "Article Title: ", data_comments_validation$article_title[i], "\n",
    "Article Caption: ", data_comments_validation$caption[i], "\n",
    "Comment: ", data_comments_validation$comment[i]
  )
  response_val_topic_c[i] <- chat_val_topic_c$chat(prompt_val_topic_c, echo = "none")
#to monitor progress  
  cat(paste(i, "\n"))
}

#save the data locally
val_comments_labels <- tibble (label=response_val_topic_c)
write_csv(val_comments_labels, "1_Data/val_comments_labels.csv")

#read them back in
val_comments_labels <- read_csv("1_Data/val_comments_labels.csv")

#read in human ratings
human_ratings <- read_csv("1_Data/data_comments_validation_mycode.csv")

#combine human and LLM ratings to be comparable
validation_comparison <- human_ratings %>% 
  rename(human_ratings = ...8) %>% 
  bind_cols(val_comments_labels) %>% 
  rename(llm_ratings = label) %>% 
  mutate(human_ratings = if_else(human_ratings == "Sugar, NNS"| human_ratings == "Sugar,NNS", "NNS, Sugar", human_ratings))

#testing out agreement as an initial test
#this showed whether we selected the same combination of labels
validation_comparison <- validation_comparison %>% 
  mutate(agreement = human_ratings == llm_ratings)

validation_comparison %>% summarise(mean(agreement))

#create individual variables of human and LLM ratings to compare by kappa value
human_topic_val <- factor(validation_comparison$human_ratings)
llm_topic_val <- factor(validation_comparison$llm_ratings)

#exploring kappa analysis
install.packages("irr")
library(irr)
kappa2(validation_comparison[, c("human_ratings", "llm_ratings")])

install.packages("stringr")
library(stringr)

#further cleaning data to analyse agreement on each label category individually
cats <- c("Sugar", "NNS", "No")
validation_comparison <- validation_comparison %>% 
  mutate(across(c(human_ratings, llm_ratings),
                ~ str_replace_all(., " ", ""))) %>%
  separate_rows(human_ratings, sep = ",") %>%
  separate_rows(llm_ratings, sep = ",") %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = human_ratings,
    values_from = value,
    values_fill = 0,
    names_prefix = "human_"
  ) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = llm_ratings,
    values_from = value,
    values_fill = 0,
    names_prefix = "llm_"
  ) %>%
  mutate(across(all_of(paste0("human_", cats)), ~ replace_na(., 0))) %>%
  mutate(across(all_of(paste0("llm_", cats)), ~ replace_na(., 0)))

write_csv(validation_comparison, "1_Data/val_comments_comparison.csv")

#comparing via kappa
kappa2(validation_comparison[, c("human_NNS", "llm_NNS")])

kappa2(validation_comparison[, c("human_No", "llm_No")])

kappa2(validation_comparison[, c("human_Sugar", "llm_Sugar")])

#creating a jitter plot on agreement in each category 
val_comments_comparison <- read_csv("1_Data/val_comments_comparison.csv")
library(ggplot2)
ggplot(data=validation_comparison) +
  geom_jitter(mapping=aes(x= human_NNS, y = llm_NNS))
ggsave("topic_agreement_jitter.png", width = 5, height = 5)

#creating a variable of level of agreement for each category
agreement_nns <- mean(val_comments_comparison$human_NNS== val_comments_comparison$llm_NNS, na.rm=TRUE) *100

agreement_sugar <- mean(val_comments_comparison$human_Sugar== val_comments_comparison$llm_Sugar, na.rm=TRUE) *100

agreement_no <- mean(val_comments_comparison$human_No== val_comments_comparison$llm_No, na.rm=TRUE) *100

# organising agreement levels to be used in a bar chart
plot_frame <- data.frame(
  category = c("Sugar", "Non-Nutritive Sweeteners", "Neither"),
  agreement = c(agreement_sugar, agreement_nns, agreement_no)
)

# set the order of categories
plot_frame$category <- reorder(plot_frame$category, -plot_frame$agreement)

# creating bar chart
ggplot(plot_frame, aes(x = category, y = agreement, fill = category)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  geom_text(aes(label = sprintf("%d%%", round(agreement))),
            vjust = -0.5,
            size = 3.5) +
  scale_fill_manual(values = c(
    "Sugar" = "mediumblue",
    "Non-Nutritive Sweeteners" = "darkblue",
    "Neither" = "lightblue"
  )) +
  labs(
    x = "Topic Classification",
    y = "Percentage Agreement (%)"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12, margin = margin(t = -8), color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin (t = 8)),
    legend.position = "none"
  )
 

ggsave("4_Results_and_plots/Topic_Percentage_Bar2.png", width = 6, height = 5)
