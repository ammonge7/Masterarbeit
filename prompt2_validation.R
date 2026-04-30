#loading relevant libraries for data cleaning, processing, and analysis
library(tidyverse)
library(dplyr)
library(ellmer)
library(irr)
library(stringr)

#reading in all data: raw, random sample for validation, comparison thus far, and human rating
data_all <- read_csv("1_Data/data_all.csv")
data_comments_validation <- read_csv("1_Data/data_comments_validation.csv")
val_comments_comparison <- read_csv("1_Data/val_comments_comparison.csv")
my_code <- read_csv("1_Data/data_comments_validation_mycode.csv")

#prompt for the LLM
prompt_valence_template_c <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels. Only use labels provided. Do not make up new ones.

Scale
-3: Strongly against non-nutritive sweeteners
-2: Somewhat against non-nutritive sweeteners
-1: Slightly against non-nutritive sweeteners
0: Neutral
1: Slightly pro non-nutritive sweeteners
2: Somewhat pro non-nutritive sweeteners
3: Strongly pro non-nutritive sweeteners
N/A: not able to determine

Expected Output Format
Print the value assigned according to the scale.

Example 1
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Cada quien debería decidir lo que consume.

Example 1 Output: 0

Example 2
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Los transgenéticos son puro veneno.

Example 2 Output: -3
---"
#identifying number of rows of data to expect, to prepare for the data to be collected
n <- nrow(val_comments_comparison)

#creating a vector for the LLM responses to be stored
response_val_valence_c <- vector("character", n)

#the following loop will process each comment separately as a fresh request to
#avoid the LLM using past requests to inform its analysis
for (i in seq_len(n)) {
  # creating and labeling the chat to interact with openai api
  chat_val_valence_c <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                                  model = "gpt-5-mini")
  prompt_val_valence_c <- paste0(
    prompt_valence_template_c, "\n",
    "Article Title: ", data_comments_validation$article_title[i], "\n",
    "Article Caption: ", data_comments_validation$caption[i], "\n",
    "Comment: ", data_comments_validation$comment[i]
  )
  response_val_valence_c[i] <- chat_val_valence_c$chat(prompt_val_valence_c, echo = "none")
  #to monitor progress  
    cat(paste(i, "\n"))
}

#experienced some issues here:
val_comments_comparison <- val_comments_comparison %>% 
  mutate(llm_valence = response_val_valence_c) 
val_comments_comparison <- bind_cols(
  val_comments_comparison,
  my_code["human_rank"]
)
#saving data locally
write_csv(val_comments_comparison, "1_Data/val_comments_comparison.csv")

# Load the data fresh
val_comments_comparison <- read_csv("1_Data/val_comments_comparison.csv")

# Fix the typo + clean the NA / "-" cases
val_comments_comparison <- val_comments_comparison %>% 
  mutate(
    human_rank = if_else(human_rank == "-", NA_character_, human_rank),
    human_rank = if_else(human_rank == "N/A", NA_character_, human_rank),
    llm_valence = if_else(llm_valence == "N/A", NA_character_, llm_valence),
    llm_valence = if_else(llm_valence == "-", NA_character_, llm_valence)
  )

# Convert to numeric (this should now work)
val_comments_comparison <- val_comments_comparison %>%  
  mutate(
    human_rank_num = as.numeric(human_rank),
    llm_valence_num = as.numeric(llm_valence)
  )

class(val_comments_comparison$human_rank_num)
class(val_comments_comparison$llm_valence_num)

names(val_comments_comparison)

#conducting a Spearman Correlation to compare valence ratings
rating_corr <- cor.test(
  val_comments_comparison$human_rank_num,
  val_comments_comparison$llm_valence_num,
  method = "spearman",
  exact = FALSE,
  use = "pairwise.complete.obs"
)
print(rating_corr)
corr_val <- rating_corr$estimate
corr_text <- paste0("r = ", round(corr_val, 2))

#creating ggplot scatter
#first, loading library for plots
library(ggplot2)
#tried a heatmap
heatmap <- ggplot(data=val_comments_comparison, 
                  aes(x=human_rank_num, 
                      y =llm_valence_num))+
  geom_bin2d(bins=10)+
  scale_fill_continuous(type = "viridis")+
  labs(
    x = "Human Rating",
    y = "LLM Rating",
    fill = "Count"
  )+
  theme_minimal()

print(heatmap)
#didn't like it

#tried a jitter plot with a line of regression
ggplot(data=val_comments_comparison,
                  aes(x=human_rank_num,
                      y=llm_valence_num))+
  geom_jitter(height=0.5, width=0.5, color = "darkblue")+
  geom_smooth(method = "lm", se = FALSE, color = "lightblue")+ #to create line
  scale_x_continuous(breaks = -3:3, limits = c(-3,3)) +  # show -3, -2, -1, 0, 1, 2, 3
  scale_y_continuous(breaks = -3:3, limits = c(-3,3)) +  
  annotate("text", x = 0.25, y = -1, label = corr_text, size = 4, hjust = 1)+
  labs(
    x="Human Valence Rating",
    y="LLM Valence Rating"
    )+
  theme_minimal()+
  theme(
    text = element_text(family = "serif"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", height = 0.5, width = 0.5),
    axis.text.x = element_text(size = 12, color = "black", margin = margin (t =10)),
    axis.text.y = element_text(size = 12, color = "black", margin = margin (t =10)),
    axis.title.y = element_text(size = 14, margin = margin(t = 10)),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "none"
  ) 
ggsave("4_Results_and_plots/agreement_rating2.png", width = 6, height = 5)

sum(val_comments_comparison$human_rank == 3, na.rm = TRUE)

sum(val_comments_comparison$llm_valence == 3, na.rm = TRUE)

sum(is.na(val_comments_comparison$human_rank_num))
sum(is.na(val_comments_comparison$llm_valence_num))

sum(val_comments_comparison$human_rank_num < -3 |
      val_comments_comparison$human_rank_num > 3)

