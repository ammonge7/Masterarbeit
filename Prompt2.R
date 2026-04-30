library(tidyverse)
library(dplyr)

comments <- read_csv("1_Data/Clean/comments.csv")
replies <- read_csv("1_Data/Clean/replies.csv")
missing_rows <- read_csv("1_Data/Clean/missing_rows.csv")

library(ellmer)

prompt_valence_template_c <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels. Only use labels provided. Do not make up new ones.
•	Do not remove duplicates

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
Print the row ID along with value assigned according to the scale.

Example 1
Row ID: 83
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Cada quien debería decidir lo que consume.

Example 1 Output: 83: 0

Example 2
Row ID: 478
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Los transgenéticos son puro veneno.

Example 2 Output: 478: -3
---"

n <- nrow(comments)

response_valence_c <- vector("character", n)

for (i in seq_len(n)) {
  # creating and labeling the chat to interact with openai api
  chat_valence_c <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                                    model = "gpt-5-mini")
  prompt_valence_c <- paste0(
    prompt_valence_template_c, "\n",
    "Row ID: ", comments$row_id[i], "\n",
    "Article Title: ", comments$article_title[i], "\n",
    "Article Caption: ", comments$caption[i], "\n",
    "Comment: ", comments$comment[i]
  )
  response_valence_c[i] <- chat_valence_c$chat(prompt_valence_c, echo = "none")
  cat(sprintf("\rProcessing %d / %d", i, n))
  flush.console()
}

response_valence_comments <- tibble (label=response_valence_c)
write_csv(response_valence_comments, "3_Responses/v2/response_valence_comments.csv")

#---------------------------------------------------------------------------------
#analysing missing rows

n_m <- nrow(missing_rows)  
response_valence_cm <- vector("character", n_m)


for (i in seq_len(n_m)) {
  # creating and labeling the chat to interact with openai api
  chat_valence_cm <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                               model = "gpt-5-mini")
  prompt_valence_cm <- paste0(
    prompt_valence_template_c, "\n",
    "Row ID: ", missing_rows$row_id[i], "\n",
    "Article Title: ", missing_rows$article_title[i], "\n",
    "Article Caption: ", missing_rows$caption[i], "\n",
    "Comment: ", missing_rows$comment[i]
  )
  response_valence_cm[i] <- chat_valence_cm$chat(prompt_valence_cm, echo = "none")
  cat(sprintf("\rProcessing %d / %d", i, n_m))
  flush.console()
}


response_valence_comments_m <- tibble (label=response_valence_cm)
write_csv(response_valence_comments_m, "3_Responses/v2/response_valence_comments_m.csv")

#---------------------------------------------------------------------------------
#replies only


MAX_CHARS <- 5000

truncate <- function(x) {
  if (is.na(x)) return(x)
  if (nchar(x) > MAX_CHARS) substr(x, 1, MAX_CHARS) else x
}

prompt_valence_template_r <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels. Only use labels provided. Do not make up new ones.
•	Do not remove duplicates

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
Print the row ID along with value assigned according to the scale.

Example 1
Row ID: 83
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Cada quien debería decidir lo que consume.
Reply: No estoy de acuerdo, necesitamos ayuda del gobierno para tomar mejores decisiones con el consumo de azucar.

Example 1 Output: 83: 0

Example 2
Row ID: 478
Article title: Nuevo impuesto para azucar ha subido el consumo de los edulcorantes
Article Caption: 
Comment: Los transgenéticos son puro veneno.
Reply: Si, pero son menos adictivos que el azucar!

Example 2 Output: 478: -1
---"

n_r <- nrow(replies)

response_valence_r <- vector("character", n_r)

for (i in seq_len(n_r)) {
  # creating and labeling the chat to interact with openai api
  chat_valence_r <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                                model = "gpt-5-mini")
  prompt_valence_r <- paste0(
    prompt_valence_template_r, "\n",
    "Row ID: ", replies$row_id[i], "\n",
    "Article Title: ", replies$article_title[i], "\n",
    "Article Caption: ", replies$caption[i], "\n",
    "Comment: ", replies$comment[i], "\n",
    "Reply: ",  truncate(replies$comment[i]), "\n"
  )
  response_valence_r[i] <- chat_valence_r$chat(prompt_valence_r, echo = "none")
  cat(sprintf("\rProcessing %d / %d", i, n_r))
  flush.console()
}

response_valence_replies <- tibble (label=response_valence_r)
write_csv(response_valence_replies, "3_Responses/v2/response_valence_replies.csv")


library(ggplot2)
library(tidyr)
library(tidyverse)  # dplyr, tidyr, ggplot2, readr
library(janitor)    # frequency tables
library(scales)     # percentages in plots
library(dplyr)

valence_comments_raw <- read_csv("3_Responses/v2/response_valence_comments.csv")
valence_replies_raw <- read_csv("3_Responses/v2/response_valence_replies.csv")
valence_missing_rows_raw <- read_csv("3_Responses/v2/response_valence_comments_m.csv")
full_data <- read_csv("1_Data/Clean/full_data.csv")

valence_all_raw <- valence_comments_raw %>% 
  bind_rows(valence_missing_rows_raw) %>% 
  bind_rows(valence_replies_raw)
nrow(valence_all_raw)

valence_all_clean <- valence_all_raw %>% 
  separate(label, into = c("row_id", "valence"), sep = ":", extra = "merge") %>% 
  mutate(valence = str_replace_all(valence, " ", "")) %>% 
  mutate(valence = if_else(is.na(valence) | valence == "", "__EMPTY__", valence))%>% 
  select(-any_of("__EMPTY__")) %>% 
  mutate(row_id = as.numeric(row_id)) %>% 
  mutate(valence = as.numeric(valence))
nrow(valence_all_clean)


valence_full <- left_join(valence_all_clean, full_data, by="row_id")

nrow(valence_full)

ggplot(valence_full, aes(x = valence_all_clean$valence)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ country)

par(family = "serif")
hist(valence_full$valence,
     breaks = seq(-3.5, 3.5, by = 1),
     main = "",
     xlab = "Valence",
     ylab = "Frequency",
     col = "cyan4",
     Border = "white"
     )

library(ggplot2)
ggplot(valence_full, aes(x = valence)) +
  geom_histogram(
    binwidth = 1,
    center = 0,           # center bins on integer 0
    fill = "cyan4",
    color = "black",
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = -3:3,        # tick positions
    labels = -3:3         # labels at bin centers
  ) +
  labs(x = "Valence", y = "Frequency") +
  theme_minimal()+
  theme (
    text = element_text(family = "serif"),
    axis.text.x=element_text(size = 10, color= "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 14, margin = margin (t = 8)),
    axis.title.y = element_text(size = 14, margin = margin (r =8)),
    panel.grid = element_blank ()
  )

ggsave("4_Results_and_plots/valence_histogram.png")

wilcox.test(valence ~ country, data = valence_full)
#wilcoxon effect size (rank biserial effect size approximation)
#Apparently this isn't the best way to do it but think about it
r <- qnorm(0.6973/2) / sqrt(1945)
r
aggregate(valence ~ country, valence_full, median)
