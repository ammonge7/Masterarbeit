library(tidyverse)
library(dplyr)

comments <- read_csv("1_Data/Clean/comments.csv")
replies <- read_csv("1_Data/Clean/replies.csv")
missing_rows <- read_csv("1_Data/Clean/missing_rows.csv")

library(ellmer)

prompt_theme_template_c <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels or none at all. Only use labels provided. Do not make up new ones.
•	Do not remove duplicates

Labels
“Natural”: mentioning honey, stevia, panela, or preference of natural substances to sweeten other than sugar
“Pro NNSs”
“Risk of NNSs”
“Risk of sugar consumption”
“TSESx”: Taxation against low SES
“TSES+”: Taxation helps low SES
“T+”: Taxation will reduce sugar consumption
“Tx”: Taxation will not reduce sugar consumption
“Distrust in food industry”
“Distrust in health system”
“Distrust in government”
“Distrust in media”


Expected Output Format
Print the row id with a list of themes separated by commas.

Example 1
Row ID: 83
Article title: Gaseosas, agua y cerveza sin alcohol que pasen del 5% de azÃºcar pagarÃ¡n 25% de ISC
Article Caption: ðŸš¨#AHORA | Bebidas que contengan entre 0,5 y 5 gramos de azÃºcar por cada 100 mililitros seguirÃ¡n pagando 17% de ISC. Si pasan de 6 gramos, ahora deberÃ¡n pagar 25%. Empresas deberÃ¡n bajar el azÃºcar a sus productos si quieren regresar al anterior rango.
Comment: Menos AZUCAR MAS SALUD.....MENOS DIABETIS MENOS CANCER...MENOS ENFERMOS

Example 1 Output: 83: Natural, Risk of Sugar Consumption, T+,  

Example 2
Row ID: 478
Article title: Una agencia de la OMS declararÃ¡ como posible cancerÃ­geno al popular edulcorante aspartamo, utilizado en la Coca-Cola light
Article Caption: ðŸ”´ Agencia de la OMS declararÃ¡ como posible cancerÃ­geno al popular edulcorante aspartamo, utilizado en la Coca-Cola light.
Comment: Hace siglos el Aspartamo es cancerÃ­geno jajajja

Example 2 Output: 478: Risk of NNS, Distrust in food industry, Distrust in health system
---"

n <- nrow(comments)


response_theme_c <- vector("character", n)

for (i in seq_len(n)) {
  # creating and labeling the chat to interact with openai api
  chat_theme_c <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                                  model = "gpt-5-mini")
  prompt_theme_c <- paste0(
    prompt_theme_template_c, "\n",
    "Row ID: ", comments$row_id[i], "\n",
    "Article Title: ", comments$article_title[i], "\n",
    "Article Caption: ", comments$caption[i], "\n",
    "Comment: ", comments$comment[i]
  )
  response_theme_c[i] <- tryCatch(
    chat_theme_c$chat(prompt_theme_c, echo = "none"),
    error = function(e) {
      Sys.sleep(5)
      tryCatch(
        chat_theme_c$chat(prompt_theme_c, echo = "none"),
        error = function(e2) {
          message("\nFailed twice at row ", i)
          NA
        }
      )
    }
  )
  cat(sprintf("\rProcessing %d / %d", i, n))
  flush.console()
}

response_theme_comments <- tibble (label=response_theme_c)
write_csv(response_theme_comments, "3_Responses/v2/response_theme_comments.csv")

#---------------------------------------------------------------------------------
#analysing missing rows

n_m <- nrow(missing_rows)

response_theme_cm <- vector("character", n_m)


for (i in seq_len(n_m)) {
  # creating and labeling the chat to interact with openai api
  chat_theme_cm <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                                 model = "gpt-5-mini")
  prompt_theme_cm <- paste0(
    prompt_theme_template_c, "\n",
    "Row ID: ", missing_rows$row_id[i], "\n",
    "Article Title: ", missing_rows$article_title[i], "\n",
    "Article Caption: ", missing_rows$caption[i], "\n",
    "Comment: ", missing_rows$comment[i]
  )
  response_theme_cm[i] <-  tryCatch(
    chat_theme_cm$chat(prompt_theme_cm, echo = "none"),
    error = function(e) {
      Sys.sleep(5)
      tryCatch(
        chat_theme_cm$chat(prompt_theme_cm, echo = "none"),
        error = function(e2) {
          message("\nFailed twice at row ", i)
          NA
        }
      )
    }
  )
  cat(sprintf("\rProcessing %d / %d", i, n_m))
  flush.console()
}


response_theme_comments_m <- tibble (label=response_theme_cm)
write_csv(response_theme_comments_m, "3_Responses/v2/response_theme_comments_m.csv")


#---------------------------------------------------------------------------------
#replies only

MAX_CHARS <- 5000

truncate <- function(x) {
  if (is.na(x)) return(x)
  if (nchar(x) > MAX_CHARS) substr(x, 1, MAX_CHARS) else x
}

prompt_theme_template_r <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels or none at all. Only use labels provided. Do not make up new ones.
•	Do not remove duplicates

Labels
“Natural”: mentioning honey, stevia, panela, or preference of natural substances to sweeten other than sugar
“Pro NNSs”
“Risk of NNSs”
“Risk of sugar consumption”
“TSESx”: Taxation against low SES
“TSES+”: Taxation helps low SES
“T+”: Taxation will reduce sugar consumption
“Tx”: Taxation will not reduce sugar consumption
“Distrust in food industry”
“Distrust in health system”
“Distrust in government”
“Distrust in media”


Expected Output Format
Print the row id with a list of themes separated by commas.

Example 1
Row ID: 83
Article title: Gaseosas, agua y cerveza sin alcohol que pasen del 5% de azÃºcar pagarÃ¡n 25% de ISC
Article Caption: ðŸš¨#AHORA | Bebidas que contengan entre 0,5 y 5 gramos de azÃºcar por cada 100 mililitros seguirÃ¡n pagando 17% de ISC. Si pasan de 6 gramos, ahora deberÃ¡n pagar 25%. Empresas deberÃ¡n bajar el azÃºcar a sus productos si quieren regresar al anterior rango.
Comment: Menos AZUCAR MAS SALUD.....MENOS DIABETIS MENOS CANCER...MENOS ENFERMOS
Reply: Pero el gobierno no debe controlar nuestra comida

Example 1 Output: 83: Tx, Distrust in government

Example 2
Row ID: 478
Article title: Una agencia de la OMS declararÃ¡ como posible cancerÃ­geno al popular edulcorante aspartamo, utilizado en la Coca-Cola light
Article Caption: ðŸ”´ Agencia de la OMS declararÃ¡ como posible cancerÃ­geno al popular edulcorante aspartamo, utilizado en la Coca-Cola light.
Comment: Hace siglos el Aspartamo es cancerÃ­geno jajajja
Reply: No es cierto, esos investigaciones han probado falsos

Example 2 Output: 478: Pro NNS, Distrust in food industry, Distrust in health system
---"


n_r <- nrow(replies)


response_theme_r <- vector("character", n_r)

for (i in seq_len(n_r)) {
  # creating and labeling the chat to interact with openai api
  chat_theme_r <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                              model = "gpt-5-mini")
  prompt_theme_r <- paste0(
    prompt_theme_template_r, "\n",
    "Row ID: ", replies$row_id[i], "\n",
    "Article Title: ", replies$article_title[i], "\n",
    "Article Caption: ", replies$caption[i], "\n",
    "Comment: ", replies$comment[i]
  )
  response_theme_r[i] <- tryCatch(
    chat_theme_r$chat(prompt_theme_r, echo = "none"),
    error = function(e) {
      Sys.sleep(5)
      tryCatch(
        chat_theme_r$chat(prompt_theme_r, echo = "none"),
        error = function(e2) {
          message("\nFailed twice at row ", i)
          NA
        }
      )
    }
  )
  cat(sprintf("\rProcessing %d / %d", i, n_r))
  flush.console()
}

response_theme_replies <- tibble (label=response_theme_r)
write_csv(response_theme_replies, "3_Responses/v2/response_theme_replies.csv")


library(ggplot2)
library(tidyr)
library(tidyverse)  # dplyr, tidyr, ggplot2, readr
library(janitor)    # frequency tables
library(scales)     # percentages in plots

theme_comments_raw <- read_csv("3_Responses/v2/response_theme_comments.csv")
theme_replies_raw <- read_csv("3_Responses/v2/response_theme_replies.csv")
theme_missing_rows_raw <- read_csv("3_Responses/v2/response_theme_comments_m.csv")

theme_all_raw <- theme_comments_raw %>% 
  bind_rows(theme_missing_rows_raw) %>% 
  bind_rows(theme_replies_raw)

nrow(theme_all_raw)


theme_all_clean <- theme_all_raw %>% 
  mutate(label = str_remove(label, regex("^Row ID[: ]*", ignore_case = TRUE))) %>%
  separate(label, into = c("row_id", "label"), sep = ":") %>% 
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
  select(-any_of("__EMPTY__")) %>% 
  select(
    row_id,
    "Risk_of_Sugar_Consumption" = RiskofSugarConsumption,
    "Risk_of_NNSs" = RiskofNNSs,
    "Pro_NNSs" = ProNNSs,
    "Distrust_in_Media" = Distrustinmedia,
    "Natural" = Natural,
    "T+" = "T+",
    "Tx" = "Tx",
    "TSES+" = "TSES+",
    "TSESx" = "TSESx",
    "Distrust_in_Health_System" = Distrustinhealthsystem,
    "Distrust_in_Food_Industry" = Distrustinfoodindustry,
    "Distrust_in_Government" = Distrustingovernment
  )

nrow(theme_all_clean)


theme_full <- left_join(theme_all_clean, full_data, by = "row_id") %>% 
  select(-match)
nrow(theme_full)
colnames(theme_full)
write_csv(theme_full, "3_Responses/v2/theme_full.csv")

theme_full <- read_csv("3_Responses/v2/theme_full.csv")

install.packages("rstatix")
library(rstatix)
#RISK OF SUGAR
risk_sugar_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    risk_sugar_yes = sum(Risk_of_Sugar_Consumption, na.rm = TRUE),
    Total = n(),
    risk_sugar_no = Total - risk_sugar_yes,
    .groups = "drop"
  )

risk_sugar_counts

risk_sugar_table <- risk_sugar_counts %>%
  select(country, risk_sugar_yes, risk_sugar_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

risk_sugar_table
chisq.test(risk_sugar_table)
cramer_v(risk_sugar_table)

#RISK OF NNS
risk_nns_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    risk_nns_yes = sum(Risk_of_NNSs, na.rm = TRUE),
    Total = n(),
    risk_nns_no = Total - risk_nns_yes,
    .groups = "drop"
  )

risk_nns_counts

risk_nns_table <- risk_nns_counts %>%
  select(country, risk_nns_yes, risk_nns_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

risk_nns_table
chisq.test(risk_nns_table)
cramer_v(risk_nns_table)

#PRO NNS
pro_nns_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    pro_nns_yes = sum(Pro_NNSs, na.rm = TRUE),
    Total = n(),
    pro_nns_no = Total - pro_nns_yes,
    .groups = "drop"
  )

pro_nns_counts

pro_nns_table <- pro_nns_counts %>%
  select(country, pro_nns_yes, pro_nns_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

pro_nns_table
chisq.test(pro_nns_table)
cramer_v(pro_nns_table)

#DISTRUST IN MEDIA
distrust_media_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    distrust_media_yes = sum(Distrust_in_Media, na.rm = TRUE),
    Total = n(),
    distrust_media_no = Total - distrust_media_yes,
    .groups = "drop"
  )

distrust_media_counts

distrust_media_table <- distrust_media_counts %>%
  select(country, distrust_media_yes, distrust_media_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

distrust_media_table
chisq.test(distrust_media_table)
cramer_v(distrust_media_table)

#NATURAL
natural_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    natural_yes = sum(Natural, na.rm = TRUE),
    Total = n(),
    natural_no = Total - natural_yes,
    .groups = "drop"
  )

natural_counts

natural_table <- natural_counts %>%
  select(country, natural_yes, natural_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

natural_table
chisq.test(natural_table)
cramer_v(natural_table)


#Tax+
T_pos_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    T_pos_yes = sum(`T+`, na.rm = TRUE),
    Total = n(),
    T_pos_no = Total - T_pos_yes,
    .groups = "drop"
  )

T_pos_counts

T_pos_table <- T_pos_counts %>%
  select(country, T_pos_yes, T_pos_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

T_pos_table
chisq.test(T_pos_table)
cramer_v(T_pos_table)


#Tax negative
T_neg_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    T_neg_yes = sum(Tx, na.rm = TRUE),
    Total = n(),
    T_neg_no = Total - T_neg_yes,
    .groups = "drop"
  )

T_neg_counts

T_neg_table <- T_neg_counts %>%
  select(country, T_neg_yes, T_neg_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

T_neg_table
chisq.test(T_neg_table)
cramer_v(T_neg_table)

#TSES+
SES_pos_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    SES_pos_yes = sum(`TSES+`, na.rm = TRUE),
    Total = n(),
    SES_pos_no = Total - SES_pos_yes,
    .groups = "drop"
  )

SES_pos_counts

SES_pos_table <- SES_pos_counts %>%
  select(country, SES_pos_yes, SES_pos_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

SES_pos_table
chisq.test(SES_pos_table)
cramer_v(SES_pos_table)

#TSES neg
SES_neg_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    SES_neg_yes = sum(TSESx, na.rm = TRUE),
    Total = n(),
    SES_neg_no = Total - SES_neg_yes,
    .groups = "drop"
  )

SES_neg_counts

SES_neg_table <- SES_neg_counts %>%
  select(country, SES_neg_yes, SES_neg_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

SES_neg_table
chisq.test(SES_neg_table)
cramer_v(SES_neg_table)

#DISTRUST IN HEALTH SYSTEM
distrust_health_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    distrust_health_yes = sum(Distrust_in_Health_System, na.rm = TRUE),
    Total = n(),
    distrust_health_no = Total - distrust_health_yes,
    .groups = "drop"
  )

distrust_health_counts

distrust_health_table <- distrust_health_counts %>%
  select(country, distrust_health_yes, distrust_health_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

distrust_health_table
chisq.test(distrust_health_table)
cramer_v(distrust_health_table)

#DISTRUST IN FOOD INDUSTRY
distrust_food_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    distrust_food_yes = sum(Distrust_in_Food_Industry, na.rm = TRUE),
    Total = n(),
    distrust_food_no = Total - distrust_food_yes,
    .groups = "drop"
  )

distrust_food_counts

distrust_food_table <- distrust_food_counts %>%
  select(country, distrust_food_yes, distrust_food_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

distrust_food_table
chisq.test(distrust_food_table)
cramer_v(distrust_food_table)



#DISTRUST IN GOVERNMENT
distrust_govt_counts <- theme_full %>%
  filter(country %in% c("Peru", "Colombia")) %>%
  group_by(country) %>%
  summarise(
    distrust_govt_yes = sum(Distrust_in_Government, na.rm = TRUE),
    Total = n(),
    distrust_govt_no = Total - distrust_govt_yes,
    .groups = "drop"
  )

distrust_govt_counts

distrust_govt_table <- distrust_govt_counts %>%
  select(country, distrust_govt_yes, distrust_govt_no) %>%
  column_to_rownames("country") %>%
  as.matrix()

distrust_govt_table
chisq.test(distrust_govt_table)
cramer_v(distrust_govt_table)

colnames(theme_full)

library(DescTools)

themes <- c("Risk_of_Sugar_Consumption","Risk_of_NNSs","Pro_NNSs","Distrust_in_Media",
            "Natural","T+","Tx","TSES+", "TSESx", "Distrust_in_Health_System",
            "Distrust_in_Food_Industry","Distrust_in_Government")

# Overall, are there differences?
CochranQTest(as.matrix(theme_full[, themes]))

# Pairwise comparisons between specific themes
pairwise_results <- combn(themes, 2, function(x) {
  test <- mcnemar.test(table(theme_full[[x[1]]],
                             theme_full[[x[2]]]))
  c(theme1 = x[1],
    theme2 = x[2],
    p = test$p.value)
})

pairwise_results <- combn(themes, 2, function(x) {
  test <- mcnemar.test(table(theme_full[[x[1]]],
                             theme_full[[x[2]]]))
  c(theme1 = x[1],
    theme2 = x[2],
    p = test$p.value)
})

pairwise_results <- t(pairwise_results)
pairwise_results <- as.data.frame(pairwise_results)

# Adjust for multiple comparisons
pairwise_results$p_adjusted <- p.adjust(
  as.numeric(pairwise_results$V3),
  method = "bonferroni"
)

write_csv(pairwise_results, "4_Results_and_plots/theme_pairwise_comparisons.csv")
#think about what to do with these...



#actually, try this within theme categories or organise them that way at least



themes_attitude <- c(
  "Risk_of_Sugar_Consumption",
  "Risk_of_NNSs",
  "Pro_NNSs",
  "Natural"
)

attitude_data <- theme_full %>%
  select(all_of(themes_attitude)) %>%
  mutate(across(everything(), ~ifelse(. > 0, 1, 0))) %>%
  drop_na()

CochranQTest(as.matrix(attitude_data))

pairwise_attitude <- combn(themes_attitude, 2, function(x) {
  
  dat <- attitude_data[, x]
  
  test <- mcnemar.test(table(dat[[1]], dat[[2]]))
  
  c(theme1 = x[1],
    theme2 = x[2],
    p = test$p.value)
})

pairwise_attitude <- t(pairwise_attitude) |> as.data.frame()

pairwise_attitude$p_adjusted <- p.adjust(
  as.numeric(pairwise_attitude$V3),
  method = "bonferroni"
)
view(pairwise_attitude)

write_csv(pairwise_attitude, "4_Results_and_plots/pairwise_attitude.csv")

#TAX
themes_tax <- c(
  "T+",
  "Tx"
)

tax_data <- theme_full %>%
  select(all_of(themes_tax)) %>%
  mutate(across(everything(), ~ifelse(. > 0, 1, 0))) %>%
  drop_na()

tax_table <- table(tax_data[[1]], tax_data[[2]])
tax_test <- mcnemar.test(tax_table)

pairwise_tax <- data.frame(
  theme1 = themes_tax[1],
  theme2 = themes_tax[2],
  p = tax_test$p.value,
  p_adjusted = tax_test$p.value  # Bonferroni not needed for single pair
)

view(pairwise_tax)
write_csv(pairwise_tax, "4_Results_and_plots/pairwise_tax.csv")

#SES
themes_ses <- c(
  "TSES+",
  "TSESx"
)

ses_data <- theme_full %>%
  select(all_of(themes_ses)) %>%
  mutate(across(everything(), ~ifelse(. > 0, 1, 0))) %>%
  drop_na()

ses_table <- table(ses_data[[1]], ses_data[[2]])
ses_test <- mcnemar.test(ses_table)

pairwise_ses <- data.frame(
  theme1 = themes_ses[1],
  theme2 = themes_ses[2],
  p = ses_test$p.value,
  p_adjusted = ses_test$p.value
)

view(pairwise_ses)
write_csv(pairwise_ses, "4_Results_and_plots/pairwise_ses.csv")

#DISTRUST
themes_distrust <- c(
  "Distrust_in_Media",
  "Distrust_in_Health_System",
  "Distrust_in_Food_Industry",
  "Distrust_in_Government"
)

distrust_data <- theme_full %>%
  select(all_of(themes_distrust)) %>%
  mutate(across(everything(), ~ifelse(. > 0, 1, 0))) %>%
  drop_na()

CochranQTest(as.matrix(distrust_data))

pairwise_distrust <- combn(themes_distrust, 2, function(x) {
  
  dat <- distrust_data[, x]
  
  test <- mcnemar.test(table(dat[[1]], dat[[2]]))
  
  c(theme1 = x[1],
    theme2 = x[2],
    p = test$p.value)
})

pairwise_distrust <- t(pairwise_distrust) |> as.data.frame()

pairwise_distrust$p_adjusted <- p.adjust(
  as.numeric(pairwise_distrust$V3),
  method = "bonferroni"
)
view(pairwise_distrust)
write_csv(pairwise_distrust, "4_Results_and_plots/pairwise_distrust.csv")


table(theme_full$Risk_of_Sugar_Consumption, theme_full$Risk_of_NNSs)
