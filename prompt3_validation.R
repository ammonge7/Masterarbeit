#loading relevant libraries for data cleaning, processing, and analysis
library(tidyverse)
library(dplyr)
library(ellmer)
library(irr)
library(stringr)

#reading in all data: raw, random sample for validation, comparison thus far, and human rating
data_all <- read_csv("1_Data/data_all.csv")
dataset_comments_validation <- read_csv("1_Data/data_comments_validation.csv")
val_comments_comparison <- read_csv("1_Data/val_comments_comparison.csv")
my_code <- read_csv("1_Data/data_comments_validation_mycode.csv")

#prompt for the LLM
prompt_theme_template_c <- "You are tasked with conducting a content analysis of Facebook comments written in Spanish. 

Instructions
•	Read the comment and interpret the overall message and sentiment it conveys
•	Analyse comments that are labelled as replies with consideration of the context of the original comment they are responding to
•	Consider the article title as context for the comment.
•	Then, assign any applicable labels from the options. A comment may fit into multiple labels or none at all. Only use labels provided. Do not make up new ones.

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
Print a list of themes separated by commas.

Example 1
Article title: Gaseosas, agua y cerveza sin alcohol que pasen del 5% de azÃºcar pagarÃ¡n 25% de ISC
Article Caption: ðŸš¨#AHORA | Bebidas que contengan entre 0,5 y 5 gramos de azÃºcar por cada 100 mililitros seguirÃ¡n pagando 17% de ISC. Si pasan de 6 gramos, ahora deberÃ¡n pagar 25%. Empresas deberÃ¡n bajar el azÃºcar a sus productos si quieren regresar al anterior rango.
Comment: Menos AZUCAR MAS SALUD.....MENOS DIABETIS MENOS CANCER...MENOS ENFERMOS

Example 1 Output: Natural, Risk of Sugar Consumption, T+,  

Example 2
Article title: Una agencia de la OMS declararÃ¡ como posible cancerÃ­geno al popular edulcorante aspartamo, utilizado en la Coca-Cola light
Article Caption: ðŸ”´ Agencia de la OMS declararÃ¡ como posible cancerÃ­geno al popular edulcorante aspartamo, utilizado en la Coca-Cola light.
Comment: Hace siglos el Aspartamo es cancerÃ­geno jajajja

Example 2 Output: Risk of NNS, Distrust in food industry, Distrust in health system
---"

#identifying number of rows of data to expect, to prepare for the data to be collected
n <- nrow(val_comments_comparison)


#creating a vector for the LLM responses to be stored
response_val_theme_c <- vector("character", n)

#the following loop will process each comment separately as a fresh request to
#avoid the LLM using past requests to inform its analysis
for (i in seq_len(n)) {
  # creating and labeling the chat to interact with openai api
  chat_val_theme_c <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                                    model = "gpt-5-mini")
  prompt_val_theme_c <- paste0(
    prompt_theme_template_c, "\n",
    "Article Title: ", data_comments_validation$article_title[i], "\n",
    "Article Caption: ", data_comments_validation$caption[i], "\n",
    "Comment: ", data_comments_validation$comment[i]
  )
  response_val_theme_c[i] <- chat_val_theme_c$chat(prompt_val_theme_c, echo = "none")
  #to monitor progress  
  cat(paste(i, "\n"))
}

#save data locally
write_csv(tibble(themes_raw = response_val_theme_c),
          "1_Data/response_val_theme_c_raw.csv")
response_val_theme_c <- read_csv("1_Data/response_val_theme_c_raw.csv")
#checking vector length
response_val_theme_c <- response_val_theme_c[1:nrow(val_comments_comparison)]

#replacing N/A or missing with empty string
response_val_theme_c[is.na(response_val_theme_c)] <- ""
response_val_theme_c[response_val_theme_c == "N/A"] <- ""

#making tibble for one-hot encoding
val_themes_cleaning <- tibble(themes_raw = response_val_theme_c)

# defining themes
all_themes <- c(
  "Natural", "Pro NNSs", "Risk of NNSs", 
  "Risk of Sugar Consumption", "TSESx", "TSES+", "T+", 
  "Tx", "Distrust in food industry", "Distrust in health system",
  "Distrust in government", "Distrust in media"
)

# one-hot encode
for (theme in all_themes) {
  colname <- paste0("LLM_", gsub(" ", "_", theme))
  val_themes_cleaning[[colname]] <- ifelse(
    str_detect(val_themes_cleaning$themes_raw, fixed(theme)),
    1,
    0
  )
}

#selecting only the 0/1 columns
theme_columns <- val_themes_cleaning %>% select(starts_with("LLM_"))

#binding to main table safely
val_comments_comparison <- bind_cols(val_comments_comparison, theme_columns)

#resaving locally
write_csv(val_comments_comparison, "1_Data/val_comments_comparison.csv")

val_comments_comparison <- read_csv("1_Data/val_comments_comparison.csv")
#incorporating human ratings into the comparison table
human_theme_columns <- my_code %>% select(human_natural, human_pro_nns, human_risk_of_nns, human_risk_sugar,human_TSESx, `human_TSES+`,`human_T+`, human_Tx, human_distrust_food, human_distrust_health, human_distrust_gov, human_distrust_media )

val_comments_comparison <- bind_cols(val_comments_comparison, human_theme_columns)
val_comments_comparison <- val_comments_comparison[, 1:40]
names(val_comments_comparison) <- str_replace(names(val_comments_comparison), "\\.+\\d*$", "")

#library for analysis via kappa
library(irr)

#making sure data is numeric to allow comparison
val_comments_comparison$LLM_Natural <- factor(val_comments_comparison$LLM_Natural)
val_comments_comparison$human_natural <- factor(val_comments_comparison$human_natural)

#calculating percentage agreement for each label category individually
agreement_natural <- mean(val_comments_comparison$human_natural== val_comments_comparison$LLM_Natural) *100
agreement_natural
kappa_natural <- kappa2(
  val_comments_comparison[, c("human_natural", "LLM_Natural")],
  weight = "unweighted"
)
agreement_pro_nns <- mean(val_comments_comparison$human_pro_nns== val_comments_comparison$LLM_Pro_NNSs) *100
agreement_pro_nns
kappa_pro_nns <- kappa2(
  val_comments_comparison[, c("human_pro_nns", "LLM_Pro_NNSs")],
  weight = "unweighted"
)
colnames(val_comments_comparison)
agreement_risk_of_nns <- mean(val_comments_comparison$human_risk_of_nns== val_comments_comparison$LLM_Risk_of_NNSs) *100
agreement_risk_of_nns
kappa_risk_of_nns <- kappa2(
  val_comments_comparison[, c("human_risk_of_nns", "LLM_Risk_of_NNSs")],
  weight = "unweighted"
)
agreement_risk_sugar <- mean(val_comments_comparison$human_risk_sugar== val_comments_comparison$LLM_Risk_of_Sugar_Consumption) *100
agreement_risk_sugar
kappa_risk_sugar <- kappa2(
  val_comments_comparison[, c("human_risk_sugar", "LLM_Risk_of_Sugar_Consumption")],
  weight = "unweighted"
)
agreement_TSESpl <- mean(val_comments_comparison$`human_TSES+`== val_comments_comparison$`LLM_TSES+`) *100
agreement_TSESpl
kappa_TSESpl <- kappa2(
  val_comments_comparison[, c("human_TSES+", "LLM_TSES+")],
  weight = "unweighted"
)
agreement_TSESx <- mean(val_comments_comparison$human_TSESx== val_comments_comparison$LLM_TSESx) *100
agreement_TSESx
kappa_TSESx <- kappa2(
  val_comments_comparison[, c("human_TSESx", "LLM_TSESx")],
  weight = "unweighted"
)
agreement_Tpl <- mean(val_comments_comparison$`human_T+`== val_comments_comparison$`LLM_T+`) *100
agreement_Tpl
kappa_Tpl <- kappa2(
  val_comments_comparison[, c("human_T+", "LLM_T+")],
  weight = "unweighted"
)

agreement_Tx <- mean(
  val_comments_comparison$human_Tx == val_comments_comparison$LLM_Tx, 
  na.rm = TRUE   #added in to cope with empty cells
) * 100
agreement_Tx
kappa_Tx <- kappa2(
  val_comments_comparison[, c("human_Tx", "LLM_Tx")],
  weight = "unweighted"
)

agreement_d_food <- mean(val_comments_comparison$human_distrust_food== val_comments_comparison$LLM_Distrust_in_food_industry) *100
agreement_d_food
kappa_d_food <- kappa2(
  val_comments_comparison[, c("human_distrust_food", "LLM_Distrust_in_food_industry")],
  weight = "unweighted"
)

agreement_d_health <- mean(val_comments_comparison$human_distrust_health== val_comments_comparison$LLM_Distrust_in_health_system) *100
agreement_d_health
kappa_d_health <- kappa2(
  val_comments_comparison[, c("human_distrust_health", "LLM_Distrust_in_health_system")],
  weight = "unweighted"
)
agreement_d_gov <- mean(
  val_comments_comparison$human_distrust_gov == val_comments_comparison$LLM_Distrust_in_government, 
  na.rm = TRUE
) * 100
agreement_d_gov
kappa_d_gov <- kappa2(
  val_comments_comparison[, c("human_distrust_gov", "LLM_Distrust_in_government")],
  weight = "unweighted"
)
agreement_d_media <- mean(val_comments_comparison$human_distrust_media== val_comments_comparison$LLM_Distrust_in_media) *100
agreement_d_media
kappa_d_media <- kappa2(
  val_comments_comparison[, c("human_distrust_media", "LLM_Distrust_in_media")],
  weight = "unweighted"
)

#organising percentage agreement by theme
bar2frame <- data.frame(
  category = c("Natural", "Pro NNS", "Risk of NNS", "Risk of Sugar Consumption",
               "TSES+","TSESx", "Pro Tax", "Against Tax", "Distrust in Food Industry", "Distrust in Health System", "Distrust in Government", "Distrust in Media"),
  agreement = c(agreement_natural, agreement_pro_nns, agreement_risk_of_nns,
                agreement_risk_sugar, agreement_TSESpl, agreement_TSESx, agreement_Tpl,
                agreement_Tx, agreement_d_food, agreement_d_health, agreement_d_gov, agreement_d_media)
)
bar2frame <- data.frame(
  category = c("Risk of Sugar Consumption", "Risk of Non-Nutritive Sweeteners", "Pro Non-Nutritive Sweeteners", "Natural",
               "For Taxation", "Against Taxation", "SES Benefit","SES Burden", "Distrust in Media", "Distrust in Health System", "Distrust in Food Industry", "Distrust in Government"),
  agreement = c(agreement_risk_sugar, agreement_risk_of_nns, agreement_pro_nns, agreement_natural, 
                agreement_Tpl, agreement_Tx, agreement_TSESpl, agreement_TSESx, 
                agreement_d_media, agreement_d_health, agreement_d_food, agreement_d_gov)
)
#setting up categories for labels
bar2frame$category <- factor(bar2frame$category,
                             levels = c("Risk of Sugar Consumption", "Risk of Non-Nutritive Sweeteners", "Pro Non-Nutritive Sweeteners", "Natural",
                                        "For Taxation", "Against Taxation", "SES Benefit","SES Burden", "Distrust in Media", "Distrust in Health System", "Distrust in Food Industry", "Distrust in Government"
                                        ))

#setting up broader categories for grouping of results
bar2frame$Category <- c(
  "Attitude", "Attitude", "Attitude", "Attitude", 
  "Opinion on Tax", "Opinion on Tax",
  "Impact on Low SES", "Impact on Low SES", 
  "Distrust", "Distrust", "Distrust", "Distrust"               
)

#creating the plot
ggplot(bar2frame, aes(x= category, y = agreement))+
  geom_col(aes(fill=Category))+
  scale_y_continuous(expand = expansion(mult = c(5, 5)))+
  geom_text(aes(label = sprintf("%d%%", round(agreement))),           # value to display
            vjust = -0.5,                     # vertical position above the bar
            size = 3)+                          # text size
  scale_fill_manual(values = c(
    "Attitude" = "cadetblue",
    "Impact on Low SES" = "cyan3",
    "Opinion on Tax" = "cadetblue2",          
    "Distrust" = "aquamarine3"      
  ),
  breaks = c("Attitude", "Opinion on Tax", "Impact on Low SES", "Distrust")) +
  labs(
    x = "Theme",
    y = "Percentage Agreement (%)"
  )+
  ylim(0,105)+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8, color = "black"),  
        axis.text.y = element_text(size = 10, color = "black"),                           
        axis.title.x = element_text(size = 14, margin = margin(t = 8)),
        axis.title.y = element_text(size = 12, margin = margin (r = 8))
  ) 
ggsave("4_Results_and_plots/_agreement_themes2.png", width = 6, height = 5)

#exploration
-----------------------------
kappa_natural <- kappa2(
  val_comments_comparison[, c("LLM_Natural", "human_natural")],
  weight = "unweighted"
)
kappa_natural

library(irr)

kappa2()


names(val_comments_comparison)
