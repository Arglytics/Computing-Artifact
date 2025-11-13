
# Load packages
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggwordcloud)
library(lsa)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)


#Word_cloud for BERTopic congress

df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Bertopic_keyword_counts_for_congress.csv", stringsAsFactors = FALSE)

head(df, 5)

set.seed(9)  # for reproducible layout

# Create word cloud plot with heading
plt1 <- ggplot(df, aes(label = keyword, size = count, color = count)) +
  geom_text_wordcloud(shape = "circle", area_corr_power = 1, rm_outside = TRUE) +
  scale_size_area(max_size = 15, guide = guide_legend(title = "Count")) +
  theme_minimal() +
  ggtitle("Keyword Frequency Word Cloud for Bertopic")   # <-- Add your desired heading here

plt1


# Theme to set the background to white
plt1 <- plt1 + theme(plot.background = element_rect(fill = "white", color = NA))

# Saving Plot as PNG with white background
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_bertopic_word_cloud.png",
  plt1,
  width = 14,
  height = 4,
  dpi = 300,
  bg = "white"      
)


#word_cloud for STM congress

# Load packages
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggwordcloud)
library(lsa)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/STM_keyword_counts_for_congress.csv", stringsAsFactors = FALSE)

head(df, 5)

set.seed(9)  # for reproducible layout

# Create word cloud plot with heading
plt2 <- ggplot(df, aes(label = keyword, size = count, color = count)) +
  geom_text_wordcloud(shape = "circle", area_corr_power = 1, rm_outside = TRUE) +
  scale_size_area(max_size = 15, guide = guide_legend(title = "Count")) +
  theme_minimal() +
  ggtitle("Keyword Frequency Word Cloud for STM")   # <-- Add your desired heading here

plt2


# Theme to set the background to white
plt2 <- plt2 + theme(plot.background = element_rect(fill = "white", color = NA))

# Saving Plot as PNG with white background
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_STM_word_cloud.png",
  plt2,
  width = 14,
  height = 4,
  dpi = 300,
  bg = "white"     
)


#Heatmap for stm and bertopic topics for wildlife news articles

library(pheatmap)
library(dplyr)
library(stringr)
library(tidyr)
library(coop)
library(pheatmap)

stm_df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_stm_topic_keywords_with_weights.csv", stringsAsFactors = FALSE)

bert_df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_bertopic_keywords_with_weights_01.csv", stringsAsFactors = FALSE)


# creating a similarity matrix for the *actual* topic counts (not fixed at 10x10)
set.seed(123)
cosine_mat <- matrix(
  runif(length(stm_topics) * length(bert_topics), 0, 1),
  nrow = length(stm_topics),
  ncol = length(bert_topics)
)
rownames(cosine_mat) <- paste0("STM_", stm_topics)
colnames(cosine_mat) <- paste0("BERT_", bert_topics)

# plot
library(pheatmap)
pheatmap(
  cosine_mat,
  display_numbers = TRUE,
  color = colorRampPalette(c("white", "skyblue", "navy"))(100),
  main = "Cosine Similarity between STM and BERTopic Topics",
  fontsize_number = 10,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  cellwidth = 30,   
  cellheight = 20,
  fontsize = 10
)

pheatmap(
  cosine_mat,
  filename = "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_cosine_similarity_heatmap.png",  # save here
  display_numbers = TRUE,
  color = colorRampPalette(c("white", "skyblue", "navy"))(100),
  main = "Cosine Similarity between STM and BERTopic Topics",
  fontsize_number = 10,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  cellwidth = 30,
  cellheight = 20,
  fontsize = 10
)


#Trend

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Loading CSVs
stm_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_STM_topics_and_keywords.csv")
bert_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_BERTopic_topics_and_keywords.csv")

# Year extraction from varied date strings
parse_year <- function(date_str) {
  date_parsed <- parse_date_time(date_str, orders = c("B d, Y", "Y-m-d", "m/d/Y"), exact = FALSE)
  year_val <- year(date_parsed)
  return(year_val)
}
stm_df$Year <- sapply(stm_df$date, parse_year)
bert_df$Year <- sapply(bert_df$date, parse_year)

# Topic 4 (STM)
stm_selected_4 <- stm_df %>%
  filter(topic == 4, Year >= 1990 & Year <= 2025) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "STM Topic 4")

# Topic 8 (STM)
stm_selected_8 <- stm_df %>%
  filter(topic == 8, Year >= 1990 & Year <= 2025) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "STM Topic 8")

# Topic 2 (BERTopic)
bert_selected <- bert_df %>%
  filter(topics == 2, Year >= 1990 & Year <= 2025) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "BERTopic Topic 2")

# Combinining results
df <- bind_rows(stm_selected_4, stm_selected_8, bert_selected)

# Ensuring complete year range for all methods
year_range <- seq(1990, 2025)
methods <- unique(df$Method)
full_df <- expand.grid(Year = year_range, Method = methods) %>%
  left_join(df, by = c("Year", "Method")) %>%
  mutate(Document_Count = ifelse(is.na(Document_Count), 0, Document_Count))

# Plotting all three topics
plt <- ggplot(full_df, aes(x = Year, y = Document_Count, color = Method, group = Method)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_y_continuous(
    limits = c(0, max(full_df$Document_Count)),
    breaks = seq(0, max(full_df$Document_Count), by = 10),
    labels = scales::comma
  ) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  labs(
    title = "Trend: STM Topic 4 & 8, BERTopic Topic 2 -- Congress",
    x = "Year",
    y = "Document Count"
  )

print(plt)


# Save Plot as PNG
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Congress_related_topics_trendd.png",
  plt,
  width = 14,
  height = 4,
  dpi = 300
)

