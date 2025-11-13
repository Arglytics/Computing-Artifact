
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


#word_cloud for BERTopic congress

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


# Add this theme to set the background to white
plt1 <- plt1 + theme(plot.background = element_rect(fill = "white", color = NA))

# Save Plot as PNG with white background
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_bertopic_word_cloud.png",
  plt1,
  width = 14,
  height = 4,
  dpi = 300,
  bg = "white"      # This argument sets the PNG background to white
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


# Add this theme to set the background to white
plt2 <- plt2 + theme(plot.background = element_rect(fill = "white", color = NA))

# Save Plot as PNG with white background
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_STM_word_cloud.png",
  plt2,
  width = 14,
  height = 4,
  dpi = 300,
  bg = "white"      # This argument sets the PNG background to white
)


#heatmap for stm and bertopic topics for wildlife news articles

library(pheatmap)
library(dplyr)
library(stringr)
library(tidyr)
library(coop)
library(pheatmap)

stm_df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_stm_topic_keywords_with_weights.csv", stringsAsFactors = FALSE)

bert_df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_bertopic_keywords_with_weights_01.csv", stringsAsFactors = FALSE)


# Just for illustration: create a similarity matrix for the *actual* topic counts (not fixed at 10x10)
set.seed(123)
cosine_mat <- matrix(
  runif(length(stm_topics) * length(bert_topics), 0, 1),
  nrow = length(stm_topics),
  ncol = length(bert_topics)
)
rownames(cosine_mat) <- paste0("STM_", stm_topics)
colnames(cosine_mat) <- paste0("BERT_", bert_topics)

# You may then proceed to plot:
library(pheatmap)
pheatmap(
  cosine_mat,
  display_numbers = TRUE,
  color = colorRampPalette(c("white", "skyblue", "navy"))(100),
  main = "Cosine Similarity between STM and BERTopic Topics",
  fontsize_number = 10,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  cellwidth = 30,    # adjust as you need
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



library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load CSVs
stm_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_STM_topics_and_keywords.csv")
bert_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_BERTopic_topics_and_keywords.csv")

# Flexible year extraction
parse_year <- function(date_str) {
  date_parsed <- parse_date_time(date_str, orders = c("B d, Y", "Y-m-d", "m/d/Y"), exact = FALSE)
  year_val <- year(date_parsed)
  return(year_val)
}

stm_df$Year <- sapply(stm_df$date, parse_year)
bert_df$Year <- sapply(bert_df$date, parse_year)

# Filter by topic and year range
stm_selected <- stm_df %>% filter(topic == 2, Year >= 1990 & Year <= 2025) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "STM Topic 2")

bert_selected <- bert_df %>% filter(topics == 3, Year >= 1990 & Year <= 2025) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "BERTopic Topic 3")

# Merge and complete missing years for each method
year_range <- seq(1990, 2025)
methods <- unique(c("STM Topic 2", "BERTopic Topic 3"))
full_df <- expand.grid(Year = year_range, Method = methods) %>%
  left_join(bind_rows(stm_selected, bert_selected), by = c("Year", "Method")) %>%
  mutate(Document_Count = ifelse(is.na(Document_Count), 0, Document_Count))

# Plot with explicit axis label color for visibility
plt <- ggplot(full_df, aes(x = Year, y = Document_Count, color = Method, group = Method)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_manual(values = c("red", "blue")) +
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
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, margin = margin(t = 5, b = 10), color = "black"),
    axis.text.y = element_text(size = 12, margin = margin(r = 5), color = "black"),  # explicit black font for ticks
    axis.title.x = element_text(size = 14, margin = margin(b = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15, l = 15)),
    plot.title = element_text(hjust = 0.5, size = 18, margin = margin(t = 10, b = 5)),
    plot.margin = margin(2, 2, 2, 2),
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 1.5),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.3)
  ) +
  labs(
    title = "Trend: STM Topic 2 vs BERTopic Topic 3 -- Congress",
    x = "Year",
    y = "Document Count"
  )

print(plt)


# Save Plot as PNG
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Congress_related_topics_trend.png",
  plt,
  width = 14,
  height = 4,
  dpi = 300
)


