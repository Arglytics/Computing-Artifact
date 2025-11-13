
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


#word_cloud for BERTopic news articles

df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Bertopic_keyword_counts_for_news.csv", stringsAsFactors = FALSE)

head(df, 5)

set.seed(9)  # for reproducible layout

# Create word cloud plot with heading
plt1 <- ggplot(df, aes(label = keyword, size = count, color = count)) +
  geom_text_wordcloud(shape = "circle", area_corr_power = 1, rm_outside = TRUE) +
  scale_size_area(max_size = 15, guide = guide_legend(title = "Count")) +
  theme_minimal() +
  ggtitle("Keyword Frequency Word Cloud for Bertopic")   # <-- Add your desired heading here

plt1

# Save Plot as PNG
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_news_articles_bertopic_word_cloud.png",
  plt1,
  width = 14,
  height = 4,
  dpi = 300
)


# Add this theme to set the background to white
plt1 <- plt1 + theme(plot.background = element_rect(fill = "white", color = NA))

# Save Plot as PNG with white background
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_news_articles_bertopic_word_cloud.png",
  plt1,
  width = 14,
  height = 4,
  dpi = 300,
  bg = "white"      # This argument sets the PNG background to white
)


#word_cloud for STM news articles


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

df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/STM_keyword_counts_for_news.csv", stringsAsFactors = FALSE)

head(df, 5)

set.seed(9)  # for reproducible layout

# Create word cloud plot with heading
plt2 <- ggplot(df, aes(label = keyword, size = count, color = count)) +
  geom_text_wordcloud(shape = "circle", area_corr_power = 1, rm_outside = TRUE) +
  scale_size_area(max_size = 15, guide = guide_legend(title = "Count")) +
  theme_minimal() +
  ggtitle("Keyword Frequency Word Cloud for STM")   # <-- Add your desired heading here

plt2


# Save Plot as PNG
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_news_articles_STM_word_cloud.png",
  plt2,
  width = 14,
  height = 4,
  dpi = 300
)

# Add this theme to set the background to white
plt2 <- plt2 + theme(plot.background = element_rect(fill = "white", color = NA))

# Save Plot as PNG with white background
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_news_articles_STM_word_cloud.png",
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

stm_words <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/news_stm_topic_keywords_with_weights.csv", stringsAsFactors = FALSE)

bert_words <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_bertopic_keywords_with_weights_01.csv", stringsAsFactors = FALSE)



# Suppose you have 10 STM topics and 10 BERTopic topics
stm_topics <- paste0("STM_", 1:10)
bert_topics <- paste0("BERT_", 0:9)

# Create a random 10x10 cosine similarity matrix for illustration (replace with your real values)
set.seed(123)
cosine_mat <- matrix(runif(100, 0, 1), nrow = 10)
rownames(cosine_mat) <- stm_topics
colnames(cosine_mat) <- bert_topics

# Order the matrix numerically: 
stm_ord <- order(as.numeric(gsub("STM_", "", rownames(cosine_mat))))
bert_ord <- order(as.numeric(gsub("BERT_", "", colnames(cosine_mat))))
cosine_mat_ordered <- cosine_mat[stm_ord, bert_ord]
rownames(cosine_mat_ordered) <- paste0("STM_", sort(as.numeric(gsub("STM_", "", rownames(cosine_mat)))))
colnames(cosine_mat_ordered) <- paste0("BERT_", sort(as.numeric(gsub("BERT_", "", colnames(cosine_mat)))))

# Plot the heatmap
#pheatmap(
  #cosine_mat_ordered,
  #display_numbers = TRUE,
  #color = colorRampPalette(c("white", "skyblue", "navy"))(100),
  #main = "Cosine Similarity between STM and BERTopic Topics",
  #fontsize_number = 10,
  #cluster_rows = FALSE,
  #cluster_cols = FALSE
#)


pheatmap(
  cosine_mat_ordered,
  display_numbers = TRUE,
  color = colorRampPalette(c("white", "skyblue", "navy"))(100),
  main = "Cosine Similarity between STM and BERTopic Topics",
  fontsize_number = 10,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  cellwidth = 30,    # decrease for smaller plot (try 20–40)
  cellheight = 20,   # decrease for smaller plot (try 10–30)
  fontsize = 10      # reduce text size if needed
)


#Saving the heatmap

pheatmap(
  cosine_mat_ordered,
  display_numbers = TRUE,
  color = colorRampPalette(c("white", "skyblue", "navy"))(100),
  main = "Cosine Similarity between STM and BERTopic Topics",
  fontsize_number = 10,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  filename = "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_news_articles_stm_bertopic_heatmap.png"  # set your desired path here
)


#Trend: STM Topic 1 vs BERTopic Topic 3 -- Wildlife news articles

library(readr)
library(dplyr)
library(ggplot2)

# Load and combine your CSVs into one dataframe called df
stm_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/best_topic_per_goid_merged.csv")
bert_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/merged_df_bertopic.csv")


# Extract year
stm_df$Year <- as.numeric(substr(stm_df$Date, 1, 4))
bert_df$Year <- as.numeric(substr(bert_df$Date, 1, 4))

# Filter by topic for comparison (for example STM topic 1 and BERTopic topic 3)
stm_selected <- stm_df %>% filter(topic == 1) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "STM Topic 1")

bert_selected <- bert_df %>% filter(Topic == 3) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "BERTopic Topic 3")

# Combine for plotting
df <- bind_rows(stm_selected, bert_selected)

# Make sure to include all years up to 2025 (even if missing)
year_range <- data.frame(Year = seq(min(df$Year), 2025))
df <- df %>% right_join(year_range, by = "Year") %>%
  mutate(
    Document_Count = ifelse(is.na(Document_Count), 0, Document_Count),
    Method = ifelse(is.na(Method), "STM Topic 1", Method)
  )

# Plot
plt <- ggplot(df, aes(x = Year, y = Document_Count, color = Method, group = Method)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_continuous(limits = c(1980, 2025), breaks = seq(1980, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, max(df$Document_Count, na.rm = TRUE)), breaks = seq(0, max(df$Document_Count, na.rm = TRUE), by = 10)) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, margin = margin(t = 5, b = 10)),
    axis.text.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 14, margin = margin(b = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15, l = 15)),
    plot.title = element_text(hjust = 0.5, size = 18, margin = margin(t = 10, b = 5)),
    plot.margin = margin(2, 2, 2, 2),
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 1.5),
    panel.grid.major = element_line(color = "white", linewidth = 0.6),
    panel.grid.minor = element_line(color = "white", linewidth = 0.3)
  ) +
  labs(
    title = "Trend: STM Topic 1 vs BERTopic Topic 3 -- Wildlife news articles",
    x = "Year",
    y = "Document Count"
  )

print(plt)

# Save Plot as PNG
ggsave(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_news_articles_related_topics_trend.png",
  plt,
  width = 14,
  height = 4,
  dpi = 300
)


