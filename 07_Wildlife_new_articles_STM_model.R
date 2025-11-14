

#install.packages("parallel")

library(stringr)
library(quanteda)
library(stm)
library(tidytext)
library(dplyr)
library(tidyverse)
library(tm)
library(parallel)


dtm <- readRDS("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/clean_dtm.rds")

inspect(dtm)


# Extract info from your DTM
D <- dtm$nrow
vocab <- dtm$dimnames$Terms
doc_names <- dtm$dimnames$Docs
docs_tm <- dtm$i # row indices (doc)
terms_tm <- dtm$j # column indices (term)
counts_tm <- dtm$v # counts

# Build STM’s documents list **and name each element** by your doc ID
documents <- vector("list", D)
for (d in seq_len(D)) {
  idx <- which(docs_tm == d)
  ti <- terms_tm[idx]
  cs <- counts_tm[idx]
  # (optional) aggregate duplicates just in case
  agg <- tapply(cs, ti, sum)
  term_ids <- as.integer(names(agg))
  term_freqs <- as.integer(agg)
  # build a 2 x N matrix: row1 = term IDs, row2 = freqs
  documents[[d]] <- rbind(term_ids, term_freqs)
}



detectCores()

# Preserve your document IDs
names(documents) <- doc_names

# Define candidate Ks
K_vals <- seq(8, 18, by = 2)

# Run searchK to compute held-out likelihood, semantic coherence, and exclusivity
k_search <- searchK(
  documents = documents,
  vocab = vocab,
  K = K_vals,
  heldout.seed = 34,
  seed = 34,
  M = 10,
  init.type = "Spectral",
  cores = 6
)

plot(k_search)

#choosing k=10


# Fiting STM model with K = 10
set.seed(34)
model <- stm(
  documents = documents,
  vocab = vocab,
  K = 10,
  init.type = "Spectral",
  verbose = TRUE,
  #cores = 6 
)

# View/top-terms for each topic
labelTopics(model, n = 10)

# Optional: Save or plot model
plot(model)

#################

gamma <- tidy(model, matrix = "gamma")
gamma <- gamma %>% 
  mutate(GOID = doc_names[document]) %>% 
  select(GOID, topic, gamma)

check <- gamma %>% 
  filter(topic == "6")

beta <- tidy(model, matrix = "beta")
check2 <- beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10)


beta <- exp(model$beta$logbeta[[1]]) # Convert log-probabilities to probabilities

write.csv(beta, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/stm_beta.csv")
write.csv(gamma, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/stm_gamma.csv")

# Get vocab names
keywords <- model$vocab

# Build dataframe: long format with topic, word, and weight (probability)
beta_df <- map_dfr(1:nrow(beta), function(topic_idx) {
  tibble(
    topic   = paste0(topic_idx),
    keyword = keywords,
    weight  = beta[topic_idx, ]
  )
})

# (Optional) Save only the top N words per topic
top_n <- 10
top_keywords_df <- beta_df %>%
  group_by(topic) %>%
  arrange(desc(weight), .by_group = TRUE) %>%
  slice(1:top_n) %>%
  ungroup()

write.csv(top_keywords_df, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/news_stm_topic_keywords_with_weights.csv", row.names = FALSE)

ggplot(check2, aes(label = term, size = beta, color = as.character(topic))) +
  geom_text_wordcloud(rm_outside =  TRUE) +
  scale_size_area(max_size = 20, guide = guide_legend(title = "Beta")) +
  theme_minimal()
