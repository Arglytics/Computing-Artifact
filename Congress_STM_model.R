

library(stringr)
library(quanteda)
library(stm)
library(tidytext)
library(dplyr)
library(tidyverse)
library(tm)


# Read in the data
congress <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Artifact_congress_for_STMM.csv", stringsAsFactors = FALSE)

head(congress, 5)

congress$cleaned_text <- as.character(congress$cleaned_text)

# Tokenization & cleaning with quanteda
toks <- tokens(
  congress$cleaned_text,
  what = "word",
  remove_punct = TRUE,
  remove_numbers = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove("")    # Remove any empty strings


# Construct Document-Feature Matrix
dfm_obj <- dfm(toks)

# Prune features based on relative document frequency
dfm_obj <- dfm_trim(dfm_obj, min_docfreq = 0.005, max_docfreq = 0.99, docfreq_type = "prop", verbose = TRUE)

# Check top features after preprocessing
topfeatures(dfm_obj, n = 10, scheme = "docfreq")

# Removing specific irrelevant words
dfm_obj <- dfm_remove(dfm_obj, c("s", "pm")) # Add more terms as needed

# Converting DFM to STM input format
dfm_stm <- convert(dfm_obj, to = "stm")

names(dfm_stm)



# Run searchK on your data
K_vals <- seq(2, 10, by = 2)

k_search <- searchK(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = K_vals,
  heldout.seed = 34,
  seed = 34,
  M = 10,
  init.type = "Spectral",
  cores = 6
)

# Plot results to select best K
plot(k_search)

#using K = 8

# Fit STM model
model <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = 8,
  verbose = TRUE
)

# Plotting model topics
plot(model)


#################

library(tidytext)
library(dplyr)

# Tidy the model for the beta (word-topic) matrix
top_terms <- tidy(model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%  # get top 10 terms for each topic
  arrange(topic, -beta)

print(top_terms)

##################
#mapping back to the original dataset

library(tidytext)
library(dplyr)
library(purrr)

# Get the most likely topic for each document
gamma <- tidy(model, matrix = "gamma") %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

# Get keywords for each topic (top 5 shown here)
keywords <- tidy(model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  arrange(topic, -beta) %>%
  summarise(keywords = paste(term, collapse = ", "))

write.csv(beta, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/stm_beta_congress.csv")
write.csv(gamma, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/stm_gamma_congress.csv")


# Prepare gamma_keywords with topic keywords as before
gamma_keywords <- gamma %>%
  left_join(keywords, by = "topic")

# Convert 'document' in gamma_keywords to character to match congress
gamma_keywords$document <- as.character(gamma_keywords$document)

# Make sure congress document column is character
congress$document <- as.character(1:nrow(congress))

# Then perform join on 'document'
congress_topics <- congress %>%
  left_join(gamma_keywords, by = "document")

# Preview final joined data
head(congress_topics[, c("filename", "date", "topic", "keywords")])

# After your join, congress_topics is the final data.frame
write.csv(congress_topics, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_STM_topics_and_keywords.csv", row.names = FALSE)



#getting the keywords and their weights

library(dplyr)
library(tidyr)
library(purrr)

# Get the per-topic, per-word probability matrix (log scale, so exponentiate)
beta <- exp(model$beta$logbeta[[1]])   # topics (rows) × words (columns)
vocab <- model$vocab

# Gather into long format: topic, keyword, weight
keywords_long <- map_dfr(
  1:nrow(beta),
  function(i) {
    tibble(
      topic = paste0(i),
      keyword = vocab,
      weight = beta[i, ]
    )
  }
)

# Optionally, keep only the top N keywords per topic for easier browsing
top_n <- 10
keywords_topn <- keywords_long %>%
  group_by(topic) %>%
  arrange(desc(weight), .by_group = TRUE) %>%
  slice_head(n = top_n) %>%
  ungroup()

# Save as CSV: each row is (topic, keyword, weight)
write.csv(keywords_topn, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_stm_topic_keywords_with_weights.csv", row.names = FALSE)





