
# ========================= BASIC ANALYSIS =================================
# frequency of words
top_tokens <- topfeatures(tokens_dfm, n = 20) # change n to get more or less tokens

print(top_tokens)

# Extract word frequencies
freq <- textstat_frequency(tokens_dfm)

# Sort in descending order and keep only the top 30 words
top_words <- head(freq, 30)

# Plot
ggplot(top_words, aes(x = reorder(feature, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 30 Word Frequencies with Stopwords",
       x = "Words",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.y = element_text(size = 9), 
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14))


# creating a histogram after we remove the stopwords for comparison
tokens_nostop <- tokens_remove(tokens, pattern = stopwords("english"))
tokens_dfm_nostop <- dfm(tokens_nostop)

# Extract word frequencies
freq_nostop <- textstat_frequency(tokens_dfm_nostop)

# Sort in descending order and keep only the top 30 words
top_words_nostop <- head(freq_nostop, 30)

# Plot
ggplot(top_words_nostop, aes(x = reorder(feature, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 30 Word Frequencies without Stopwords",
       x = "Words",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.y = element_text(size = 9), 
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14))


# Sort in descending order and keep only the top 30 words
top_words <- head(freq, 30)

# Plot
ggplot(top_words, aes(x = reorder(feature, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 30 Word Frequencies with Stopwords",
       x = "Words",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.y = element_text(size = 9), 
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14))

# Set the base font size
par(cex = 3)  # Increase the size by 50%

# Generate the word cloud
textplot_wordcloud(tokens_dfm)

# Reset the base font size to the default
par(cex = 1)

# ========================= WORD RELATIONSHIPS ===============================
# Compute the feature co-occurrence matrix
fcm_matrix <- fcm(tokens)

# Print the feature co-occurrence matrix
print(fcm_matrix)

# Assuming you already have a tokenized text in 'tokens'
collocations2 <- textstat_collocations(tokens, size = 2)  # for bigrams
collocations3 <- textstat_collocations(tokens, size = 3)  # for trigrams

# print the top 10 collocations
head(collocations2, 10)
head(collocations3, 10)

# Assuming you already have a tokenized text in 'tokens'
collocations2_nostop <- textstat_collocations(tokens_nostop, size = 2)  # for bigrams
collocations3_nostop <- textstat_collocations(tokens_nostop, size = 3)  # for trigrams

# print the top 10 collocations
head(collocations2_nostop, 10)
head(collocations3_nostop, 10)












