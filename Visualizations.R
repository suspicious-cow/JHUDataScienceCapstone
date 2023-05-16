# ========================= FULL SOURCE =================================
# Print the results
print(initialfilemetrics)


# ========================= BASIC ANALYSIS =================================
# frequency of words with stops
top_tokens <- topfeatures(tokens_dfm, n = 20) # change n to get more or less tokens

print(top_tokens)



# frequency of words wihtout stops
# creating a histogram after we remove the stopwords for comparison
top_tokens_nostop <- topfeatures(tokens_dfm_nostop, n = 20)

print(top_tokens_nostop)





# Generate frequency histogram of data with stopwords

# Extract word frequencies
freq <- textstat_frequency(tokens_dfm)

# Sort in descending order and keep only the top 30 words
top_words <- head(freq, 30)

# Show our histogram
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


# Generate frequency histogram of data without stopwords

# Extract word frequencies
freq_nostop <- textstat_frequency(tokens_dfm_nostop)

# Sort in descending order and keep only the top 30 words
top_words_nostop <- head(freq_nostop, 30)


# Show our histogram
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



# ========================= WORD RELATIONSHIPS ===============================
# Compute the feature co-occurrence matrix
fcm_matrix <- fcm(tokens)

# Print the feature co-occurrence matrix
print(fcm_matrix)



# Print the top 10 most frequent 2-grams, 3-grams, and 4-grams
print(head(ngram2, 10))
print(head(ngram3, 10))
print(head(ngram4, 10))

# Convert tokens to dfm
dfm2 <- dfm(ngram2)
dfm3 <- dfm(ngram3)
dfm4 <- dfm(ngram4)

top_ngrams2 <- topfeatures(dfm2, n = 10)
top_ngrams3 <- topfeatures(dfm3, n = 10)
top_ngrams4 <- topfeatures(dfm4, n = 10)


# Convert to data frames
df2 <- data.frame(ngram = names(top_ngrams2), freq = unname(top_ngrams2))
df3 <- data.frame(ngram = names(top_ngrams3), freq = unname(top_ngrams3))
df4 <- data.frame(ngram = names(top_ngrams4), freq = unname(top_ngrams4))

# Add a column to identify the n-gram order
df2$order <- "2-gram"
df3$order <- "3-gram"
df4$order <- "4-gram"

# Combine the data frames
df_combined <- bind_rows(df2, df3, df4)

# Plot our n-gram data
ggplot(df_combined, aes(x = reorder(ngram, -freq), y = freq, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~order, scales = "free") +
  labs(x = "N-gram", y = "Frequency", fill = "Order",
       title = "Top 10 N-grams in Text Data",
       subtitle = "Comparing 2-grams, 3-grams, and 4-grams")









# print the top 10 collocations bigram and trigram objects for sample with stopwords
head(collocations2, 10)
head(collocations3, 10)
head(collocations4, 10)


# print the top 10 collocations bigram and trigram objects for sample without stopwords
head(collocations2_nostop, 10)
head(collocations3_nostop, 10)
head(collocations4_nostop, 10)












