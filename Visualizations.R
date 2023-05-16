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

# Print the total number of 2-grams, 3-grams, and 4-grams in the corpus
print(length(ngram2))
print(length(ngram3))
print(length(ngram4))

# Print the average frequency of 2-grams, 3-grams, and 4-grams in the corpus
print(mean(ngram2))
print(mean(ngram3))
print(mean(ngram4))


# print the top 10 collocations bigram and trigram objects for sample with stopwords
head(collocations2, 10)
head(collocations3, 10)
head(collocations4, 10)


# print the top 10 collocations bigram and trigram objects for sample without stopwords
head(collocations2_nostop, 10)
head(collocations3_nostop, 10)
head(collocations4_nostop, 10)












