top_tokens <- topfeatures(tokens_dfm, n = 20) # change n to get more or less tokens

print(top_tokens)

# Set the base font size
par(cex = 3)  # Increase the size by 50%

# Generate the word cloud
textplot_wordcloud(tokens_dfm)

# Reset the base font size to the default
par(cex = 1)
