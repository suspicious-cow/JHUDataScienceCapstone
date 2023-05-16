
# check and see if our bootstap object is already saved
if (file.exists("Objects/bootstrap_results_list.rds")) {
  
  # if it is then read our bootstrap object into memory from disk
  bootstrap_results_list <- readRDS("Objects/bootstrap_results_list.rds")
  
} else {
  # Function to process a file and generate bootstrap samples
  bootstrap_unique_words <- function(file_path, n_bootstrap) {
    # Read the file
    text <- readLines(file_path, warn = FALSE)
    
    # Tokenize the text
    tokens <- tokens(text, what = "word", remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english"))
    
    # Generate bootstrap samples and compute the number of unique tokens
    n_unique <- numeric(n_bootstrap)
    for (i in 1:n_bootstrap) {
      bootstrap_sample <- sample(tokens, size = length(tokens), replace = TRUE)
      n_unique[i] <- ntype(bootstrap_sample)
    }
    
    return(n_unique)
  }
  
  
  # Define our file paths
  file_paths <- c("SampleData/blogs_sample.txt", "SampleData/news_sample.txt", "SampleData/twitter_sample.txt")  # replace with your actual file paths
  
  # Number of bootstrap samples
  n_bootstrap <- 5000
  
  # Perform bootstrap sampling for each file
  bootstrap_results_list <- lapply(file_paths, bootstrap_unique_words, n_bootstrap = n_bootstrap)
  
  # save the bootstrap_results_list object to disk
  saveRDS(bootstrap_results_list, "Objects/bootstrap_results_list.rds")
  
}



# Combine the results into a data frame for plotting
bootstrap_results_df <- data.frame(
  Blogs = bootstrap_results_list[[1]],
  News = bootstrap_results_list[[2]],
  Twitter = bootstrap_results_list[[3]]
)

# Melt the data frame to long format
bootstrap_results_long <- reshape2::melt(bootstrap_results_df)


# Compute the mean and standard deviation of the number of unique words for each source file
summary_stats <- sapply(bootstrap_results_list, function(x) c(mean = mean(x), sd = sd(x)))

# Convert to a data frame for easier viewing
summary_stats_df <- data.frame(t(summary_stats))
names(summary_stats_df) <- c("Mean", "Standard Deviation")
rownames(summary_stats_df) <- c("Blogs", "News", "Twitter")

cat("Summary Statistics without Stopwords", "\n")
print(summary_stats_df)



ggplot(bootstrap_results_long, aes(x = value, fill = variable)) + 
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +  
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +  
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5)  
  ) +
  labs(
    title = "Bootstrap Sample Unique Word Counts without Stopwords", 
    y = "Frequency", 
    x = "Number of Unique Words", 
    fill = "Text Source"  
  )