
# check and see if our bootstap object is already saved
if (file.exists("Objects/bootstrap_results_list_allwords.rds")) {
  
  # if it is then read our bootstrap object into memory from disk
  bootstrap_results_list_allwords <- readRDS("Objects/bootstrap_results_list_allwords.rds")
  
} else {
  # Function to process a file and generate bootstrap samples
  bootstrap_unique_words_allwords <- function(file_path, n_bootstrap) {
    # Read the file
    text <- readLines(file_path, warn = FALSE)
    
    # Tokenize the text
    tokens <- tokens(text, what = "word") %>% tokens_tolower()
    
    # Generate bootstrap samples and compute the number of unique tokens
    n_unique <- numeric(n_bootstrap)
    for (i in 1:n_bootstrap) {
      bootstrap_sample <- sample(tokens, size = length(tokens), replace = TRUE)
      n_unique[i] <- ntype(bootstrap_sample)
    }
    
    return(n_unique)
  }
  
  
  # Define your file paths
  file_paths <- c("SampleData/blogs_sample.txt", "SampleData/news_sample.txt", "SampleData/twitter_sample.txt")  # replace with your actual file paths
  
  # Number of bootstrap samples
  n_bootstrap <- 5000
  
  # Perform bootstrap sampling for each file
  bootstrap_results_list_allwords <- lapply(file_paths, bootstrap_unique_words_allwords, n_bootstrap = n_bootstrap)
  
  # save the bootstrap_results_list object to disk
  saveRDS(bootstrap_results_list_allwords, "Objects/bootstrap_results_list_allwords.rds")
  
}



# Combine the results into a data frame for plotting
bootstrap_results_df_allwords <- data.frame(
  Blogs = bootstrap_results_list_allwords[[1]],
  News = bootstrap_results_list_allwords[[2]],
  Twitter = bootstrap_results_list_allwords[[3]]
)

# Melt the data frame to long format
bootstrap_results_long_allwords <- reshape2::melt(bootstrap_results_df_allwords)

# Plot
library(ggplot2)
ggplot(bootstrap_results_long_allwords, aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Bootstrap Sample Unique Word Counts (Raw)", y = "Frequency", x = "Number of Unique Words", fill = "Sample")

# Compute the mean and standard deviation of the number of unique words for each source file
summary_stats_allwords <- sapply(bootstrap_results_list_allwords, function(x) c(mean = mean(x), sd = sd(x)))

# Convert to a data frame for easier viewing
summary_stats_df_allwords <- data.frame(t(summary_stats_allwords))
names(summary_stats_df_allwords) <- c("Mean", "Standard Deviation")
rownames(summary_stats_df_allwords) <- c("Blogs", "News", "Twitter")

print(summary_stats_df_allwords)