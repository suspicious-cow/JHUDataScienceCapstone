if (file.exists("Objects/bootstrap_results_list.rds")) {
  bootstrap_results_list <- readRDS("Objects/bootstrap_results_list.rds")
} else {
  # Function to process a file
  process_file <- function(file_path) {
    # Read the file
    text <- readLines(file_path, warn = FALSE)
    
    # Convert to a corpus
    corpus <- corpus(text)
    
    # Convert to tokens, lower-case, remove punctuation and stop words
    tokens <- tokens(corpus, what = "word", remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english"))
    
    if(length(tokens) == 0) {
      stop(paste("No valid tokens in file:", file_path))
    }
    
    return(tokens)
  }
  
  # Function to compute word frequencies in a bootstrap sample
  compute_word_frequencies <- function(sample_indices, tokens) {
    types <- types(tokens)  # get all types (words)
    sample_types <- types[sample_indices]  # get the types corresponding to the indices
    tokens_sample <- tokens_select(tokens, sample_types)
    freqs <- textstat_frequency(dfm(tokens_sample))
    return(freqs)
  }
  
  # Function to generate bootstrap samples and compute word frequencies
  bootstrap_sampling <- function(file_path, n_bootstrap) {
    # Process the file
    tokens <- process_file(file_path)
    
    # Initialize a list to store the bootstrap results
    bootstrap_results <- list()
    
    # Generate bootstrap samples
    for (i in 1:n_bootstrap) {
      # Generate a bootstrap sample (with replacement)
      sample_indices <- sample(1:length(types(tokens)), replace = TRUE)
      
      # Compute word frequencies in the bootstrap sample
      freqs <- compute_word_frequencies(sample_indices, tokens)
      
      # Store the results
      bootstrap_results[[i]] <- freqs
    }
    
    return(bootstrap_results)
  }
  
  # Define our file paths
  file_paths <- c("SampleData/blogs_sample.txt", "SampleData/news_sample.txt", "SampleData/twitter_sample.txt")  # replace with your actual file paths
  
  # Number of bootstrap samples
  n_bootstrap <- 100
  
  # Perform bootstrap sampling for each file
  results_list <- lapply(file_paths, bootstrap_sampling, n_bootstrap = n_bootstrap)
  
  # save the results_list so we don't have to wait to load it again
  saveRDS(results_list, "Objects/bootstrap_results_list.rds")
}



# Now you have a list of bootstrap results for each file
# Extract the first text file's bootstrap results
bootstrap_results_file1 <- bootstrap_results_list[[1]]
bootstrap_results_file2 <- bootstrap_results_list[[2]]
bootstrap_results_file3 <- bootstrap_results_list[[3]]

# Define function to compute mean and standard deviation of word frequencies
compute_summary_stats <- function(bootstrap_results) {
  # Combine all bootstrap results into one data frame
  all_freqs <- do.call(rbind, bootstrap_results)
  
  # Compute mean and standard deviation of word frequencies
  aggregated_freqs <- all_freqs %>%
    group_by(feature) %>%
    summarise(Mean_Frequency = mean(frequency),
              SD_Frequency = sd(frequency),
              .groups = "drop")
  
  return(aggregated_freqs)
}

# Compute summary statistics for each file's bootstrap results
aggregated_freqs_file1 <- compute_summary_stats(bootstrap_results_file1)
aggregated_freqs_file2 <- compute_summary_stats(bootstrap_results_file2)
aggregated_freqs_file3 <- compute_summary_stats(bootstrap_results_file3)

# Define the number of bootstrap samples
n_bootstrap <- 100

# Calculate the confidence intervals
aggregated_freqs_file1 <- aggregated_freqs_file1 %>%
  mutate(
    CI_low = Mean_Frequency - 1.96*SD_Frequency/sqrt(n_bootstrap),
    CI_high = Mean_Frequency + 1.96*SD_Frequency/sqrt(n_bootstrap)
  )

aggregated_freqs_file2 <- aggregated_freqs_file2 %>%
  mutate(
    CI_low = Mean_Frequency - 1.96*SD_Frequency/sqrt(n_bootstrap),
    CI_high = Mean_Frequency + 1.96*SD_Frequency/sqrt(n_bootstrap)
  )

aggregated_freqs_file3 <- aggregated_freqs_file3 %>%
  mutate(
    CI_low = Mean_Frequency - 1.96*SD_Frequency/sqrt(n_bootstrap),
    CI_high = Mean_Frequency + 1.96*SD_Frequency/sqrt(n_bootstrap)
  )

# Select words of interest
words_of_interest <- c("said", "one", "just", "like", "can")  # replace with your words of interest

# Function to report summary stats for a group of words
report_summary_stats <- function(aggregated_freqs, file_name, words_of_interest) {
  selected_words <- aggregated_freqs[aggregated_freqs$feature %in% words_of_interest, ]
  for (i in 1:nrow(selected_words)) {
    row <- selected_words[i, ]
    print(paste("In", file_name, ", the word '", row$feature, "' has a mean frequency of ", round(row$Mean_Frequency, 2), 
                " with a 95% confidence interval of [", round(row$CI_low, 2), ", ", round(row$CI_high, 2), "]."))
  }
}

# Report summary stats for each file
report_summary_stats(aggregated_freqs_file1, "Blogs", words_of_interest)
report_summary_stats(aggregated_freqs_file2, "News", words_of_interest)
report_summary_stats(aggregated_freqs_file3, "Twitter", words_of_interest)