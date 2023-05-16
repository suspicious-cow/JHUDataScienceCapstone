

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