

sample_lines2 <- function(file, n) {
  total_lines <- sum(readLines(file) != "")
  
  # Determine the probability of selection for each line
  p <- n / total_lines
  
  # Initialize an empty vector to store the selected lines
  lines_sample <- c()
  
  # Read the file line by line
  con <- file(file, "r")
  while(TRUE) {
    line <- readLines(con, n = 1)
    if(length(line) == 0) {
      break
    }
    
    # Add the line to the sample with probability p
    if(runif(1) < p) {
      lines_sample <- c(lines_sample, line)
    }
  }
  close(con)
  
  return(lines_sample)
}

sample_size <- 10000
blogs_sample <- sample_lines2("SwiftKeyData/en_US.blogs.txt", sample_size)
news_sample <- sample_lines2("SwiftKeyData/en_US.news.txt", sample_size)
twitter_sample <- sample_lines2("SwiftKeyData/en_US.twitter.txt", sample_size)

write.table(blogs_sample, file = "SampleData/blogs_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(news_sample, file = "SampleData/news_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(twitter_sample, file = "SampleData/twitter_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

data_sample <- rbind(blogs_sample, news_sample, twitter_sample)
write.table(full_data_sample, file = "SampleData/full_data_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)



# Create a TextDocument object
tdm <- TextDocument(data_sample$V1)

# Create n-grams
ngram <- ngram(tdm, n = 3) # Change n as per your requirement
