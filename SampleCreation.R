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

# Check if the file exists before sampling
if (!file.exists("SampleData/blogs_sample.txt")) {
  blogs_sample <- sample_lines2("SwiftKeyData/en_US.blogs.txt", sample_size)
  write.table(blogs_sample, file = "SampleData/blogs_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  blogs_sample <- readLines("SampleData/blogs_sample.txt")
}

if (!file.exists("SampleData/news_sample.txt")) {
  news_sample <- sample_lines2("SwiftKeyData/en_US.news.txt", sample_size)
  write.table(news_sample, file = "SampleData/news_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  news_sample <- readLines("SampleData/news_sample.txt")
}

if (!file.exists("SampleData/twitter_sample.txt")) {
  twitter_sample <- sample_lines2("SwiftKeyData/en_US.twitter.txt", sample_size)
  write.table(twitter_sample, file = "SampleData/twitter_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  twitter_sample <- readLines("SampleData/twitter_sample.txt")
}

full_data_sample <- c(blogs_sample, news_sample, twitter_sample)

if (!file.exists("SampleData/full_data_sample.txt")) {
  write.table(full_data_sample, file = "SampleData/full_data_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  full_data_sample <- readLines("SampleData/full_data_sample.txt")
}


# Check if the digest file exists
if (!file.exists("Objects/full_data_sample_digest.txt")) {
  sample_digest <- digest(full_data_sample)
  write.table(sample_digest, file = "Objects/full_data_sample_digest.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  sample_digest <- readLines("Objects/full_data_sample_digest.txt")
}

# Check if the objects exist and the digest matches
if (file.exists("Objects/corp.RDS") & file.exists("Objects/tokens.RDS") & file.exists("Objects/ngram2.RDS") & file.exists("Objects/ngram3.RDS") & file.exists("Objects/ngram4.RDS") & (digest(full_data_sample) == sample_digest)) {
  corp <- readRDS("Objects/corp.RDS")
  tokens <- readRDS("Objects/tokens.RDS")
  ngram2 <- readRDS("Objects/ngram2.RDS")
  ngram3 <- readRDS("Objects/ngram3.RDS")
  ngram4 <- readRDS("Objects/ngram4.RDS")
} else {
  # Convert the text to a corpus
  corp <- corpus(full_data_sample)
  saveRDS(corp, "Objects/corp.RDS")
  
  # Convert the corpus to tokens
  tokens <- tokens(corp)
  saveRDS(tokens, "Objects/tokens.RDS")
  
  # Create n-grams
  ngram2 <- tokens_ngrams(tokens, n = 2)
  saveRDS(ngram2, "Objects/ngram2.RDS")
  
  ngram3 <- tokens_ngrams(tokens, n = 3)
  saveRDS(ngram3, "Objects/ngram3.RDS")
  
  ngram4 <- tokens_ngrams(tokens, n = 4)
  saveRDS(ngram4, "Objects/ngram4.RDS")
  
  # Update the digest
  sample_digest <- digest(full_data_sample)
  write.table(sample_digest, file = "Objects/full_data_sample_digest.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
}

