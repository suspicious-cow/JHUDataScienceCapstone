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
  blogs_sample <- read.table("SampleData/blogs_sample.txt", header = FALSE, stringsAsFactors = FALSE)
}

if (!file.exists("SampleData/news_sample.txt")) {
  news_sample <- sample_lines2("SwiftKeyData/en_US.news.txt", sample_size)
  write.table(news_sample, file = "SampleData/news_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  news_sample <- read.table("SampleData/news_sample.txt", header = FALSE, stringsAsFactors = FALSE)
}

if (!file.exists("SampleData/twitter_sample.txt")) {
  twitter_sample <- sample_lines2("SwiftKeyData/en_US.twitter.txt", sample_size)
  write.table(twitter_sample, file = "SampleData/twitter_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  twitter_sample <- read.table("SampleData/twitter_sample.txt", header = FALSE, stringsAsFactors = FALSE)
}


if (!file.exists("SampleData/full_data_sample.txt")) {
  full_data_sample <- c(blogs_sample, news_sample, twitter_sample)
  write.table(full_data_sample, file = "SampleData/full_data_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
} else {
  full_data_sample <- read.table("SampleData/full_data_sample.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
}




# Convert the text to a corpus
corp <- corpus(full_data_sample)

# Convert the corpus to tokens
tokens <- tokens(corp)

# Create n-grams
ngram <- tokens_ngrams(tokens, n = 3) # Change n as per your requirement
