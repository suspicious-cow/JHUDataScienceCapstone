# set a seed in case we use any random items
set.seed(1337)


# Set the names of the packages and libraries you want to install
# Most notably load up all the quanteda packages we will need
required_libraries <- c("quanteda","quanteda.textmodels","quanteda.textstats",
                        "quanteda.textplots", "readtext", "text", "sqldf", "digest")

# Install missing packages and load all required libraries
for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}

# Define the function for sampling lines
sample_lines2 <- function(file, n) {
  total_lines <- sum(readLines(file) != "")
  p <- n / total_lines
  lines_sample <- c()
  con <- file(file, "r")
  while(TRUE) {
    line <- readLines(con, n = 1)
    if(length(line) == 0) {
      break
    }
    if(runif(1) < p) {
      lines_sample <- c(lines_sample, line)
    }
  }
  close(con)
  return(lines_sample)
}

# Check if the digest files exist
if (!file.exists("Digests/blogs_sample_digest.txt") | 
    !file.exists("Digests/news_sample_digest.txt") | 
    !file.exists("Digests/twitter_sample_digest.txt")) {
  
  blogs_sample_digest <- digest(file.info("SampleData/blogs_sample.txt")$mtime)
  write.table(blogs_sample_digest, file = "Digests/blogs_sample_digest.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  
  news_sample_digest <- digest(file.info("SampleData/news_sample.txt")$mtime)
  write.table(news_sample_digest, file = "Digests/news_sample_digest.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  
  twitter_sample_digest <- digest(file.info("SampleData/twitter_sample.txt")$mtime)
  write.table(twitter_sample_digest, file = "Digests/twitter_sample_digest.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  
} else {
  blogs_sample_digest <- readLines("Digests/blogs_sample_digest.txt")
  news_sample_digest <- readLines("Digests/news_sample_digest.txt")
  twitter_sample_digest <- readLines("Digests/twitter_sample_digest.txt")
}

# Check if the sample files have changed
if (digest(file.info("SampleData/blogs_sample.txt")$mtime) != blogs_sample_digest |
    digest(file.info("SampleData/news_sample.txt")$mtime) != news_sample_digest |
    digest(file.info("SampleData/twitter_sample.txt")$mtime) != twitter_sample_digest) {
  
  sample_size <- 10000
  blogs_sample <- sample_lines2("SwiftKeyData/en_US.blogs.txt", sample_size)
  news_sample <- sample_lines2("SwiftKeyData/en_US.news.txt", sample_size)
  twitter_sample <- sample_lines2("SwiftKeyData/en_US.twitter.txt", sample_size)
  
  write.table(blogs_sample, file = "SampleData/blogs_sample.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  write.table(news_sample, file = "SampleData/news_sample.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  write.table(twitter_sample, file = "SampleData/twitter_sample.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  
} else {
  blogs_sample <- readLines("SampleData/blogs_sample.txt")
  news_sample <- readLines("SampleData/news_sample.txt")
  twitter_sample <- readLines("SampleData/twitter_sample.txt")
}

full_data_sample <- c(blogs_sample, news_sample, twitter_sample)

# Check if the RDS files exist
if (file.exists("Objects/corp.rds") &
    file.exists("Objects/tokens.rds") &
    file.exists("Objects/ngram2.rds") &
    file.exists("Objects/ngram3.rds") &
    file.exists("Objects/ngram4.rds") & 
    file.exists("Objects/tokens_dfm.rds")) {
  
  # Load the RDS files
  corp <- readRDS("Objects/corp.rds")
  tokens <- readRDS("Objects/tokens.rds")
  ngram2 <- readRDS("Objects/ngram2.rds")
  ngram3 <- readRDS("Objects/ngram3.rds")
  ngram4 <- readRDS("Objects/ngram4.rds")
  tokens_dfm <- readRDS("Objects/tokens_dfm.rds")
  
} else {
  
  # Create the corpus, tokens, and ngrams
  corp <- corpus(full_data_sample)
  tokens <- tokens(corp)
  ngram2 <- tokens_ngrams(tokens, n = 2)
  ngram3 <- tokens_ngrams(tokens, n = 3)
  ngram4 <- tokens_ngrams(tokens, n = 4)
  tokens_dfm <- dfm(tokens)
  
  # Save the corpus, tokens, and ngrams to RDS files
  saveRDS(corp, "Objects/corp.rds")
  saveRDS(tokens, "Objects/tokens.rds")
  saveRDS(ngram2, "Objects/ngram2.rds")
  saveRDS(ngram3, "Objects/ngram3.rds")
  saveRDS(ngram4, "Objects/ngram4.rds")
  saveRDS(tokens_dfm, "Objects/tokens_dfm.rds")
}

# Update the digest files with the new modification times
blogs_sample_digest <- digest(file.info("SampleData/blogs_sample.txt")$mtime)
news_sample_digest <- digest(file.info("SampleData/news_sample.txt")$mtime)
twitter_sample_digest <- digest(file.info("SampleData/twitter_sample.txt")$mtime)
full_data_sample_digest <- digest(file.info("SampleData/full_data_sample.txt")$mtime)

writeLines(blogs_sample_digest, con = "SampleData/blogs_sample_digest.txt")
writeLines(news_sample_digest, con = "SampleData/news_sample_digest.txt")
writeLines(twitter_sample_digest, con = "SampleData/twitter_sample_digest.txt")
writeLines(full_data_sample_digest, con = "SampleData/full_data_sample_digest.txt")
