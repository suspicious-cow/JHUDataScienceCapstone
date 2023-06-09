##
# Created by Zain Naboulsi for the Johns Hopkins Data Science Specialization Capstone Course 
##

# set a seed in case we use anything that uses random numbers
set.seed(1337)


# Set the names of the packages and libraries you want to install
# Most notably load up all the quanteda packages we will need
required_libraries <- c("quanteda","quanteda.textmodels","quanteda.textstats",
                        "quanteda.textplots", "readtext", "text", "sqldf", 
                        "digest", "ggplot2", "dplyr", "gridExtra", "broom",
                        "tidyverse", "markovchain")

for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}


# Get our initial file metrics on the original source files
# see if we have an RDS file for the object we need
if (file.exists("Objects/initialfilemetrics.rds")) {
  
  # if we do, read our object into memory from disk
  initialfilemetrics <- readRDS("Objects/initialfilemetrics.rds")
  
} else {
  # Define our file paths
  file_paths <- c("SwiftKeyData/en_US.blogs.txt", "SwiftKeyData/en_US.news.txt", "SwiftKeyData/en_US.twitter.txt")
  
  # Initialize a data frame to store the results
  initialfilemetrics <- data.frame(
    File = character(),
    Line_Count = numeric(),
    Word_Count = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each file in our loop
  for (file_path in file_paths) {
    
    # Initialize counters
    line_count <- 0
    word_count <- 0
    
    # Open a connection to the file
    con <- file(file_path, "r")
    
    # Read the file line by line
    while (TRUE) {
      lines <- readLines(con, n = 1)  # read one line at a time
      
      if (length(lines) == 0) {  # end of file
        break
      }
      
      # Update line and word counts
      line_count <- line_count + 1
      word_count <- word_count + length(strsplit(lines, "\\s")[[1]])
    }
    
    # Close the connection
    close(con)
    
    # Add the results to the data frame for each file
    initialfilemetrics <- rbind(initialfilemetrics, data.frame(
      File = file_path,
      Line_Count = line_count,
      Word_Count = word_count,
      stringsAsFactors = FALSE
    ))
  }
  
  # save our object to disk
  saveRDS(initialfilemetrics, "Objects/initialfilemetrics.rds")
}

##
# Created by Zain Naboulsi for the Johns Hopkins Data Science Specialization Capstone Course 
##

# Check if the digest files are missing
if (!file.exists("Digests/blogs_sample_digest.txt") | 
    !file.exists("Digests/news_sample_digest.txt") | 
    !file.exists("Digests/twitter_sample_digest.txt")) {
  
  # if any digest file is missing then create them all to keep track of 
  # when the data changes
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
  
  # if no files are missing read in the digest information for all three files
  blogs_sample_digest <- readLines("Digests/blogs_sample_digest.txt")
  news_sample_digest <- readLines("Digests/news_sample_digest.txt")
  twitter_sample_digest <- readLines("Digests/twitter_sample_digest.txt")
}


# Define the function for sampling lines to use if we have changes to the data
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


# Check if the sample files have changed by looking at the modification times
if (digest(file.info("SampleData/blogs_sample.txt")$mtime) != blogs_sample_digest |
    digest(file.info("SampleData/news_sample.txt")$mtime) != news_sample_digest |
    digest(file.info("SampleData/twitter_sample.txt")$mtime) != twitter_sample_digest) {
  
  # redo the seed to make sure the results are reproducible
  set.seed(1337)
  
  # if there have been changes to the files then we need to sample the 
  # lines from the new files again
  sample_size <- 50000
  blogs_sample <- sample_lines2("SwiftKeyData/en_US.blogs.txt", sample_size)
  news_sample <- sample_lines2("SwiftKeyData/en_US.news.txt", sample_size)
  twitter_sample <- sample_lines2("SwiftKeyData/en_US.twitter.txt", sample_size)
  
  # now let's save our sample files to disk
  write.table(blogs_sample, file = "SampleData/blogs_sample.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  write.table(news_sample, file = "SampleData/news_sample.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  write.table(twitter_sample, file = "SampleData/twitter_sample.txt", 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)
  
} else {
  
  # if none of the sample files have changed just use the already generated
  # sample files
  blogs_sample <- readLines("SampleData/blogs_sample.txt")
  news_sample <- readLines("SampleData/news_sample.txt")
  twitter_sample <- readLines("SampleData/twitter_sample.txt")
}

# after everything is done, combine our three sample files into one
full_data_sample <- c(blogs_sample, news_sample, twitter_sample)

# Check if all of our saved objects exist
if (file.exists("Objects/corp.rds") &
    file.exists("Objects/tokens.rds") &
    file.exists("Objects/tokens_dfm.rds") &
    file.exists("Objects/ngram2.rds") &
    file.exists("Objects/ngram3.rds") &
    file.exists("Objects/ngram4.rds") & 
    file.exists("Objects/collocations2.rds") &
    file.exists("Objects/collocations3.rds") &
    file.exists("Objects/collocations4.rds") &
    file.exists("Objects/tokens_nostop.rds") &
    file.exists("Objects/tokens_dfm_nostop.rds") &
    file.exists("Objects/ngram2_nostop.rds") &
    file.exists("Objects/ngram3_nostop.rds") &
    file.exists("Objects/ngram4_nostop.rds") & 
    file.exists("Objects/collocations2_nostop.rds") &
    file.exists("Objects/collocations3_nostop.rds") &
    file.exists("Objects/collocations4_nostop.rds") 
    
    
    ) {
  
  # if all of our saved objects are there then load them into memory
  corp <- readRDS("Objects/corp.rds")
  tokens <- readRDS("Objects/tokens.rds")
  tokens_dfm <- readRDS("Objects/tokens_dfm.rds")
  ngram2 <- readRDS("Objects/ngram2.rds")
  ngram3 <- readRDS("Objects/ngram3.rds")
  ngram4 <- readRDS("Objects/ngram4.rds")
  collocations2 <- readRDS("Objects/collocations2.rds")
  collocations3 <- readRDS("Objects/collocations3.rds")
  collocations4 <- readRDS("Objects/collocations4.rds")
  tokens_nostop <- readRDS("Objects/tokens_nostop.rds")
  tokens_dfm_nostop <- readRDS("Objects/tokens_dfm_nostop.rds")
  ngram2_nostop <- readRDS("Objects/ngram2_nostop.rds")
  ngram3_nostop <- readRDS("Objects/ngram3_nostop.rds")
  ngram4_nostop <- readRDS("Objects/ngram4_nostop.rds")
  collocations2_nostop <- readRDS("Objects/collocations2_nostop.rds")
  collocations3_nostop <- readRDS("Objects/collocations3_nostop.rds")
  collocations4_nostop <- readRDS("Objects/collocations4_nostop.rds")
  
} else {
  
  # if any saved objects are missing, then redo all the objects just to make
  # sure we don't miss anything
  
  # Create the corpus and tokens objects
  corp <- corpus(full_data_sample)
  tokens <- tokens(corp)
  
  # remove all non-alphanumeric characters since they aren't interesting to us
  tokens <- tokens_remove(tokens, pattern = "[^[:alnum:]]", valuetype = "regex") 
  tokens_dfm <- dfm(tokens)
  
  # generate our frequency distributions for bi,tri, and quad-grams
  # with stopwords
  ngram2 <- tokens_ngrams(tokens, n = 2)
  ngram3 <- tokens_ngrams(tokens, n = 3)
  ngram4 <- tokens_ngrams(tokens, n = 4)
  
  
  # create our bigram and trigram objects for sample with stopwords
  collocations2 <- textstat_collocations(tokens, size = 2)  # for bi-grams
  collocations3 <- textstat_collocations(tokens, size = 3)  # for tri-grams
  collocations4 <- textstat_collocations(tokens, size = 4)  # for quad-grams
  
  # create tokens and dfm with stopwords and specific phrases removed
  tokens_nostop <- tokens_remove(tokens, pattern = c(stopwords("english"), 
                                                     "ã â ã â", 
                                                     "â ã â ã"))
  tokens_dfm_nostop <- dfm(tokens_nostop)
  
  
  # generate our frequency distributions for bi,tri, and quad-grams
  # without stopwords
  ngram2_nostop <- tokens_ngrams(tokens_nostop, n = 2)
  ngram3_nostop <- tokens_ngrams(tokens_nostop, n = 3)
  ngram4_nostop <- tokens_ngrams(tokens_nostop, n = 4)
  
  # create our bigram and trigram objects for sample without stopwords
  collocations2_nostop <- textstat_collocations(tokens_nostop, size = 2)  # for bi-grams
  collocations3_nostop <- textstat_collocations(tokens_nostop, size = 3)  # for tri-grams
  collocations4_nostop <- textstat_collocations(tokens_nostop, size = 4)  # for quad-grams
  
  # Save the corpus, tokens, ngrams, and collocations to RDS files
  saveRDS(corp, "Objects/corp.rds")
  saveRDS(tokens, "Objects/tokens.rds")
  saveRDS(tokens_dfm, "Objects/tokens_dfm.rds")
  saveRDS(ngram2, "Objects/ngram2.rds")
  saveRDS(ngram3, "Objects/ngram3.rds")
  saveRDS(ngram4, "Objects/ngram4.rds")
  saveRDS(collocations2, "Objects/collocations2.rds")
  saveRDS(collocations3, "Objects/collocations3.rds")
  saveRDS(collocations4, "Objects/collocations4.rds")
  saveRDS(tokens_nostop, "Objects/tokens_nostop.rds")
  saveRDS(tokens_dfm_nostop, "Objects/tokens_dfm_nostop.rds")
  saveRDS(ngram2_nostop, "Objects/ngram2_nostop.rds")
  saveRDS(ngram3_nostop, "Objects/ngram3_nostop.rds")
  saveRDS(ngram4_nostop, "Objects/ngram4_nostop.rds")
  saveRDS(collocations2_nostop, "Objects/collocations2_nostop.rds")
  saveRDS(collocations3_nostop, "Objects/collocations3_nostop.rds")
  saveRDS(collocations4_nostop, "Objects/collocations4_nostop.rds")
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


##
# Created by Zain Naboulsi for the Johns Hopkins Data Science Specialization Capstone Course 
##
