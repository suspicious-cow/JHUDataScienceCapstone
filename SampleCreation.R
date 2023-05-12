

sample_lines <- function(file, n) {
  total_lines <- sum(readLines(file) != "")
  line_numbers <- data.frame(line_number = 1:total_lines)
  sample_line_numbers <- sqldf(paste0("SELECT * FROM line_numbers 
                                        ORDER BY RANDOM() 
                                        LIMIT ", n))
  read.table(file, skip = setdiff(1:total_lines, sample_line_numbers$line_number),
             header = FALSE, stringsAsFactors = FALSE)
}


sample_size <- 10000
blogs_sample <- sample_lines("SwiftKeyData/en_US.blogs.txt", sample_size)
news_sample <- sample_lines("SwiftKeyData/en_US.news.txt", sample_size)
twitter_sample <- sample_lines("SwiftKeyData/en_US.twitter.txt", sample_size)

write.table(blogs_sample, file = "SampleData/blogs_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(news_sample, file = "SampleData/news_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(twitter_sample, file = "SampleData/twitter_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

data_sample <- rbind(blogs_sample, news_sample, twitter_sample)
write.table(full_data_sample, file = "SampleData/full_data_sample.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)



# Create a TextDocument object
tdm <- TextDocument(data_sample$V1)

# Create n-grams
ngram <- ngram(tdm, n = 3) # Change n as per your requirement
