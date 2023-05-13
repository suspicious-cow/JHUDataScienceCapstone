# Define your file paths
file_paths <- c("SwiftKeyData/en_US.blogs.txt", "SwiftKeyData/en_US.news.txt", "SwiftKeyData/en_US.twitter.txt")

# Initialize a data frame to store the results
results <- data.frame(
  File = character(),
  Line_Count = numeric(),
  Word_Count = numeric(),
  stringsAsFactors = FALSE
)

# Process each file
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
  
  # Add the results to the data frame
  initialfilemetrics <- rbind(results, data.frame(
    File = file_path,
    Line_Count = line_count,
    Word_Count = word_count,
    stringsAsFactors = FALSE
  ))
}

# save our object
saveRDS(initialfilemetrics, "Objects/initialfilemetrics.rds")

# read our object
initialfilemetrics <- readRDS("Objects/initialfilemetrics.rds")

# Print the results
print(initialfilemetrics)



