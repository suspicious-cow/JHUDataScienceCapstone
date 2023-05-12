# set a seed in case we use any random items
set.seed(1337)


# Set the names of the packages and libraries you want to install
# Most notably load up all the quanteda packages we will need
required_libraries <- c("quanteda","quanteda.textmodels","quanteda.textstats",
                        "quanteda.textplots", "readtext")

# Install missing packages and load all required libraries
for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}

# Initial load of the data
# Define the path to our data and the files we want
directory <- "SwiftKeyData/*.txt"  

# Read the text files
text_data <- readtext(directory)

# Create a corpus to hold all our text data
corpus <- corpus(text_data)

# Next we tokenize our data to begin breaking it down
corpus <- tokens(corpus)

# Remove stop words to aren't interesting to our analysis
corpus <- tokens_remove(corpus, stopwords("english"))

# Finally, we create our document feature matrix to get a handle
# on the token frequency
dfm <- dfm(corpus)



