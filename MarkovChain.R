# Convert the tokens into a character vector
tokens_vector <- as.character(tokens_nostop)

# Fit a Markov chain to the tokens
markov_chain <- markovchainFit(data = tokens_vector)
