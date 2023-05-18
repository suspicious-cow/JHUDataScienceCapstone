# Define the fraction of data you want to sample. E.g., 0.1 for 10% of data
fraction_to_sample <- 0.1

# Sample the fraction of your DFM
sampled_dfm <- dfm_sample(tokens_dfm, size = round(ndoc(tokens_dfm)*fraction_to_sample))

# Define the fraction of sampled data you want for training. E.g., 0.8 for 80% of sampled data
train_fraction <- 0.8

# Create an index to split the data
index <- 1:nrow(sampled_dfm) %in% sample(1:nrow(sampled_dfm), round(nrow(sampled_dfm)*train_fraction))

# Split the DFM into training and testing sets
train_dfm <- sampled_dfm[index, ]
test_dfm <- sampled_dfm[!index, ]
