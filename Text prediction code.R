rm(list = ls())

# 1. LOAD DATA
blogs <- readLines("en_US.blogs.txt")
news <- readLines("en_US.news.txt")
twitter <- readLines("en_US.twitter.txt")
options(scipen=999)

# 2. SAMPLE
# Sample proportionally, knowing that blogs and news may be overrepresented compared 
# to the population because of long tail distribution
set.seed(123)
bl <- sample(blogs, 10000)
set.seed(345)
ne <- sample(news, 10000)
set.seed(567)
tw <- sample(twitter, 20000)
data <- c(bl, ne, tw)

# 4. PRE-PROCESS

# 4a. CLEAN UP TEXT
# Make all lower case
data <- tolower(data)

# Remove all non-letter characters, non-whitespace characters, except for apostrophes
library(stringr)
data <- str_replace_all(data, "[^[:alpha:][:space:]’']", " ")

# Replace one or more spaces with one space
data <- str_replace_all(data, "\\s+", " ")

# Trim leading and trailing whitespace
data <- str_trim(data)

# Remove profanity from data set
# Load profanity data set
# The dataset was found at http://fffff.at/googles-official-list-of-bad-words/
profanity <- readLines("profanity_list.txt")
profanity <- str_replace_all(profanity, "[^[:alpha:][:space:]’']", "")
profanity <- profanity[c(5:6, 8:449)]
# Remove them
library(tm)
clean_data <- removeWords(data, profanity)

# Split into test and train sets
## 75% of the sample size
str(clean_data)
smp_size <- floor(0.8 * length(clean_data))
set.seed(665)
train_ind <- sample(seq_len(length(clean_data)), size = smp_size)
train <- clean_data[train_ind ]
test <- clean_data[-train_ind ]

# Rename clean data, x
x <- train

# 4b. CREATE N-GRAM TABLES
# Unigrams
library(tokenizers)
unique_words <- unlist (tokenize_ngrams(x, n = 1, n_min = 1))
table1 <- as.data.frame(table(unique_words))
words <- table1[order(-table1$Freq),]
words$unique_words <- as.character(words$unique_words)

# Bigrams
unique_bigrams <- unlist (tokenize_ngrams(x, n = 2, n_min = 2))
table2 <- as.data.frame(table(unique_bigrams))
bigrams <- table2[order(-table2$Freq),]

# Trigrams
unique_trigrams <- unlist (tokenize_ngrams(x, n = 3, n_min = 3))
table3 <- as.data.frame(table(unique_trigrams))
trigrams <- table3[order(-table3$Freq),]

# 4c. CLEAN UP BIGRAM TABLES
# Make the bigrams into a df with the bigram and then each 1-2 terms in separate columns
# Split terms
terms2 <- strsplit(as.character(bigrams$unique_bigrams), " ")
# Create terms2 matrix
terms2 <- matrix(unlist(terms2), ncol=2, byrow=TRUE)
# Paste the full trigram and frequency and remane cols
bi <- cbind(bigrams, terms2)
names(bi) <- c("bigrams", "freq", "term1", "term2")
# Make factors into characters
bi$bigrams <- as.character(bi$bigrams)
bi$term1 <- as.character(bi$term1)
bi$term2 <- as.character(bi$term2)

# Remove all bigrams that have repetition in w1 and w2 
bi <- bi[!bi$term1==bi$term2,]
# Remove all bigrams that are "a" for term1 and start with "a" for term2
bi <- bi[!bi$term1=="a" & grepl("^a", bi$term2)==F,]
# Remove all bigrams that contain "RT" (Retweet)
bi <- bi[!bi$term1=="rt" & !bi$term2=="rt",]
# Remove all bigrams that contain "u" as a word
bi <- bi[!bi$term1=="u" & !bi$term2=="u",]


# 4d. CLEAN UP TRIGRAM TABLES
# Make the trigrams into a df with the trigram and then each 1-3 terms in separate columns
# Split terms
terms3 <- strsplit(as.character(trigrams$unique_trigrams), " ")
# Create terms3 matrix
terms3 <- matrix(unlist(terms3), ncol=3, byrow=TRUE)
# Paste the full trigram and frequency
tri <- cbind(trigrams, terms3)
names(tri) <- c("trigrams", "freq", "term1", "term2", "term3")
# Make factors into characters
tri$trigrams <- as.character(tri$trigrams)
tri$term1 <- as.character(tri$term1)
tri$term2 <- as.character(tri$term2)
tri$term3 <- as.character(tri$term3)

# Remove all tris that have repetition in w1w2 or w2w3
tri <- tri[!tri$term1==tri$term2 & !tri$term2==tri$term3,]
# Remove all trigrams that are "a" for term1 and start with "a" for term2
tri <- tri[!tri$term1=="a" & grepl("^a", tri$term2)==F,]
# Remove all trigrams that are "a" for term2 and start with "a" for term3
tri <- tri[!tri$term2=="a" & grepl("^a", tri$term3)==F,]
# Remove all trigrams that contain "rt" (retweet)
tri <- tri[!tri$term1=="rt" & !tri$term2=="rt" & !tri$term3=="rt",]
# Remove all trigrams that contain "u" as a word
tri <- tri[!tri$term1=="u" & !tri$term2=="u" & !tri$term3=="u",]

# 5. CALCULATIONS

# 5a. BIGRAM AND TRIGRAM DISCOUNT FACTORS
# Create bigram discount factor
bi_count1 <- length(bi[bi$freq==1,1])
bi_count2 <- length(bi[bi$freq==2,1])
D2 <- bi_count1 / (bi_count1 + 2 * bi_count2)

# Create trigram discount factor
tri_count1 <- length(tri[tri$freq==1,1])
tri_count2 <- length(tri[tri$freq==2,1])
D3 <- tri_count1 / (tri_count1 + 2 * tri_count2)

# 5b. ADD ALL FIELDS NECESSARY FOR KNESER-NEY PROBABILITY CALCULATION FOR TRIGRAMS
# Create w1w2 bigram w freq count for the trigram table
# This means merging tri (x) and bi (y), with a full join on the x values, by term1 and term2
library(data.table)
tri <- data.table(tri)
bi <- data.table(bi)
new_tri <- merge(x=tri, y=bi, by=c("term1", "term2"))
new_tri <- new_tri[,c(3,4,6,7,1,2,5)]
new_tri <- setnames(new_tri,c("freq.x", "freq.y", "bigrams"),c("tri_w1w2w3_count", "bi_w1w2_count", "bigrams_w1w2"))
head(new_tri)

# Create new col w unique counts of trigram w1w2*
new_tri <- new_tri[ , uniques := length(unique(term3)), by="bigrams_w1w2"]
new_tri <- setnames(new_tri, "uniques", "tri_w1w2*_unique")

# Create new col w unique counts of trigram *w2w3
# This means first merging new_tri (x) with bi (y) with a full join on the x values, by term2 and term3
# for new_tri (x) and by term1 and term 2 for bi (y)
# First, create a new bi table with renamed cols
bi_2 <- bi
names(bi_2) <- c("bigrams", "freq", "term2", "term3")
new_tri <- merge(x=new_tri, y=bi_2, by=c("term2", "term3"))
new_tri <- setnames(new_tri, c("bigrams", "freq"), c("bigrams_w2w3","bi_w2w3_count"))
new_tri <- new_tri[,c(3,4,5,6,8,9,10,7,1,2)]
# Create col for unique counts of trigram *w2w3
new_tri <- new_tri[ , uniques := length(unique(term1)), by="bigrams_w2w3"]
new_tri <- setnames(new_tri, "uniques", "tri_*w2w3_unique")
# Create new col w unique counts of bigram *w2
new_tri <- new_tri[ , uniques := length(unique(term1)), by="term2"]
new_tri <- setnames(new_tri, "uniques","bi_*w2_unique")

# Create new col w unique counts of bigram w2*
new_tri <- new_tri[ , uniques := length(unique(term3)), by="term2"]
new_tri <- setnames(new_tri, "uniques", "bi_w2*_unique")

# Create new col w unique counts of bigram *w3
new_tri <- new_tri[ , uniques := length(unique(term2)), by="term3"]
new_tri <- setnames(new_tri, "uniques","bi_*w3_unique")

# Convert back to data.frame
new_tri <- data.frame(new_tri)

# Create new col w unique counts of bigrams (all)
new_tri$bi_total_unique <- length(bi$bigrams)

# Clean up names
names(new_tri) <- gsub("\\.", "\\*", names(new_tri))

# Create discounted value cols
new_tri$tri_w1w2w3_count_discounted <- new_tri$tri_w1w2w3_count - D3
new_tri$`tri_*w2w3_unique_discounted`  <- new_tri$`tri_*w2w3_unique` - D2

# Create col of zeros to calculate the max in the Kneser-Ney formula
new_tri$zeros <- rep(0, length(new_tri$trigrams))

# Max calculations
new_tri$tri_max <- apply(new_tri[,c(16,18)], 1, max)
new_tri$bi_max <- apply(new_tri[,c(17,18)], 1, max) 

# 5c. CALCULATE KNESER-NEY PROBABILITY FOR TRIGRAMS
# Calculate the trigram probability using the formula
new_tri$tri_PKN <- 
    # Discounted trigram probability 
    new_tri$tri_max / new_tri$bi_w1w2_count +
    # Plus bigram backoff weight 
    D3 * new_tri$`tri_w1w2*_unique` / new_tri$bi_w1w2_count *
    # Times discounted bigram probability 
    new_tri$bi_max / new_tri$`bi_*w2_unique` +
    # Plus unigram backoff weight
    D2 * new_tri$`bi_w2*_unique` / new_tri$`bi_*w2_unique` *
    # Times unigram probability 
    new_tri$`bi_*w3_unique` / new_tri$bi_total_unique


# 5d. ADD ALL FIELDS NECESSARY FOR KNESER-NEY PROBABILITY CALCULATION FOR BIGRAMS
# Bigram probability

# bi_w1w2_count
bi <- data.table(bi)
new_bi <- setnames(bi, "freq", "bi_w1w2_count")

# uni_w1_count
# Merge new_bi (x) and words (y) by term1 for x and Freq for y
new_bi <- merge(new_bi, words, by.x="term1", by.y="unique_words")
new_bi <- setnames(new_bi, "Freq", "uni_w1_count")

# bi_w1*_unique
new_bi <- new_bi[ , uniques := length(unique(term2)), by="term1"]
new_bi <- setnames(new_bi, "uniques", "bi_w1*_unique")

# bi_*w2_unique
new_bi <- new_bi[ , uniques := length(unique(term1)), by="term2"]
new_bi <- setnames(new_bi, "uniques", "bi_*w2_unique")

# Convert back to a data frame
new_bi <- data.frame(new_bi)

# Discounted bi_w1w2_count
new_bi$bi_w1w2_count_discounted <- new_bi$bi_w1w2_count - D2

# Add zeros
new_bi$zeros <- rep(0, length(new_bi$bigrams))

# Max calculations
new_bi$bi_max <- apply(new_bi[,c(8,9)], 1, max)

# Clean up names
names(new_bi) <- gsub("\\.", "\\*", names(new_bi))

# 5c. CALCULATE KNESER-NEY PROBABILITY FOR BIGRAMS
# Calculate the bigram probability using the formula
new_bi$bi_PKN <- 
    # Discounted bigram probability 
    new_bi$bi_max / new_bi$uni_w1_count +
    # Plus unigram backoff weight 
    D2 * new_bi$`bi_w1*_unique` / new_bi$uni_w1_count *
    # Times unigram probability
    new_bi$`bi_*w2_unique` / (new_bi$`bi_*w2_unique` + new_bi$`bi_w1*_unique`)

# 6. EXPORT KNESER-NEY PROBABILITY TABLES

# 6a. EXPORT KNESER-NEY PROBABILIY TABLE FOR TRIGRAMS
# Keep only the top five matches for each trigram following term1term2
new_tri <- data.table(new_tri)
new_tri <- setDT(new_tri)[order(bigrams_w1w2,-tri_PKN), .SD[1:5], by=bigrams_w1w2]
new_tri <- data.frame(new_tri)
# Add a column that labels the probability rank
new_tri$Rank <- rep(c("Top Prediction", "Option 2", "Option 3", "Option 4", "Option 5"), nrow(new_tri)/5)
# Get rid of rows that have NA 
new_tri <- new_tri[complete.cases(new_tri),]
new_tri <- new_tri[, c(1,10,21,22)]
# Export table
write.csv(new_tri, "Modified Kneser-Ney Trigram Probabilities.csv")

# 6b. EXPORT KNESER-NEY PROBABILIY TABLE FOR BIGRAMS
# Keep only the top five matches for each bigram following term1
new_bi <- data.table(new_bi)
new_bi <- setDT(new_bi)[order(term1,-bi_PKN), .SD[1:5], by=term1]
new_bi <- data.frame(new_bi)
# Add a column that labels the probability rank
new_bi$Rank <- rep(c("Top Prediction", "Option 2", "Option 3", "Option 4", "Option 5"), nrow(new_bi)/5)
# Get rid of rows that have NA 
new_bi <- new_bi[complete.cases(new_bi),]
new_bi <- new_bi[, c(1,4,11, 12)]

# Export table
write.csv(new_bi, "Modified Kneser-Ney Bigram Probabilities.csv")

# 7. CREATE FUNCTIONS TO PREDICT NEXT WORD

# 7a. FUNCTION TO PREDICT NEXT WORD IF INPUT IS >= TWO WORDS
# Now use these prob tables to predict the next word following a two-word phrase
next_word_tri <- function(x){
    if (x %in% new_tri$bigrams_w1w2==F) {
        Rank <- "Please try again" 
        Prediction <- "No match"
        no_match <- data.frame(Rank, Prediction)
        no_match
    }
    else {
        # Subset for all the trigrams that start with the two-word phrase
        subset <- new_tri[new_tri$bigrams_w1w2==x,]
        # Fetch the top 5 most likely words to complete the two-word phrase
        subset <- subset[,c(4,2)] 
        subset <- subset[complete.cases(subset),]
        names(subset) <- c("Rank", "Prediction")
        subset 
    }
}
next_word_tri("pppffff iin")

# 7b. FUNCTION TO PREDICT NEXT WORD IF INPUT IS = ONE WORD
# Now use these prob tables to predict the next word following a one-word phrase
next_word_bi <- function(x){
    if (x %in% new_bi$term1==F) {
        Rank <- "Please try again" 
        Prediction <- "No match"
        no_match <- data.frame(Rank, Prediction)
        no_match 
    }
    else {
        # Subset for top 5 bigrams that start with the unigram
        subset <- new_bi[new_bi$term1==x,]
        # Fetch the max prob value
        subset <- subset[,c(4,2)]
        subset <- subset[complete.cases(subset),]
        names(subset) <- c("Rank", "Prediction")
        subset
    }
}

# 7c. FUNCTION TO PREDICT NEXT WORD (ALL INPUTS)
next_word <- function (x) {
    # Make the string lower case
    x <- tolower(x)
    # If STRING IS TWO OR MORE WORDS
    if(sapply(gregexpr("\\S+", x), length) >= 2) {
        # Make x the last two words in the string
        y <- word(x,-2, -1)
        # And use the trigram predictor to predict the next word
        trigram_pred <- next_word_tri(y)
        # If trigram_pred is a "no match" value, then go to the bigram table for the last word in the string
        if (trigram_pred[1,2] == "No match") {
            # Make x the last word in the string
            z <- word(x,-1)
            # Use bigram predictor to predict next word
            next_word_bi(z)
            # If trigram_pred is an actual value, print that value
        } else { 
            trigram_pred
        }
        # Else IF STRING IS LESS THAN TWO WORDS
    } else {
        # Use bigram predictor
        next_word_bi(x)
    }
}
next_word("isn't it")

# 8. TEST
x <- test

# Create trigrams table
unique_trigrams <- unlist (tokenize_ngrams(x, n = 3, n_min = 3))
terms3 <- strsplit(as.character(trigrams$unique_trigrams), " ")
terms3 <- data.frame (matrix(unlist(terms3), ncol=3, byrow=TRUE) )
terms3$term1term2 <- paste(terms3$X1, terms3$X2)
terms3 <- terms3[,c(4,3)]
names(terms3) <- c("term1term2", "term3")
terms3$term3 <- as.character(terms3$term3)
tri <- terms3

# Run next_word function on the df [warning: this takes a really long time to run]. 
predicted_term3 <- sapply(tri[,1], next_word) 
tri <- cbind(tri, predicted_term3)

# Test if the predicted term3 is the same as the actual term3
tri$prediction_correct <- ifelse(tri$term3==tri$predicted_term3, "YES", "NO")

# Compute percentage of correctly predicted term3
library(plyr)
counts <- count(tri$prediction_correct)
percent_correct <- paste0(round(counts[2,2]/sum(counts$freq), 4) * 100, "%")
percent_correct
percent_no_match <- paste0(round(nrow(tri[tri$predicted_term3=="no match",])/nrow(tri),4) * 100, "%")
percent_no_match

# Test how long the computation takes 
system.time(next_word("great time"))
system.time(next_word("oh please"))
system.time(next_word("i can't"))



