################################################################################
# This script splits data into two training sets and a test set
################################################################################

### Load necessary libraries ----
library(textmineR)

library(stringr)

### load 20 newsgroups data and build dtm ----
if (! file.exists("news20")) { # if statement so I don't duplicate download etc.
  download.file("http://www.cs.cmu.edu/afs/cs.cmu.edu/project/theo-20/www/data/news20.tar.gz",
                destfile = "news20.tar.gz")
  
  untar(tarfile = "news20.tar.gz",
        exdir = "news20/")
}



docnames <- list.files("news20/20_newsgroup", full.names = TRUE, recursive = TRUE)

docs <- parallel::mclapply(docnames, function(d){
  doc <- scan(d, what = "character", sep = "\n")
  
  doc <- paste(doc, collapse = "\n")
  
  doc
}, mc.cores = 4) %>% 
  unlist() %>%
  stringr::str_conv("UTF-8")

names(docs) <- docnames

doc_class <- stringr::str_split(docnames, pattern = "/") %>%
  sapply(function(x) x[3])

doc_class <- factor(doc_class)

# Using the default stopwords from textmineR. 
# The default is the "english" set and "SMART" set.
# Pretty common choice and it shouldn't make much of a difference on this data set.
dtm <- CreateDtm(doc_vec = docs) 

dim(dtm)

# prune vocab so that only words appearing in greater than 5 docs are included
dtm <- dtm[, colSums(dtm > 0) >= 5]

dim(dtm)

### sample rows into three groups ----
set.seed(90210) # Setting a seed for reproducibility since we call sample below

idx <- seq_len(nrow(dtm))

train1 <- sample(x = idx, size = 6665)

train2 <- sample(x = setdiff(idx, train1), size = 6665)

test <- setdiff(idx, c(train1, train2))

### save the things we need for future use ----
save(docs, dtm, train1, train2, test, doc_class, file = "data_derived/20_newsgroups_formatted.RData")

### remove unzipped newsgroups files ----
# (keeps them from syncing with my cloud backup)

system("rm -r news20")
