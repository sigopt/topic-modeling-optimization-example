################################################################################
# Build an optimal LDA model based on SigOpt's results
# 
# Notes: 
#  * I abandoned running an "optimal" LSA model when I saw how much more
#    effective LDA was. 
#  * I hard coded in the optimal model (with rounding) below. This is because
#    I found it easier to do that than automate it the whole way through since
#    this is just a one-off analysis, after all. Please don't judge me :)
################################################################################

source("00_globals.R")

### read in data ----
load("data_derived/20_newsgroups_formatted.RData")

### Get optimal parameters ----

# hard coding this for now
optimal_params <- list(k = 575,
                       alpha = 0.01,
                       beta_sum = 135.16)

### Get the train/test/validation splits ----

# sample a split to train the topic model and train the random forest model
tlda <- sample(train1, size = 1000, replace = FALSE)

trf <- setdiff(train1, tlda)

### train an lda topic model ---

# train model
lda <- FitLdaModel(dtm[tlda, ], k = optimal_params$k, 
                   iterations = 300, 
                   burnin = 250,
                   alpha = optimal_params$alpha, 
                   beta = (optimal_params$beta_sum) * (colSums(dtm[tlda,]) / sum(dtm[tlda, ])),
                   optimize_alpha = TRUE,
                   calc_likelihood = FALSE,
                   calc_r2 = FALSE,
                   calc_coherence = FALSE)


# predict to held out sets
lda2 <- predict(lda, dtm[trf, ], method = "dot")

lda2[is.na(lda2) | is.infinite(lda2) ] <- 0

lda3 <- predict(lda, dtm[c(train2, test), ], method = "dot")

lda3[is.na(lda3) | is.infinite(lda3)] <- 0

### train a random forest model ----

# train model
m_lda <- train_classifier(y = doc_class[trf],
                          x = lda2)

# predict to held out set
p_lda <- predict_classifier(object = m_lda,
                            new_data = lda3)

### calc final evaluation metrics of each model ----

# get accuracy for random forest on train2 and test combined
predicted_class <- apply(p_lda, 1, function(x) names(x)[which.max(x)])

acc <- sum(predicted_class == as.character(doc_class[c(train2, test)])) / 
  length(c(train2, test))

# get coherence from train2 & test
coh <- CalcProbCoherence(phi = lda$phi, dtm = dtm[c(train2, test), ])


# create a metrics object
metrics <- list(acc = acc, coh = coh)

### save results for analysis ----
save(lda, lda2, lda3, m_lda, p_lda, metrics, tlda, trf,
     file = "data_derived/optimal_lda.RData")

