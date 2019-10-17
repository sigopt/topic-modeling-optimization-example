################################################################################
# This script runs the LSI analysis using SigOpt optimized parameters
################################################################################

source("00_globals.R")

### Set global options etc. to work with SigOpt ----

# set environmental token to use sigopt API
Sys.setenv(SIGOPT_API_TOKEN =
             scan("sigopt_api_key", what = "character", sep = "\n", quiet = TRUE)
)

# create an experiment
experiment <- create_experiment(list(
  name = "LDA optimization",
  parameters = list(
    list(name = "k", type = "int", bounds = list(min = 100, max = 900)),
    list(name = "alpha", type = "double", bounds = list(min = 0.01, max = 1)),
    list(name = "beta_sum", type = "double", bounds = list(min = 50, max = 500))
  ),
  parallel_bandwidth = 4,
  observation_budget = 100,
  metrics = list(list(name = "acc"), list(name = "coh")),
  project = "topicmodel_compare"
))


### read in data ----
load("data_derived/20_newsgroups_formatted.RData")

### declare model creation and evaluation functions for SigOpt ----

create_model <- function(assignments) {
  
  # create an LDA model 
  
  lda <- FitLdaModel(dtm[train1, ], k = assignments$k, 
                     iterations = 300, 
                     burnin = 250,
                     alpha = assignments$alpha, 
                     beta = (assignments$beta_sum) * (colSums(dtm[train1,]) / sum(dtm[train1, ])),
                     optimize_alpha = TRUE,
                     calc_likelihood = FALSE,
                     calc_r2 = FALSE,
                     calc_coherence = FALSE)
  
  # apply it to train2
  lda2 <- predict(lda, dtm[train2, ], method = "dot")
  
  lda2[is.na(lda2) | is.infinite(lda2) ] <- 0
  
  # get a train/test split for random forest classification
  tr <- sample(x = seq_along(train2), size = 5332, replace = FALSE)
  te <- seq_along(train2)[-tr]
  
  # train a classifier using train2
  m_lda <- train_classifier(y = doc_class[train2][tr],
                            x = lda2[tr, ])
  
  # predict it on training data for optimization
  p_lda <- predict_classifier(object = m_lda,
                              new_data = lda2[te, ])
  
  # get accuracy for random forest on test
  predicted_class <- apply(p_lda, 1, function(x) names(x)[which.max(x)])
  
  acc <- sum(predicted_class == as.character(doc_class[train2][te])) / length(te)
  
  # get coherence from train2
  coh <- CalcProbCoherence(phi = lda$phi, dtm = dtm[train2, ])
  
  coh <- mean(coh)
  
  # return metrics
  metrics <- list(acc = acc, coh = coh)
  
  metrics
  
}

### run the optimization loop ----
output <- parallel::mclapply(seq_len(experiment$observation_budget), function(j){
  
  suggestion <- create_suggestion(experiment$id)
  
  value <- create_model(suggestion$assignments)
  
  create_observation(experiment$id, list(
    suggestion=suggestion$id,
    value=value
  ))
  
}, mc.cores = 4)


### get the final results ----
lda_experiment <- fetch_experiment(experiment$id)

lda_best_assignments <- experiment$progress$best_observation$assignments

print(lda_best_assignments)

save(lda_experiment, lda_best_assignments, file = "data_derived/lda_sigopt.RData")


