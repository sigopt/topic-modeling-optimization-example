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
  name = "LSI optimization",
  parameters = list(
    list(name = "k", type = "int", bounds = list(min = 20, max = 900))
  ),
  parallel_bandwidth = 4,
  observation_budget = 100,
  metrics = list(list(name = "acc",
                      objective = "maximize",
                      strategy = "optimize"),
                 list(name = "coh",
                      objective = "maximize",
                      strategy = "optimize")),
  project = "topicmodel_compare"
))

### read in data ----
load("data_derived/20_newsgroups_formatted.RData")

# prepare tf-idf
# calculate idf from train1
idf <- log(length(train1) / colSums(dtm[train1,] > 0))

idf_mat <- Matrix(0, nrow = length(idf), ncol = length(idf), sparse = TRUE)

diag(idf_mat) <- idf

rownames(idf_mat) <- names(idf)

colnames(idf_mat) <- names(idf)

# create tfidf for train1 and train2

tfidf1 <- dtm[train1, ] %*% idf_mat

tfidf2 <- dtm[train2, ] %*% idf_mat

### declare model creation and evaluation functions for SigOpt ----

create_model <- function(assignments) {
  
  # create an LSI model 

  lsa <- FitLsaModel(tfidf1, assignments$k)
  
  # apply it to train2
  lsa2 <- predict(lsa, tfidf2)
  
  lsa2[is.na(lsa2) | is.infinite(lsa2) ] <- 0

  # get a train/test split for random forest classification
  tr <- sample(x = seq_along(train2), size = 5332, replace = FALSE)
  te <- seq_along(train2)[-tr]

  # train a classifier using train2
  m_lsa <- train_classifier(y = doc_class[train2][tr],
                            x = lsa2[tr, ])
  
  # predict it on training data for optimization
  p_lsa <- predict_classifier(object = m_lsa,
                              new_data = lsa2[te, ])
  
  # get accuracy for random forest on test
  predicted_class <- apply(p_lsa, 1, function(x) names(x)[which.max(x)])
  
  acc <- sum(predicted_class == as.character(doc_class[train2][te])) / length(te)
  
  # get coherence from train2
  coh <- CalcProbCoherence(phi = lsa$phi, dtm = dtm[train2, ])
  
  coh <- mean(coh)
  
  # return metrics
  metrics <- list(list(name = "acc", value = acc), 
                  list(name = "coh", value = coh))

  metrics
  
}

### run the optimization loop ----

# need to pause the execution so the parallel workers don't get ahead of the 
# API call
Sys.sleep(60) 

output <- parallel::mclapply(seq_len(experiment$observation_budget), function(j){

  suggestion <- create_suggestion(experiment$id)

  value <- create_model(suggestion$assignments)

  create_observation(experiment$id, list(
    suggestion=suggestion$id,
    values=value
  ))
}, mc.cores = 4)


### get the final results ----
lsa_experiment <- fetch_experiment(experiment$id)

lsa_best_assignments <- lsa_experiment$progress$best_observation$assignments

print(lsa_best_assignments)

save(lsa_experiment, lsa_best_assignments, file = "data_derived/lsa_sigopt.RData")


