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

### declare model creation and evaluation functions for SigOpt ----

# sample a split to train the topic model and train the random forest model
tlda <- sample(train1, size = 1000, replace = FALSE)

trf <- setdiff(train1, tlda)

create_model <- function(assignments) {
  
  # create an LDA model 
  
  lda <- FitLdaModel(dtm[tlda, ], k = assignments$k, 
                     iterations = 300, 
                     burnin = 250,
                     alpha = assignments$alpha, 
                     beta = (assignments$beta_sum) * (colSums(dtm[tlda,]) / sum(dtm[tlda, ])),
                     optimize_alpha = TRUE,
                     calc_likelihood = FALSE,
                     calc_r2 = FALSE,
                     calc_coherence = FALSE)
  
  # apply it to trf
  lda2 <- predict(lda, dtm[trf, ], method = "dot")
  
  lda2[is.na(lda2) | is.infinite(lda2) ] <- 0
  
  # train a classifier using trf
  m_lda <- train_classifier(y = doc_class[trf],
                            x = lda2)
  
  # apply topic model to train2
  lda3 <- predict(lda, dtm[train2, ], method = "dot")
  
  # predict it on train2 data for optimization
  p_lda <- predict_classifier(object = m_lda,
                              new_data = lda3)
  
  # get accuracy for random forest on train2
  predicted_class <- apply(p_lda, 1, function(x) names(x)[which.max(x)])
  
  acc <- sum(predicted_class == as.character(doc_class[train2])) / length(train2)
  
  # get coherence from train2
  coh <- CalcProbCoherence(phi = lda$phi, dtm = dtm[train2, ])
  
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
lda_experiment <- fetch_experiment(experiment$id)

lda_best_assignments <- lda_experiment$progress$best_observation$assignments

print(lda_best_assignments)

save(lda_experiment, lda_best_assignments, file = "data_derived/lda_sigopt.RData")


