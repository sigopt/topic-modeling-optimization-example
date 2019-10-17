################################################################################
# This script stands up a Google Cloud ML instance to run the experiments
################################################################################

library(cloudml)

# note that if this is the first time you run this, you need to run the command
# "gcloud_install()" then "gcloud_init()" to set everything up

cloudml_train("04_run_all.R", master_type = "complex_model_s")



