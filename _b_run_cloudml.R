################################################################################
# This script stands up a Google Cloud ML instance to run the experiments
# I've got this set up in two scripts: _a_run_all.R and _b_run_cloudml.R (this
# script) so that the whole project is sent as one job. If you run on cloud ML,
# you'll have to pull the results out of the runs/ folder, instead of 
# data_derived/ directly.
################################################################################

library(cloudml)

# note that if this is the first time you run this, you need to run the command
# "gcloud_install()" then "gcloud_init()" to set everything up

cloudml_train("_run_all.R", master_type = "complex_model_s")



