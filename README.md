# topic-modeling-optimization-example

To successfully run the code in this repository you will need to have a SigOpt API key. Save it to a text file called "sigopt_api_key" in the root folder of this repository. The file should be a single line, with your API key saved on it.

You will also need to install the dependencies with the following commands:
install.packages("parallel")
install.packages("textmineR"")
install.packages("randomForest")
install.packages("magrittr")
install.packages("stringr")
install.packages("SigOptR")

You will also have to run this code from the root folder by doing one of the follwing:
1. From the command line: enter your R environment from the root directory of the folder
2. From any start point: run setwd("X") where "X" is the root folder of this repo.
3. Use RStudio and open the project "topic-modeing-optimization-example.Rproj" from your RStudio IDE. This will handle all working diretory issues.

Note that if you choose option "2" above [Jenny Bryan will set your computer on fire.](https://www.tidyverse.org/blog/2017/12/workflow-vs-script/)

## To run these files locally
Run the scripts in the following order:

1. 01_download_format_data.R
2. 02_lsa_sigopt.R
3. 03_lda_sigopt.R
4. 04_lda_optimal.R

# To run these files on google cloud compute

1. Make sure you've installed

