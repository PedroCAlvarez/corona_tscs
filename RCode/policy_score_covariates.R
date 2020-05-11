# do some analysis of policy score covariate effects

require(idealstan)
require(ggplot2)
require(tidyr)
require(dplyr)
require(lubridate)
require(readr)
require(stringr)
require(readxl)
require(rstan)

ar_mod <- read_stan_csv("data/cluster/ar/corona_ar2.csv")

# pop it into an idealstan model

id_mod_data <- readRDS("data/cluster/ar/ar1/idealdata_object.rds")
id_mod@stan_samples <- ar_mod

id_plot_legis_dyn(id_mod)
