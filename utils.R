try(library(tidyverse))
try(library(rstan))
try(library(RPostgreSQL))
try(library(dplyr))
try(library(tidyr))
try(library(lubridate))
try(library(pitchRx))
try(library(data.table))
try(library(dplyr))
try(library(ggplot2))

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


## DB connection
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname = 'sammi', port = 5432, host = '192.168.1.71', user="kevin")
