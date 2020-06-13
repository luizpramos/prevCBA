
#install.packages("devtools")
library(devtools)
devtools::install_github("nome-do-repo/nome-do-pacote")


#install.packages("future")
#install.packages("dplyr")
#install.packages("furrr")
#install.packages("parallel")



library(prevCBA)
library(future)
library(dplyr)
library(furrr)
library(parallel)
library(readr)

data(bd)

bd <- read.csv2("bd.csv")
# se for linux
#plan(multicore)

#se for windows
plan(multisession)

  #rodada
  rodada = 38

  system.time({
    tab <- prevCBA(bd, rodada, meanCBPfm = 1.38, meanCBPsm = 1.04,
                   meanCBPfv = 1.04, meanCBPsv = 1.38, nsim = 1000, CopaBR = "COR", Lib = "CAP")
  })
  tab

