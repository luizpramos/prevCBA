% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/otimizado.R
\name{sim}
\alias{sim}
\title{#####################################Trabalho de Luiz Fernando - PROFMAT

#####################################DESCRICAO:Previsao dos jogos do campeonato brasileiro de futebol- Serie A}
\usage{
sim(data, rodada, n_partidas, n_rodadas, res, meanCBPfm = 1.38,
  meanCBPsm = 1.04, meanCBPfv = 1.04, meanCBPsv = 1.38, nsim)
}
\description{
#' código original
#'
#'data(bd)
}
\details{
library(future)
plan(multicore)
system.time({
tab2 <- prevCBA_original2(data = bd, dados = bd, rodada = 19,
meanCBPfm = 1.38, meanCBPsm = 1.04,
meanCBPfv = 1.04, meanCBPsv = 1.38,
nsim = 1000)
})

system.time({
tab <- prevCBA_original(data = bd, dados = bd, rodada = 19,
meanCBPfm = 1.38, meanCBPsm = 1.04,
meanCBPfv = 1.04, meanCBPsv = 1.38,
nsim = 1000)
})
}
