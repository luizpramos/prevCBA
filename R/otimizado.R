#' #####################################
#' # Trabalho de Luiz Fernando - PROFMAT
#' #####################################
#' # DESCRICAO:
#' # Previsao dos jogos do campeonato brasileiro de futebol- Serie A
#'

#roda uma simula??o
sim <- function(data, rodada, n_partidas, n_rodadas, res,
                meanCBPfm = 1.38, meanCBPsm = 1.04,
                meanCBPfv = 1.04, meanCBPsv = 1.38, nsim, CopaBR, Lib) {

  # Objeto que armazenarar a previsao dos jogos
  prevdados <- criar_prevdados(data, rodada, n_partidas)

  # Atualizando o class com o banco de dados informado
  prevclass <- class

  #for que varre as rodada
  for (i in rodada:n_rodadas) {

    # Atualizacao dos lambdas da media de gols
    lauxfm <- meanCBPfm_atual(prevdados, i, meanCBPfm)
    lauxsv <- meanCBPsv_atual(prevdados, i, meanCBPsv)
    lauxfv <- meanCBPfv_atual(prevdados, i, meanCBPfv)
    lauxsm <- meanCBPsm_atual(prevdados, i, meanCBPsm)

    # Contador de partidas por rodada
    j <- n_partidas*(i - 1) + 1

    for(r in j:(j + n_partidas - 1)) {

      # Buscando time1 e time2 de acordo com coluna da rodada
      time1 <- as.character(prevdados$Time1[r])
      time2 <- as.character(prevdados$Time2[r])

      if (i == 1) {
        lambdam <- (meanCBPfm + meanCBPsv)/2 # parametro da poisson do time mandante
        lambdav <- (meanCBPsm + meanCBPfv)/2 # parametro da poisson do time visitante
      } else {
        lfm <- lauxfm[rownames(lauxfm) == time1]
        lsv <- lauxsv[rownames(lauxsv) == time2]
        lfv <- lauxfv[rownames(lauxfv) == time2]
        lsm <- lauxsm[rownames(lauxsm) == time1]

        lambdam <- (lfm + lsv)/2 # parametro da poisson do time mandante
        lambdav <- (lsm + lfv)/2 # parametro da poisson do time visitante
      }

      # Escala para mandante
      escm <- ppois(c(0, 1, 2, 3, 4, 5), lambda = lambdam)

      # Escala para visitante
      escv <- ppois(c(0, 1, 2, 3, 4, 5), lambda = lambdav)

      # Uma simulacao para o mandante
      simm <- cbind(runif(1, 0, 1))

      # Armazenando os resultados dos gols como mandante
      resm <- apply(simm, 1, simaux, escala = escm)

      # Simulacao para o visitante
      simv <- cbind(runif(1, 0, 1))

      # Armazenando os resultados dos gols como visitante
      resv <- apply(simv, 1, simaux, escala = escv)

      prevdados$GFM[r] <- round(mean(resm), 0)
      prevdados$GSM[r] <- round(mean(resv), 0)
      prevdados$GFV[r] <- round(mean(resv), 0)
      prevdados$GSV[r] <- round(mean(resm), 0)

    }

  }

  list(
    prevprob = atualizar_prevprob(prevdados, rodada, nsim),
    res = atualizar_res(prevdados, res, nsim, CopaBR, Lib)
  )
}

#' prevCBA
#'
#' Faz toda a previsão
#'
#' @param data banco de dados
#' @param rodada rodada
#' @param meanCBPfm meanCBPfm
#' @param meanCBPsm meanCBPsm
#' @param meanCBPfv meanCBPfv
#' @param meanCBPsv meanCBPsv
#' @param nsim numero de simulacoes
#'
#' @export
prevCBA <- function(data, rodada, meanCBPfm, meanCBPsm,
                    meanCBPfv, meanCBPsv, nsim, CopaBR, Lib) {

  # mandante do jogo - time1
  # visitante do jogo - time2
  # data - conjunto de dados
  # rodada - representa o n?mero de rodadas realizadas
  # prevrodada - previsao da rodada desejada
  # lambdam - parametro da poisson do time mandante
  # lambdav - parametro da poisson do time visitante
  # meanCBPfm - Media de gols do CB passado feitos como mandante
  # meanCBPsm - Media de gols do CB passado sofridos como mandante
  # meanCBPfv - Media de gols do CB passado feitos como visitante
  # meanCBPsv - Media de gols do CB passado sofridos como visitante
  # nsim - numero de simulacoes

  class <- classific(data, rodada = rodada)

  # Objeto que armazenarar os contadores de resultado
  res <- cbind(class[2],  Camp = 0, Liber = 0, Sul = 0, Reb = 0)

  # Objeto auxiliar que armazenarar as probabilidades por rodada
  prevprobaux <- cbind(data[,1:3], PVM = 0, EMP = 0, PVV = 0)

  # numero de partidas por rodada
  n_partidas <- length(data$Rodada[data$Rodada == 1])
  n_rodadas <- max(data$Rodada)

  # Objeto final que armazenarar as probabilidades por rodada
  prevprob <- subset(prevprobaux, Rodada == rodada)

  #rodada em paralelo, roda independente
  sims <- furrr::future_map(
    seq_len(nsim),
    ~ sim(
      data, rodada, n_partidas, n_rodadas, res, meanCBPfm, meanCBPsm,
      meanCBPfv, meanCBPsv, nsim, Lib, CopaBR,
    ),
    .progress = TRUE
  )



  #esse for soma todos os resultados independentes
  for (n in 1:nsim) {
    x <- sims[[n]]
    res[, 2:5] <- res[,2:5] + x$res[,2:5]
    prevprob[, 4:6] <- prevprob[, 4:6] + x$prevprob[,4:6]
  }

  rownames(res) <- 1:20
  tabelas <- list(Tab1 = res, Tab2 = prevprob)
  names(tabelas)[1] <- "Tabela de probabilidades para Campeao/Libertadores/Sulamericana/Rebaixamento"
  names(tabelas)[2] <- "Tabela de probabilidades da rodada"
  return(tabelas)
  #return(res)
}


