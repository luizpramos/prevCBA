#####################################
# Trabalho de Luiz Fernando - PROFMAT
#####################################
# DESCRICAO:
# Previsao dos jogos do campeonato brasileiro de futebol- Serie A

#' Original
#'
#'
prevCBA_original <- function(data = dados, dados, rodada, meanCBPfm, meanCBPsm,
                    meanCBPfv, meanCBPsv, nsim) {

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

  # Funcao auxiliar para a simulacao
  simaux <- function(x, escala){
    if (x < escala[1]) {
      res <- 0
    }else if (x >= escala[1] & x < escala[2]) {
      res <- 1
    } else if (x >= escala[2] & x < escala[3]) {
      res <- 2
    } else if (x >= escala[3] & x < escala[4]) {
      res <- 3
    } else if (x >= escala[4] & x < escala[5]) {
      res <- 4
    } else if (x > escala[5]) {
      res <- 5
    }
    return(res)
  }

  #essa fun??o classific gera a classifica??o da rodada, fiz isso para n?o importar mais uma base de dados, podemos chamar como uma fun??o independente
  classific <- function(data = dados, rodada){


    #criando objetos de armazenamento
    prevclass <- cbind(data[1:20,1:2], P = 0, J = 0, V = 0, E = 0, D = 0, GP = 0, GC = 0, SG = 0)
    prevdados <- cbind(data[,1:3], GFM = rep(0,length(data[,1])), GSM = 0, GSV = 0, GFV = 0)
    colnames(prevclass)[2] <- "Time"
    prevclass$Rodada <- 1:20
    rownames(prevclass) <- 1:20
    colnames(prevclass)[1] <- "Class"


    # o if come?a em 2 pq na primeira rodada a classifica??o ? zerada para todo mundo
    if (rodada > 1) {


      for (d in 1:(rodada - 1)) {


        #prevdadosaux recebe apenas os dados da rodada informada
        prevdadosaux <- subset(data, Rodada == d)

        # Contadores
        cont1 <- ((d - 1)*(length(prevdados$Rodada[prevdados$Rodada == 1])) + 1)
        cont2 <- d*(length(prevdados$Rodada[prevdados$Rodada == 1]))
        for (c in  cont1:cont2) {

          # Buscando time1 e time2 de acordo com coluna da rodada
          time1 <- as.character(prevdados$Time1[c])
          time2 <- as.character(prevdados$Time2[c])



          # Atualizando tabela de classifica??o (tabela prevclass) precisa passar pra ela quem ? time1 e time2
          # vit?ria do time1

          if (prevdadosaux[grep(time1, prevdadosaux$Time1),4] > prevdadosaux[grep(time1, prevdadosaux$Time1),5]) {

            prevclass[grep(time1, prevclass$Time),3] <- prevclass[grep(time1, prevclass$Time),3] + 3
            prevclass[grep(time1, prevclass$Time),4] <- prevclass[grep(time1, prevclass$Time),4] + 1
            prevclass[grep(time1, prevclass$Time),5] <- prevclass[grep(time1, prevclass$Time),5] + 1
            prevclass[grep(time1, prevclass$Time),6] <- prevclass[grep(time1, prevclass$Time),6] + 0
            prevclass[grep(time1, prevclass$Time),7] <- prevclass[grep(time1, prevclass$Time),7] + 0
            prevclass[grep(time1, prevclass$Time),8] <- prevclass[grep(time1, prevclass$Time),8] + prevdadosaux[grep(time1, prevdadosaux$Time1),4]
            prevclass[grep(time1, prevclass$Time),9] <- prevclass[grep(time1, prevclass$Time),9] + prevdadosaux[grep(time1, prevdadosaux$Time1),5]
            prevclass[grep(time1, prevclass$Time),10] <- prevclass[grep(time1, prevclass$Time),10] + (prevdadosaux[grep(time1, prevdadosaux$Time1),4] - prevdadosaux[grep(time1, prevdadosaux$Time1),5])

            prevclass[grep(time2, prevclass$Time),3] <- prevclass[grep(time2, prevclass$Time),3] + 0
            prevclass[grep(time2, prevclass$Time),4] <- prevclass[grep(time2, prevclass$Time),4] + 1
            prevclass[grep(time2, prevclass$Time),5] <- prevclass[grep(time2, prevclass$Time),5] + 0
            prevclass[grep(time2, prevclass$Time),6] <- prevclass[grep(time2, prevclass$Time),6] + 0
            prevclass[grep(time2, prevclass$Time),7] <- prevclass[grep(time2, prevclass$Time),7] + 1
            prevclass[grep(time2, prevclass$Time),8] <- prevclass[grep(time2, prevclass$Time),8] + prevdadosaux[grep(time1, prevdadosaux$Time1),5]
            prevclass[grep(time2, prevclass$Time),9] <- prevclass[grep(time2, prevclass$Time),9] + prevdadosaux[grep(time1, prevdadosaux$Time1),4]
            prevclass[grep(time2, prevclass$Time),10] <- prevclass[grep(time2, prevclass$Time),10] + (prevdadosaux[grep(time1, prevdadosaux$Time1),5] - prevdadosaux[grep(time1, prevdadosaux$Time1),4])


          }

          # vit?ria do time2

          if (prevdadosaux[grep(time1, prevdadosaux$Time1),4] < prevdadosaux[grep(time1, prevdadosaux$Time1),5]) {

            prevclass[grep(time2, prevclass$Time),3] <- prevclass[grep(time2, prevclass$Time),3] + 3
            prevclass[grep(time2, prevclass$Time),4] <- prevclass[grep(time2, prevclass$Time),4] + 1
            prevclass[grep(time2, prevclass$Time),5] <- prevclass[grep(time2, prevclass$Time),5] + 1
            prevclass[grep(time2, prevclass$Time),6] <- prevclass[grep(time2, prevclass$Time),6] + 0
            prevclass[grep(time2, prevclass$Time),7] <- prevclass[grep(time2, prevclass$Time),7] + 0
            prevclass[grep(time2, prevclass$Time),8] <- prevclass[grep(time2, prevclass$Time),8] + prevdadosaux[grep(time1, prevdadosaux$Time1),5]
            prevclass[grep(time2, prevclass$Time),9] <- prevclass[grep(time2, prevclass$Time),9] + prevdadosaux[grep(time1, prevdadosaux$Time1),4]
            prevclass[grep(time2, prevclass$Time),10] <- prevclass[grep(time2, prevclass$Time),10] + (prevdadosaux[grep(time1, prevdadosaux$Time1),5] - prevdadosaux[grep(time1, prevdadosaux$Time1),4])

            prevclass[grep(time1, prevclass$Time),3] <- prevclass[grep(time1, prevclass$Time),3] + 0
            prevclass[grep(time1, prevclass$Time),4] <- prevclass[grep(time1, prevclass$Time),4] + 1
            prevclass[grep(time1, prevclass$Time),5] <- prevclass[grep(time1, prevclass$Time),5] + 0
            prevclass[grep(time1, prevclass$Time),6] <- prevclass[grep(time1, prevclass$Time),6] + 0
            prevclass[grep(time1, prevclass$Time),7] <- prevclass[grep(time1, prevclass$Time),7] + 1
            prevclass[grep(time1, prevclass$Time),8] <- prevclass[grep(time1, prevclass$Time),8] + prevdadosaux[grep(time1, prevdadosaux$Time1),4]
            prevclass[grep(time1, prevclass$Time),9] <- prevclass[grep(time1, prevclass$Time),9] + prevdadosaux[grep(time1, prevdadosaux$Time1),5]
            prevclass[grep(time1, prevclass$Time),10] <- prevclass[grep(time1, prevclass$Time),10] + (prevdadosaux[grep(time1, prevdadosaux$Time1),4] - prevdadosaux[grep(time1, prevdadosaux$Time1),5])

          }


          # empate

          if (prevdadosaux[grep(time1, prevdadosaux$Time1),4] == prevdadosaux[grep(time1, prevdadosaux$Time1),5]) {

            prevclass[grep(time1, prevclass$Time),3] <- prevclass[grep(time1, prevclass$Time),3] + 1
            prevclass[grep(time1, prevclass$Time),4] <- prevclass[grep(time1, prevclass$Time),4] + 1
            prevclass[grep(time1, prevclass$Time),5] <- prevclass[grep(time1, prevclass$Time),5] + 0
            prevclass[grep(time1, prevclass$Time),6] <- prevclass[grep(time1, prevclass$Time),6] + 1
            prevclass[grep(time1, prevclass$Time),7] <- prevclass[grep(time1, prevclass$Time),7] + 0
            prevclass[grep(time1, prevclass$Time),8] <- prevclass[grep(time1, prevclass$Time),8] + prevdadosaux[grep(time1, prevdadosaux$Time1),4]
            prevclass[grep(time1, prevclass$Time),9] <- prevclass[grep(time1, prevclass$Time),9] + prevdadosaux[grep(time1, prevdadosaux$Time1),5]
            prevclass[grep(time1, prevclass$Time),10] <- prevclass[grep(time1, prevclass$Time),10] + 0

            prevclass[grep(time2, prevclass$Time),3] <- prevclass[grep(time2, prevclass$Time),3] + 1
            prevclass[grep(time2, prevclass$Time),4] <- prevclass[grep(time2, prevclass$Time),4] + 1
            prevclass[grep(time2, prevclass$Time),5] <- prevclass[grep(time2, prevclass$Time),5] + 0
            prevclass[grep(time2, prevclass$Time),6] <- prevclass[grep(time2, prevclass$Time),6] + 1
            prevclass[grep(time2, prevclass$Time),7] <- prevclass[grep(time2, prevclass$Time),7] + 0
            prevclass[grep(time2, prevclass$Time),8] <- prevclass[grep(time2, prevclass$Time),8] + prevdadosaux[grep(time1, prevdadosaux$Time1),5]
            prevclass[grep(time2, prevclass$Time),9] <- prevclass[grep(time2, prevclass$Time),9] + prevdadosaux[grep(time1, prevdadosaux$Time1),4]
            prevclass[grep(time2, prevclass$Time),10] <- prevclass[grep(time2, prevclass$Time),10] + 0

          }


        }#for dos jogos

      } #for das rodadas

    }#if (rodada > 1)


    # ordenando a classifica??o
    prevclass <- prevclass[order(prevclass[3], prevclass[5], prevclass[10], prevclass[8], prevclass[9], decreasing = TRUE),]
    prevclass$Class <- 1:20

    return(prevclass)
  }#funcao classific

  class = classific(data = dados, rodada = rodada)

  # Objeto que armazenarar os contadores de resultado
  res <- cbind(class[2:2],  Camp = 0, Lib = 0, Sul = 0, Reb = 0)

  # Objeto auxiliar que armazenarar as probabilidades por rodada
  prevprobaux <- cbind(data[,1:3], PVM = 0, EMP = 0, PVV = 0)

  #recebendo rodada

  i = rodada

  # Objeto final que armazenarar as probabilidades por rodada
  prevprob <- subset(prevprobaux, Rodada == i)


  for (n in 1:nsim) {

    #tudo que tiver prev ? um objeto para armazenar os resultados simulados

    # Objeto que armazenarar a previsao dos jogos
    prevdados <- cbind(data[,1:3], GFM = rep(0,length(data[,1])), GSM = 0, GSV = 0, GFV = 0)

    # Objeto que armazenarar a previsao da classificacao
    prevclass <- cbind(class[,1:2], P = rep(0,length(class[,1])), J = 0, V = 0, E = 0, D = 0, GP = 0, GC = 0, SG = 0)

    # Atualizando o class com o banco de dados informado
    prevclass = class

    # Atualizando o prevdados com o banco de dados informado
    prevdados[1:((rodada - 1) * length(prevdados$Rodada[prevdados$Rodada == 1])),4:7] <- data[1:((rodada - 1) * length(data$Rodada[data$Rodada == 1])),4:7]

    # Contador de partidas por rodada

    if (rodada == 1) {
      j <- 1
    }

    #Aqui o J recebe 11 (10 + 1), porque a segunda rodada come?a na decima primeira linha, s?o 10 jogos por rodada
    if (rodada > 1) {
      j <- (length(prevdados$Rodada[prevdados$Rodada == 1])*(rodada - 1)) + 1
    }


    #for que varre as rodada
    for (i in rodada:max(dados$Rodada)) {


      if (i == 1) {

        #for que varre os jogos dentro de uma rodada (ele vai de 1 at? 10)

        for (r in j:(length(prevdados$Rodada[prevdados$Rodada == 1]))) {


          lambdam <- meanCBPfm + meanCBPsv # parametro da poisson do time mandante
          lambdav <- meanCBPsm + meanCBPfv # parametro da poisson do time visitante


          # Escala para mandante
          escm <- ppois(c(0, 2, 4, 6, 8, 10), lambda = lambdam)

          # Escala para visitante
          escv <- ppois(c(0, 2, 4, 6, 8, 10), lambda = lambdav)


                    # Uma simulacao para o mandante
          simm <- cbind(runif(1, 0, 1))


          # Armazenando os resultados dos gols como mandante
          resm <- apply(simm, 1, simaux, escala = escm)

          # Simulacao para o visitante
          simv <- cbind(runif(1, 0, 1))

          # Armazenando os resultados dos gols como visitante
          resv <- apply(simv, 1, simaux, escala = escv)

          # Buscando time1 e time2 de acordo com coluna da rodada
          time1 <- as.character(prevdados$Time1[r])
          time2 <- as.character(prevdados$Time2[r])

          #Vai na coluna da rodada em prevdados e acrescenta o resultado do jogo

          prevdados$GFM[r] <- round(mean(resm), 0)# aqui entra os Gols Feitos como Mandante (GFM)
          prevdados$GSM[r] <- round(mean(resv), 0)# gols sofridos como mandante
          prevdados$GFV[r] <- round(mean(resv), 0)
          prevdados$GSV[r] <- round(mean(resm), 0)

          # Atualizando tabela de classifica??o e tabela de probabilidade



          # vit?ria do time1

          if (round(mean(resm), 0) > round(mean(resv), 0)) {

            prevprob[grep(time1, prevprob$Time1),4] <- prevprob[grep(time1, prevprob$Time1),4] + 1

            prevclass[grep(time1, prevclass$Time),3] <- 3# 3 pontos da vitoria
            prevclass[grep(time1, prevclass$Time),4] <- 1# 1 jogo
            prevclass[grep(time1, prevclass$Time),5] <- 1# 1 derrota
            prevclass[grep(time1, prevclass$Time),6] <- 0#zero empate
            prevclass[grep(time1, prevclass$Time),7] <- 0#zero derrota
            prevclass[grep(time1, prevclass$Time),8] <- round(mean(resm), 0)#gols feitos
            prevclass[grep(time1, prevclass$Time),9] <- round(mean(resv), 0)#gols sofridos
            prevclass[grep(time1, prevclass$Time),10] <- (round(mean(resm), 0) - round(mean(resv), 0))#saldo de gols

            prevclass[grep(time2, prevclass$Time),3] <- 0#zero pontos
            prevclass[grep(time2, prevclass$Time),4] <- 1# 1 jogo
            prevclass[grep(time2, prevclass$Time),5] <- 0
            prevclass[grep(time2, prevclass$Time),6] <- 0
            prevclass[grep(time2, prevclass$Time),7] <- 1#1 derrota
            prevclass[grep(time2, prevclass$Time),8] <- round(mean(resv), 0)
            prevclass[grep(time2, prevclass$Time),9] <- round(mean(resm), 0)
            prevclass[grep(time2, prevclass$Time),10] <- (round(mean(resv), 0) - round(mean(resm), 0))

          }


          # vit?ria do time2

          if (round(mean(resm), 0) < round(mean(resv), 0)) {

            prevprob[grep(time1, prevprob$Time1),6] <- prevprob[grep(time1, prevprob$Time1),6] + 1

            prevclass[grep(time2, prevclass$Time),3] <- 3
            prevclass[grep(time2, prevclass$Time),4] <- 1
            prevclass[grep(time2, prevclass$Time),5] <- 1
            prevclass[grep(time2, prevclass$Time),6] <- 0
            prevclass[grep(time2, prevclass$Time),7] <- 0
            prevclass[grep(time2, prevclass$Time),8] <- round(mean(resv), 0)
            prevclass[grep(time2, prevclass$Time),9] <- round(mean(resm), 0)
            prevclass[grep(time2, prevclass$Time),10] <- (round(mean(resv), 0) - round(mean(resm), 0))

            prevclass[grep(time1, prevclass$Time),3] <- 0
            prevclass[grep(time1, prevclass$Time),4] <- 1
            prevclass[grep(time1, prevclass$Time),5] <- 0
            prevclass[grep(time1, prevclass$Time),6] <- 0
            prevclass[grep(time1, prevclass$Time),7] <- 1
            prevclass[grep(time1, prevclass$Time),8] <- round(mean(resm), 0)
            prevclass[grep(time1, prevclass$Time),9] <- round(mean(resv), 0)
            prevclass[grep(time1, prevclass$Time),10] <- (round(mean(resm), 0) - round(mean(resv), 0))

          }


          # empate

          if (round(mean(resm), 0) == round(mean(resv), 0)) {

            prevprob[grep(time1, prevprob$Time1),5] <- prevprob[grep(time1, prevprob$Time1),5] + 1

            prevclass[grep(time1, prevclass$Time),3] <- 1
            prevclass[grep(time1, prevclass$Time),4] <- 1
            prevclass[grep(time1, prevclass$Time),5] <- 0
            prevclass[grep(time1, prevclass$Time),6] <- 1
            prevclass[grep(time1, prevclass$Time),7] <- 0
            prevclass[grep(time1, prevclass$Time),8] <- round(mean(resm), 0)
            prevclass[grep(time1, prevclass$Time),9] <- round(mean(resv), 0)
            prevclass[grep(time1, prevclass$Time),10] <- 0

            prevclass[grep(time2, prevclass$Time),3] <- 1
            prevclass[grep(time2, prevclass$Time),4] <- 1
            prevclass[grep(time2, prevclass$Time),5] <- 0
            prevclass[grep(time2, prevclass$Time),6] <- 1
            prevclass[grep(time2, prevclass$Time),7] <- 0
            prevclass[grep(time2, prevclass$Time),8] <- round(mean(resv), 0)
            prevclass[grep(time2, prevclass$Time),9] <- round(mean(resm), 0)
            prevclass[grep(time2, prevclass$Time),10] <- 0

          }


          # Atualizando a j-esima rodada
          j <- j + 1
        }#for() dentro do if(i==1)

      }#if (i==1)


      if (i > 1) {

        # Funcao para atualizacao da media de gols feitos como mandante
        #Essa fun??o pega todos os feitos como mandante da tabela prevdados e faz um m?dia,
        meanCBPfm_atual <-  function(prevdados, i, meanCBPfm = meanCBPfm){

          tabela <- with(prevdados[prevdados$Rodada < i,], table(Time1, GFM))
          npart <- apply(tabela, 1, sum)
          x <- cbind(as.numeric(colnames(tabela)))
          ngols <- tabela %*% x
          return((ngols + meanCBPfm)/(npart + 1))
        }


        # Funcao para atualizacao da media de gols sofridos como mandante
        meanCBPsm_atual <-  function(data, i, meanCBPsm){

          tabela <- with(prevdados[prevdados$Rodada < i,],
                         table(Time1, GSM))
          npart <- apply(tabela, 1, sum)
          x <- cbind(as.numeric(colnames(tabela)))
          ngols <- tabela %*% x
          return((ngols + meanCBPsm)/(npart + 1))
        }


        # Funcao para atualizacao da media de gols feitos como visitantes
        meanCBPfv_atual <-  function(data, i, meanCBPfv){

          tabela <- with(prevdados[prevdados$Rodada < i,],
                         table(Time2, GFV))
          npart <- apply(tabela, 1, sum)
          x <- cbind(as.numeric(colnames(tabela)))
          ngols <- tabela %*% x
          return((ngols + meanCBPfv)/(npart + 1))
        }


        # Funcao para atualizacao da media de gols sofridos como visitantes
        meanCBPsv_atual <-  function(data, i, meanCBPsv){

          tabela <- with(prevdados[prevdados$Rodada < i,],
                         table(Time2, GSV))
          npart <- apply(tabela, 1, sum)
          x <- cbind(as.numeric(colnames(tabela)))
          ngols <- tabela %*% x
          return((ngols + meanCBPsv)/(npart + 1))
        }

        # Atualizacao dos lambdas da media de gols
        lauxfm <- meanCBPfm_atual(prevdados, i, meanCBPfm)
        lauxsv <- meanCBPsv_atual(prevdados, i, meanCBPsv)
        lauxfv <- meanCBPfv_atual(prevdados, i, meanCBPfv)
        lauxsm <- meanCBPsm_atual(prevdados, i, meanCBPsm)


        #for que varre os jogos dentro de uma rodada
        for (r in j:((j - 1) + length(prevdados$Rodada[prevdados$Rodada == 1]))) {

          # Buscando time1 e time2 de acordo com coluna da rodada
          time1 <- as.character(prevdados$Time1[r])
          time2 <- as.character(prevdados$Time2[r])

          # Buscando as medias dos time1 e time2 de acordo com coluna da rodada
          lfm <- lauxfm[rownames(lauxfm) == time1]
          lsv <- lauxsv[rownames(lauxsv) == time2]
          lfv <- lauxfv[rownames(lauxfv) == time2]
          lsm <- lauxsm[rownames(lauxsm) == time1]

          lambdam <- lfm + lsv # parametro da poisson do time mandante
          lambdav <- lsm + lfv # parametro da poisson do time visitante


          # Escala para mandante
          escm <- ppois(c(0, 2, 4, 6, 8, 10), lambda = lambdam)

          # Escala para visitante
          escv <- ppois(c(0, 2, 4, 6, 8, 10), lambda = lambdav)

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


          # Atualizando tabela de classifica??o (tabela prevclass) precisa passar pra ela quem ? time1 e time2 e tabela de probabilidade

          # vit?ria do time1

          if (round(mean(resm), 0) > round(mean(resv), 0)) {

            prevprob[grep(time1, prevprob$Time1),4] <- prevprob[grep(time1, prevprob$Time1),4] + 1

            prevclass[grep(time1, prevclass$Time),3] <- prevclass[grep(time1, prevclass$Time),3] + 3
            prevclass[grep(time1, prevclass$Time),4] <- prevclass[grep(time1, prevclass$Time),4] + 1
            prevclass[grep(time1, prevclass$Time),5] <- prevclass[grep(time1, prevclass$Time),5] + 1
            prevclass[grep(time1, prevclass$Time),6] <- prevclass[grep(time1, prevclass$Time),6] + 0
            prevclass[grep(time1, prevclass$Time),7] <- prevclass[grep(time1, prevclass$Time),7] + 0
            prevclass[grep(time1, prevclass$Time),8] <- prevclass[grep(time1, prevclass$Time),8] + round(mean(resm), 0)
            prevclass[grep(time1, prevclass$Time),9] <- prevclass[grep(time1, prevclass$Time),9] + round(mean(resv), 0)
            prevclass[grep(time1, prevclass$Time),10] <- prevclass[grep(time1, prevclass$Time),10] + (round(mean(resm), 0) - round(mean(resv), 0))

            prevclass[grep(time2, prevclass$Time),3] <- prevclass[grep(time2, prevclass$Time),3] + 0
            prevclass[grep(time2, prevclass$Time),4] <- prevclass[grep(time2, prevclass$Time),4] + 1
            prevclass[grep(time2, prevclass$Time),5] <- prevclass[grep(time2, prevclass$Time),5] + 0
            prevclass[grep(time2, prevclass$Time),6] <- prevclass[grep(time2, prevclass$Time),6] + 0
            prevclass[grep(time2, prevclass$Time),7] <- prevclass[grep(time2, prevclass$Time),7] + 1
            prevclass[grep(time2, prevclass$Time),8] <- prevclass[grep(time2, prevclass$Time),8] + round(mean(resv), 0)
            prevclass[grep(time2, prevclass$Time),9] <- prevclass[grep(time2, prevclass$Time),9] + round(mean(resm), 0)
            prevclass[grep(time2, prevclass$Time),10] <- prevclass[grep(time2, prevclass$Time),10] + (round(mean(resv), 0) - round(mean(resm), 0))

          }

          # vit?ria do time2

          if (round(mean(resm), 0) < round(mean(resv), 0)) {


            prevprob[grep(time1, prevprob$Time1),6] <- prevprob[grep(time1, prevprob$Time1),6] + 1

            prevclass[grep(time2, prevclass$Time),3] <- prevclass[grep(time2, prevclass$Time),3] + 3
            prevclass[grep(time2, prevclass$Time),4] <- prevclass[grep(time2, prevclass$Time),4] + 1
            prevclass[grep(time2, prevclass$Time),5] <- prevclass[grep(time2, prevclass$Time),5] + 1
            prevclass[grep(time2, prevclass$Time),6] <- prevclass[grep(time2, prevclass$Time),6] + 0
            prevclass[grep(time2, prevclass$Time),7] <- prevclass[grep(time2, prevclass$Time),7] + 0
            prevclass[grep(time2, prevclass$Time),8] <- prevclass[grep(time2, prevclass$Time),8] + round(mean(resv), 0)
            prevclass[grep(time2, prevclass$Time),9] <- prevclass[grep(time2, prevclass$Time),9] + round(mean(resm), 0)
            prevclass[grep(time2, prevclass$Time),10] <- prevclass[grep(time2, prevclass$Time),10] + (round(mean(resv), 0) - round(mean(resm), 0))

            prevclass[grep(time1, prevclass$Time),3] <- prevclass[grep(time1, prevclass$Time),3] + 0
            prevclass[grep(time1, prevclass$Time),4] <- prevclass[grep(time1, prevclass$Time),4] + 1
            prevclass[grep(time1, prevclass$Time),5] <- prevclass[grep(time1, prevclass$Time),5] + 0
            prevclass[grep(time1, prevclass$Time),6] <- prevclass[grep(time1, prevclass$Time),6] + 0
            prevclass[grep(time1, prevclass$Time),7] <- prevclass[grep(time1, prevclass$Time),7] + 1
            prevclass[grep(time1, prevclass$Time),8] <- prevclass[grep(time1, prevclass$Time),8] + round(mean(resm), 0)
            prevclass[grep(time1, prevclass$Time),9] <- prevclass[grep(time1, prevclass$Time),9] + round(mean(resv), 0)
            prevclass[grep(time1, prevclass$Time),10] <- prevclass[grep(time1, prevclass$Time),10] + (round(mean(resm), 0) - round(mean(resv), 0))

          }


          # empate

          if (round(mean(resm), 0) == round(mean(resv), 0)) {

            prevprob[grep(time1, prevprob$Time1),5] <- prevprob[grep(time1, prevprob$Time1),5] + 1

            prevclass[grep(time1, prevclass$Time),3] <- prevclass[grep(time1, prevclass$Time),3] + 1
            prevclass[grep(time1, prevclass$Time),4] <- prevclass[grep(time1, prevclass$Time),4] + 1
            prevclass[grep(time1, prevclass$Time),5] <- prevclass[grep(time1, prevclass$Time),5] + 0
            prevclass[grep(time1, prevclass$Time),6] <- prevclass[grep(time1, prevclass$Time),6] + 1
            prevclass[grep(time1, prevclass$Time),7] <- prevclass[grep(time1, prevclass$Time),7] + 0
            prevclass[grep(time1, prevclass$Time),8] <- prevclass[grep(time1, prevclass$Time),8] + round(mean(resm), 0)
            prevclass[grep(time1, prevclass$Time),9] <- prevclass[grep(time1, prevclass$Time),9] + round(mean(resv), 0)
            prevclass[grep(time1, prevclass$Time),10] <- prevclass[grep(time1, prevclass$Time),10] + 0

            prevclass[grep(time2, prevclass$Time),3] <- prevclass[grep(time2, prevclass$Time),3] + 1
            prevclass[grep(time2, prevclass$Time),4] <- prevclass[grep(time2, prevclass$Time),4] + 1
            prevclass[grep(time2, prevclass$Time),5] <- prevclass[grep(time2, prevclass$Time),5] + 0
            prevclass[grep(time2, prevclass$Time),6] <- prevclass[grep(time2, prevclass$Time),6] + 1
            prevclass[grep(time2, prevclass$Time),7] <- prevclass[grep(time2, prevclass$Time),7] + 0
            prevclass[grep(time2, prevclass$Time),8] <- prevclass[grep(time2, prevclass$Time),8] + round(mean(resv), 0)
            prevclass[grep(time2, prevclass$Time),9] <- prevclass[grep(time2, prevclass$Time),9] + round(mean(resm), 0)
            prevclass[grep(time2, prevclass$Time),10] <- prevclass[grep(time2, prevclass$Time),10] + 0

          }


          # Atualizando a j-esima rodada
          j <- j + 1
        }#for() dentro do if(i > 1)
      } # if(i > 1)
    } # for() das rodadas

    # ordenando a classifica??o
    prevclass <- prevclass[order(prevclass[3], prevclass[5], prevclass[10], prevclass[8], prevclass[9], decreasing = TRUE),]
    prevclass$Class <- 1:20
    rownames(prevclass) <- 1:20



    #Campeao
    res[grep(prevclass[1,2], res$Time),2] = res[grep(prevclass[1,2], res$Time),2] + (1/nsim)

    #libertadores
    res[grep(prevclass[1,2], res$Time),3] = res[grep(prevclass[1,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[2,2], res$Time),3] = res[grep(prevclass[2,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[3,2], res$Time),3] = res[grep(prevclass[3,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[4,2], res$Time),3] = res[grep(prevclass[4,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[5,2], res$Time),3] = res[grep(prevclass[5,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[6,2], res$Time),3] = res[grep(prevclass[6,2], res$Time),3] + (1/nsim)

    #sulamericana
    res[grep(prevclass[7,2], res$Time),4] = res[grep(prevclass[7,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[8,2], res$Time),4] = res[grep(prevclass[8,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[9,2], res$Time),4] = res[grep(prevclass[9,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[10,2], res$Time),4] = res[grep(prevclass[10,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[11,2], res$Time),4] = res[grep(prevclass[11,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[12,2], res$Time),4] = res[grep(prevclass[12,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

  }#for das n simulacoes

  #dividindo a quantidade de resultados pelo total simulado

  total = (prevprob[1,4] + prevprob[1,5] + prevprob[1,6])

  prevprob[,4] = prevprob[,4] / total
  prevprob[,5] = prevprob[,5] / total
  prevprob[,6] = prevprob[,6] / total

  prevprob[,4] = round(prevprob[,4], 3)
  prevprob[,5] = round(prevprob[,5], 3)
  prevprob[,6] = round(prevprob[,6], 3)


  res[,2] = round(res[,2], 3)
  res[,3] = round(res[,3], 3)
  res[,4] = round(res[,4], 3)
  res[,5] = round(res[,5], 3)

  rownames(res) <- 1:20
  tabelas <- list(Tab1 = res, Tab2 = prevprob)
  names(tabelas)[1] <- "Tabela de probabilidades para Campe?o/Libertadores/Sulamericana/Rebaixamento"
  names(tabelas)[2] <- "Tabela de probabilidades da rodada"
  return(tabelas)


}#primeira fu??o


