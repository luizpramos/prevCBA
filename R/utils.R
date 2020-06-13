#' Funcao auxiliar para a simulacao
#'
#' @param x x
#' @param escala vetor de tamanho 5.
#'
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

criar_prevdados <- function(data, rodada, n_partidas) {
  prevdados <- cbind(data[,1:3], GFM = rep(0,length(data[,1])), GSM = 0, GSV = 0, GFV = 0)
  prevdados[1:((rodada - 1) * n_partidas),4:7] <- data[1:((rodada - 1) * n_partidas),4:7]
  prevdados
}

atualizar_prevprob <- function(prevdados, rodada, nsim) {
  x <- prevdados[prevdados$Rodada == rodada,]
  x$PVM <- as.integer(x$GFM > x$GSM)/nsim
  x$EMP <- as.integer(x$GFM == x$GSM)/nsim
  x$PVV <- as.integer(x$GFM < x$GSM)/nsim
  x[, -c(4:7)]
}

atualizar_res <- function(prevdados, res, nsim, CopaBR, Lib) {

  prevclass <- classific(prevdados, rodada = 39)

  # ordenando a classificacao
  prevclass <- prevclass[order(prevclass[3], prevclass[5], prevclass[10], prevclass[8], prevclass[9], decreasing = TRUE),]
  prevclass$Class <- 1:20
  rownames(prevclass) <- 1:20

  if(CopaBR == "NULL" && Lib == "NULL"){

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

  }

  if(any(prevclass$Time[1:6] == CopaBR) && (Lib == "NULL")){

    #Campeao
    res[grep(prevclass[1,2], res$Time),2] = res[grep(prevclass[1,2], res$Time),2] + (1/nsim)

    #libertadores
    res[grep(prevclass[1,2], res$Time),3] = res[grep(prevclass[1,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[2,2], res$Time),3] = res[grep(prevclass[2,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[3,2], res$Time),3] = res[grep(prevclass[3,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[4,2], res$Time),3] = res[grep(prevclass[4,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[5,2], res$Time),3] = res[grep(prevclass[5,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[6,2], res$Time),3] = res[grep(prevclass[6,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[7,2], res$Time),3] = res[grep(prevclass[7,2], res$Time),3] + (1/nsim)

    #sulamericana
    res[grep(prevclass[8,2], res$Time),4] = res[grep(prevclass[8,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[9,2], res$Time),4] = res[grep(prevclass[9,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[10,2], res$Time),4] = res[grep(prevclass[10,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[11,2], res$Time),4] = res[grep(prevclass[11,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[12,2], res$Time),4] = res[grep(prevclass[12,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[7:12] == CopaBR) && (Lib == "NULL")){


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
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[13:20] == CopaBR) && (Lib == "NULL")){

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


    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)
  }

  if(any(prevclass$Time[1:7] == CopaBR) && any(prevclass$Time[1:7] == Lib)){

    #Campeao
    res[grep(prevclass[1,2], res$Time),2] = res[grep(prevclass[1,2], res$Time),2] + (1/nsim)

    #libertadores
    res[grep(prevclass[1,2], res$Time),3] = res[grep(prevclass[1,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[2,2], res$Time),3] = res[grep(prevclass[2,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[3,2], res$Time),3] = res[grep(prevclass[3,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[4,2], res$Time),3] = res[grep(prevclass[4,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[5,2], res$Time),3] = res[grep(prevclass[5,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[6,2], res$Time),3] = res[grep(prevclass[6,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[7,2], res$Time),3] = res[grep(prevclass[7,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[8,2], res$Time),3] = res[grep(prevclass[8,2], res$Time),3] + (1/nsim)

    #sulamericana
    res[grep(prevclass[9,2], res$Time),4] = res[grep(prevclass[9,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[10,2], res$Time),4] = res[grep(prevclass[10,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[11,2], res$Time),4] = res[grep(prevclass[11,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[12,2], res$Time),4] = res[grep(prevclass[12,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[14,2], res$Time),4] = res[grep(prevclass[14,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[1:7] == CopaBR) && any(prevclass$Time[8:13] == Lib)){

    #Campeao
    res[grep(prevclass[1,2], res$Time),2] = res[grep(prevclass[1,2], res$Time),2] + (1/nsim)

    #libertadores
    res[grep(prevclass[1,2], res$Time),3] = res[grep(prevclass[1,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[2,2], res$Time),3] = res[grep(prevclass[2,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[3,2], res$Time),3] = res[grep(prevclass[3,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[4,2], res$Time),3] = res[grep(prevclass[4,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[5,2], res$Time),3] = res[grep(prevclass[5,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[6,2], res$Time),3] = res[grep(prevclass[6,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[7,2], res$Time),3] = res[grep(prevclass[7,2], res$Time),3] + (1/nsim)


    #sulamericana
    res[grep(prevclass[8,2], res$Time),4] = res[grep(prevclass[8,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[9,2], res$Time),4] = res[grep(prevclass[9,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[10,2], res$Time),4] = res[grep(prevclass[10,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[11,2], res$Time),4] = res[grep(prevclass[11,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[12,2], res$Time),4] = res[grep(prevclass[12,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[14,2], res$Time),4] = res[grep(prevclass[14,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[1:7] == CopaBR) && any(prevclass$Time[14:20] == Lib)){

    #Campeao
    res[grep(prevclass[1,2], res$Time),2] = res[grep(prevclass[1,2], res$Time),2] + (1/nsim)

    #libertadores
    res[grep(prevclass[1,2], res$Time),3] = res[grep(prevclass[1,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[2,2], res$Time),3] = res[grep(prevclass[2,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[3,2], res$Time),3] = res[grep(prevclass[3,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[4,2], res$Time),3] = res[grep(prevclass[4,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[5,2], res$Time),3] = res[grep(prevclass[5,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[6,2], res$Time),3] = res[grep(prevclass[6,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[7,2], res$Time),3] = res[grep(prevclass[7,2], res$Time),3] + (1/nsim)


    #sulamericana
    res[grep(prevclass[8,2], res$Time),4] = res[grep(prevclass[8,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[9,2], res$Time),4] = res[grep(prevclass[9,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[10,2], res$Time),4] = res[grep(prevclass[10,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[11,2], res$Time),4] = res[grep(prevclass[11,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[12,2], res$Time),4] = res[grep(prevclass[12,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[8:13] == CopaBR) && any(prevclass$Time[1:7] == Lib)){

    #Campeao
    res[grep(prevclass[1,2], res$Time),2] = res[grep(prevclass[1,2], res$Time),2] + (1/nsim)

    #libertadores
    res[grep(prevclass[1,2], res$Time),3] = res[grep(prevclass[1,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[2,2], res$Time),3] = res[grep(prevclass[2,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[3,2], res$Time),3] = res[grep(prevclass[3,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[4,2], res$Time),3] = res[grep(prevclass[4,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[5,2], res$Time),3] = res[grep(prevclass[5,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[6,2], res$Time),3] = res[grep(prevclass[6,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[7,2], res$Time),3] = res[grep(prevclass[7,2], res$Time),3] + (1/nsim)


    #sulamericana
    res[grep(prevclass[8,2], res$Time),4] = res[grep(prevclass[8,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[9,2], res$Time),4] = res[grep(prevclass[9,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[10,2], res$Time),4] = res[grep(prevclass[10,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[11,2], res$Time),4] = res[grep(prevclass[11,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[12,2], res$Time),4] = res[grep(prevclass[12,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[14,2], res$Time),4] = res[grep(prevclass[14,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[8:13] == CopaBR) && any(prevclass$Time[8:13] == Lib)){

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
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[14,2], res$Time),4] = res[grep(prevclass[14,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[8:13] == CopaBR) && any(prevclass$Time[14:20] == Lib)){

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
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)


    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[14:20] == CopaBR) && any(prevclass$Time[1:7] == Lib)){

    #Campeao
    res[grep(prevclass[1,2], res$Time),2] = res[grep(prevclass[1,2], res$Time),2] + (1/nsim)

    #libertadores
    res[grep(prevclass[1,2], res$Time),3] = res[grep(prevclass[1,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[2,2], res$Time),3] = res[grep(prevclass[2,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[3,2], res$Time),3] = res[grep(prevclass[3,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[4,2], res$Time),3] = res[grep(prevclass[4,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[5,2], res$Time),3] = res[grep(prevclass[5,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[6,2], res$Time),3] = res[grep(prevclass[6,2], res$Time),3] + (1/nsim)
    res[grep(prevclass[7,2], res$Time),3] = res[grep(prevclass[7,2], res$Time),3] + (1/nsim)


    #sulamericana
    res[grep(prevclass[8,2], res$Time),4] = res[grep(prevclass[8,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[9,2], res$Time),4] = res[grep(prevclass[9,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[10,2], res$Time),4] = res[grep(prevclass[10,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[11,2], res$Time),4] = res[grep(prevclass[11,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[12,2], res$Time),4] = res[grep(prevclass[12,2], res$Time),4] + (1/nsim)
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)

    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[14:20] == CopaBR) && any(prevclass$Time[8:13] == Lib)){

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
    res[grep(prevclass[13,2], res$Time),4] = res[grep(prevclass[13,2], res$Time),4] + (1/nsim)


    #rebaixamento
    res[grep(prevclass[17,2], res$Time),5] = res[grep(prevclass[17,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[18,2], res$Time),5] = res[grep(prevclass[18,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[19,2], res$Time),5] = res[grep(prevclass[19,2], res$Time),5] + (1/nsim)
    res[grep(prevclass[20,2], res$Time),5] = res[grep(prevclass[20,2], res$Time),5] + (1/nsim)

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }

  if(any(prevclass$Time[14:20] == CopaBR) && any(prevclass$Time[14:20] == Lib)){

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

    #CopaBR recebendo 100.00 como Lib e 0.00 com Sub

    res[grep(CopaBR, res$Time),4]  = (0/nsim)
    res[grep(CopaBR, res$Time),3]  = (1/nsim)

    res[grep(Lib, res$Time),4]  = (0/nsim)
    res[grep(Lib, res$Time),3]  = (1/nsim)

  }


  res
}

# Funcao para atualizacao da media de gols feitos como mandante
#Essa funcao pega todos os feitos como mandante da tabela prevdados e faz um m?dia,
meanCBPfm_atual <-  function(prevdados, i, meanCBPfm = meanCBPfm ){
  tabela <- with(prevdados[prevdados$Rodada < i,], table(Time1, GFM))
  npart <- apply(tabela, 1, sum)
  x <- cbind(as.numeric(colnames(tabela)))
  ngols <- tabela %*% x
  return((ngols + meanCBPfm)/(npart + 1))
}

# Funcao para atualizacao da media de gols sofridos como mandante
meanCBPsm_atual <-  function(prevdados, i, meanCBPsm){

  tabela <- with(prevdados[prevdados$Rodada < i,],
                 table(Time1, GSM))
  npart <- apply(tabela, 1, sum)
  x <- cbind(as.numeric(colnames(tabela)))
  ngols <- tabela %*% x
  return((ngols + meanCBPsm)/(npart + 1))
}


# Funcao para atualizacao da media de gols feitos como visitantes
meanCBPfv_atual <-  function(prevdados, i, meanCBPfv){

  tabela <- with(prevdados[prevdados$Rodada < i,],
                 table(Time2, GFV))
  npart <- apply(tabela, 1, sum)
  x <- cbind(as.numeric(colnames(tabela)))
  ngols <- tabela %*% x
  return((ngols + meanCBPfv)/(npart + 1))
}


# Funcao para atualizacao da media de gols sofridos como visitantes
meanCBPsv_atual <-  function(prevdados, i, meanCBPsv){

  tabela <- with(prevdados[prevdados$Rodada < i,],
                 table(Time2, GSV))
  npart <- apply(tabela, 1, sum)
  x <- cbind(as.numeric(colnames(tabela)))
  ngols <- tabela %*% x
  return((ngols + meanCBPsv)/(npart + 1))
}
