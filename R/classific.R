#' Classificação
#'
#' Essa função gera a classificação da rodada, fiz isso para não importar mais
#' uma base de dados, podemos chamar como uma funçãoo independente.
#'
#' @param bd df salvo no pacote
#' @param rodada rodada para gerar a classificacao.
#'
#' @return a classificação no início da rodada.
#'
#'
classific <- function(bd, rodada){

  # lidar com a rodada 1.
  if (rodada == 1) {
    return(
      bd %>%
        dplyr::select(Time = Time1) %>%
        dplyr::slice(1:20) %>%
        dplyr::mutate(
          Class = 1:20,
          P = 0,
          J = 0,
          V = 0,
          E = 0,
          D = 0,
          GP = 0,
          GC = 0,
          SG = 0
        ) %>%
        dplyr::select(Class, Time, P, J, V, E, D, GP, GC, SG)
    )
  }

  # para as demais rodadas.
  aux <- bd %>%
    dplyr::filter(Rodada < rodada)

  dplyr::bind_rows(
    aux %>% dplyr::select(Time = Time1, GF = GFM, GS = GSM),
    aux %>% dplyr::select(Time = Time2, GF = GFV, GS = GSV)
  ) %>%
    dplyr::mutate(
      J = 1,
      V = GF > GS,
      E = GF == GS,
      D = GF < GS,
      P = 3*V + 1*E,
      GP = GF,
      GC = GS,
      SG = GF - GS
    ) %>%
    dplyr::group_by(Time) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::arrange(dplyr::desc(P), dplyr::desc(V), dplyr::desc(SG), dplyr::desc(GP), GC) %>%
    dplyr::mutate(Class = 1:20) %>%
    dplyr::select(Class, Time, P, J, V, E, D, GP, GC, SG) %>%
    as.data.frame()
}
