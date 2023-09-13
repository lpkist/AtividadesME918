#' @title Simulador de experimentos
#' @param configuracoes Uma lista de configurações (listas nomeadas) para os experimentos, com valores
# n, n_replicas e lambda.
#' @importFrom purrr map
#' @importFrom dplyr %>%
#' @importFrom dplyr transmute
#' @importFrom dplyr bind_rows
#' @return Um `data frame` com as colunas:
#' \itemize{
#' \item exper: O índice do experimento.
#' \item i: O índice da réplica (dentro do experimento).
#' \item emv: O valor do EMV para a simulação do índice i.
#' \item n: O valor de n utilizado no experimento.
#' \item lambda: O valor de λ utilizado no experimento.
#' }
#' @export
experiment_pois <- function(configuracoes){
  res_lista <- purrr::map(seq_along(configuracoes), function(idx){
    lista <- configuracoes[[idx]]
    amostras <- list_rpois(lista$n, lista$n_replicas, lista$lambda)
    emv <- emv_df(amostras)
    emv %>% transmute(exper = idx, i = replica,
                      emv = emv, n = lista$n, lambda = lista$lambda)
  })
  dplyr::bind_rows(res_lista)
}
