#' @title Gera uma lista com amostras da distribuição Poisson
#' @param n tamanho das amostras;
#' @param n_replicas número de réplicas da simulação;
#' @param lambda Parâmetro utilizado na simulação.
#' @importFrom purrr map
#' @return A função deve retorna uma lista com n_replicas elementos, sendo cada elemento uma amostra de tamanho n (vetor) da distribuição Poisson com parâmetro lambda.
#' @export
list_rpois <- function(n, n_replicas, lambda){
  purrr::map(1:n_replicas, function(x) rpois(n, lambda))
}
