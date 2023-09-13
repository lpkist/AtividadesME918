#' @title Cria todas as listas com todas as combinações `n`s e \eqn{\lambda}s informadas
#' @param n Vetor numérico com os tamanhos de amostra
#' @param lambda Vetor numérico com os \eqn{\lambda}s da Poisson
#' @param n_replicas Número de réplicas em cada experimento
#' @export
preguica_de_listar <- function(n, lambda, n_replicas){
  utils::globalVariables(c("replica", "rpois", "emv", "n", "lambda"))
  aux <-map(n, function(a){
    map(lambda, function(lambda_a, n = a){
      list(n = a, n_replicas = n_replicas, lambda = lambda_a)
    })
  })
  do.call(c, aux)
}
experiment_pois(preguica_de_listar(n=c(10,50,100), lambda = c(5,20), n_replicas = 200))
