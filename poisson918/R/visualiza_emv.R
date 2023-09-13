#' @title Visualizador dos EMV de \eqn{\lambda} para Poisson
#' @param data um `data frame` no formato de saída da função experiment_pois
#' @import ggplot2
#' @return Um gráfico feito no ggplot2 com a visualização dos dados
#' @export
visualiza_emv <- function(data){
  ggplot(data, aes(x = emv))+
    geom_density(aes(color = factor(n)))+
    geom_vline(aes(xintercept = lambda), color = "red", linetype = 2)+
    facet_wrap(~lambda, scales = "free")+
    theme_bw()+
    labs(x = paste0("Estimador de m", "\\u00e1", "xima verossimilhan", "\\u00e7", "a"),
         color = "Tamanho da amostra",
         title = expression(paste("Distribui", "\\u00e7", "\\u00f5", "es dos EMV para v", "\\u00e1", "rios ", lambda, " e ns")))

}
