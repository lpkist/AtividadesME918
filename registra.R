source("executa.R")
salva_arquivo <- function(i){
  plano <- planos[[i]]
  result <- simula_distribuicao(plano)
  distribuicao <- plano$distribution
  obs <- plano$obs
  if(i <10){
    i_aux <- paste0("0",as.character(i))
  } else{
    i_aux <- as.character(i)
  }
  
  if(distribuicao == "bernoulli"){
    p <- plano$p
    save(distribuicao, p, obs, result, file = paste0("resultados/simulacao", i_aux, ".RData"))
  }else if(distribuicao == "poisson"){
    lambda <- plano$lambda
    save(distribuicao, lambda, obs, result, file = paste0("resultados/simulacao", i_aux, ".RData"))
  }else if(distribuicao == "normal"){
    mu <- plano$mu
    sigma2 <- plano$sigma2
    save(distribuicao, mu, sigma2, obs, result, file = paste0("resultados/simulacao", i_aux, ".RData"))
  }
}

walk(1:length(planos), salva_arquivo)
