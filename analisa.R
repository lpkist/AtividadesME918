library(devtools)
library(purrr)

simula_distribuicao <- function(plano){
  distribuicao <- plano$distribution
  obs <- plano$obs
  if(distribuicao == "bernoulli"){
    aa <- rbinom(obs, 1, plano$p)
  }else if(distribuicao == "poisson"){
    aa <- rpois(obs, plano$lambda)
  }else if(distribuicao == "normal"){
    aa <- rnorm(obs, mean = plano$mu, sd = sqrt(plano$sigma2))
  }
  return(aa)
}

devtools::source_url("https://ime.unicamp.br/~ra137784/ME918/2023s2/lab01_ig.R")
planos <- input_lab01()       
amostras <- map(planos, simula_distribuicao)


lista_master <- list()
for(i in 1:length(planos)){
  plano <- planos[[i]]
  result <- simula_distribuicao(plano)
  distribuicao <- plano$distribution
  obs <- plano$obs

  if(distribuicao == "bernoulli"){
    p <- plano$p
    lista_master[[i]] <- list("distribution" = distribuicao, "p" = p,
                              "obs" = obs, "result" = result)
  }else if(distribuicao == "poisson"){
    lambda <- plano$lambda
    lista_master[[i]] <- list("distribution" = distribuicao, "lambda" = lambda,
                              "obs" = obs, "result" = result)
  }else if(distribuicao == "normal"){
    mu <- plano$mu
    sigma2 <- plano$sigma2
    lista_master[[i]] <- list("distribution" = distribuicao, "mu" =  mu,
                              "sigma2" = sigma2, "obs" = obs, "result" = result)
  }
}

grafico_list <- list()
for(i in 1:length(planos)){
  plano <- planos[[i]]
  result <- simula_distribuicao(plano)
  distribuicao <- plano$distribution
  obs <- plano$obs
  
  if(distribuicao == "bernoulli"){
    p <- plano$p
    grafico_list[[i]] <- list("distribution" = distribuicao, "param" = p,
                              "obs" = obs, "media_amostral" = mean(result))
  }else if(distribuicao == "poisson"){
    lambda <- plano$lambda
    grafico_list[[i]] <- list("distribution" = distribuicao, "param" = lambda,
                              "obs" = obs, "media_amostral" = mean(result))
  }else if(distribuicao == "normal"){
    mu <- plano$mu
    grafico_list[[i]] <- list("distribution" = distribuicao, "param" =  mu,
                              "obs" = obs, "media_amostral" = mean(result))
  }
}
grafico_df <- data.frame(do.call(rbind, grafico_list))
colnames(grafico_df) <- c("Distribuicao", "param_media", "n_obs", "media_amostral")
grafico_df$Distribuicao <- unlist(grafico_df$Distribuicao)
grafico_df$param_media <- unlist(grafico_df$param_media)
grafico_df$n_obs <- unlist(grafico_df$n_obs)
grafico_df$media_amostral <- unlist(grafico_df$media_amostral)

library(tidyverse)
grafico_df %>% 
  mutate(dif = media_amostral - param_media) %>% 
  ggplot(aes(x = n_obs, y = dif))+
  geom_point()+
  facet_wrap(~Distribuicao)+
  theme_bw()+
  geom_hline(yintercept = 0, color = "red")

