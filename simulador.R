f <- function(arg){
  distribuicao <- arg$distribution
  obs <- arg$obs
  if(distribuicao == "bernoulli"){
    aa <- rbinom(obs, 1, arg$p)
  }else if(distribuicao == "poisson"){
    aa <- rpois(obs, arg$lambda)
  }else if(distribuicao == "normal"){
    aa <- rnorm(obs, mean = arg$mu, sd = sqrt(arg$sigma2))
  }
  return(aa)
}
f(list(distribution = "poisson", lambda = 2.0, obs = 20))
f(list(distribution = "normal", mu = 1.2, sigma2 = 1.0, obs = 25))
f(list(distribution = "bernoulli", p = 0.3, obs = 30))

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
