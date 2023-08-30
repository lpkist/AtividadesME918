############## PRODUTOS DE DADOS ############
# Considere um vetor numeric x e um vetor numeric de tamanho y
# e os operadores binários como +, -, * e /, qual o comportamento
# de x + y quando:
### x tem 4 elementos, y tem 1 elemento?
x <- 1:4
y <- c(5)
x+y
x-y
x*y
x/y

### x tem 4 elementos, y tem 2 elementos?
y <- c(5,6)
x+y # repete o y pra fechar
x-y
x*y
x/y

### x tem 4 elementos, y tem 3 elementos?
y <- c(5,6,7)
x+y # soma, mas dá erro (também recicla)
x-y
x*y
x/y

### x tem 4 elementos, y tem 4 elementos?
y <- 5:8
x+y 
x-y
x*y
x/y


# Explique o comportamento geral das operações aritméticas
# conforme os tamanhos dos vetores.
## Se for um escalar, cada elemento é operado separadamente com o escalar.
## Se a dimensão for a mesma, é elemento a elemento.
## Caso contrário, ele recicla o vetor menor até completar. Se a dimensão
## for múltipla não dá erro, se não, dá um warning


# Qual o resultado da multiplicação entre um vetor do tipo logical
# e um do tipo numeric? Dê um exemplo prático onde essa multiplicação
# pode ser útil.
logic <- c(T, T, F, F)
logic*x
## é zero onde é false e o valor onde é true. Aplicação: cálculo da 
## soma de uma coluna com restrição em outra, como:

logico <- iris$Species == "virginica" # logico é um vetor lógico
sum(logico*iris$Sepal.Length)/sum(logico)
mean(iris[logico, "Sepal.Length"])


# Entenda as funções abaixo e proponha nomes para cada uma delas.
library(stringr)
f1 <- function(string, prefix) {
  str_sub(string, # pega uma string
          1, # na posição 1
          str_length(prefix) # até o comprimento do prefixo
          ) == prefix # verifica se é o prefixo
}
f1("carro", "car")

verifica_prefixo <- function(string, prefix) {
  str_sub(string, # pega uma string
          1, # na posição 1
          str_length(prefix) # até o comprimento do prefixo
  ) == prefix # verifica se é o prefixo
}

f3 <- function(x, y) {
  rep(y, # repete o valor y
      length.out = length(x)) # cria um vetor do tamanho de x apenas com y
  #basicamente recicla y até ter length(x)
}
f3(1:10, 1:3)

recicla_y <- function(x, y) {
  rep(y, # repete o valor y
      length.out = length(x)) # cria um vetor do tamanho de x apenas com y
  #basicamente recicla y até ter length(x)
}


# Reescreva a função rescale01 de forma que valores numéricos 
# infinitos, -Inf e Inf, sejam desconsiderados na obtenção dos
# limites e os valores sejam mapeados para 0 e 1, respectivamente.

### original 
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(c(-Inf, 10))

rescale01 <- function(x){
  inf_pos <- x == Inf
  inf_neg <- x == -Inf
  fin <- 1:length(x) - inf_pos-inf_neg
  rng <- range(x[fin], na.rm = TRUE)
  x[fin] <- (x[fin] - rng[1]) / (rng[2] - rng[1])
  x[inf_pos] <- 1
  x[inf_neg] <- 0 
  return(x)
}
rescale01(c(-Inf, 10, Inf, 15, 1, -20))  


# Escreva uma função que receba um data.frame e reescale cada
# coluna numérica para o intervalo [0,1].
rescale_df <- function(df){
  for(coluna in colnames(df)){
    if(is.numeric(df[[coluna]])){
      df[[coluna]] <- rescale01(df[[coluna]])
    }
  }
  return(df)
  
}

rescale_df(iris)

# Escreva uma função que reescale um vetor numérico para o
# intervalo [0,c], onde c é um argumento da função. Essa função
# deve, necessariamente, reaproveitar a rescale01.
rescale0c <- function(x,c){
  x <- rescale01(x)
  return(c*x)
}
rescale0c(iris$Sepal.Length, 5)

# Qual a diferença entre copas[1] e copas[[1]]?
list(
  "brazil" = list(
    "titulos" = 5,
    "ultimo_titulo" = 2002,
    "capital" = "Brasilia"
  ),
  "argentina" = list(
    "titulos" = 3,
    "ultimo_titulo" = 2022,
    "capital" = "Buenos Aires"
  ),
  "australia" = list(
    "titulos" = 0,
    "capital" = "Canberra"
  )
) -> copas
copas[1] # todas as informações do brasil, com nome
str(copas[1]) # precisa acessar brazil antes de extrair as infos
copas[1]$brazil$titulos
copas[[1]] # todas as informações do primeiro elemento
str(copas[[1]]) # brazil já está acessado
copas[[1]]$titulos

# Qual a diferença entre pops[1] e pops[[1]]?
list(
  "brazil" = 214.3*10^6,
  "argentina" = 45.81*10^6,
  "australia" = 25.69*10^6
) -> pops
pops[1] # infos sobre o nome do primeiro elemento
pops[[1]] # pop do 1o elemento
# Qual a diferença entre acessar itens da lista via [[ ou [? E por meio
# do nome utilizando $?
#### Acessar por $ necessita inserir o nome do elemento e vai retornar 
#### o valor nesse objeto. [[]] é análogo a $, mas aceita nome ou índice
#### já [] retorna as informações e o nome do primeiro elemento, sem acessá-lo

# Retire os nomes dos países na lista do exemplo. Quais diferenças
# observamos no comportamento das funções da família map ao retirar os nomes?
list(
  list(
    "titulos" = 5,
    "ultimo_titulo" = 2002,
    "capital" = "Brasilia"
  ),
  list(
    "titulos" = 3,
    "ultimo_titulo" = 2022,
    "capital" = "Buenos Aires"
  ),
  list(
    "titulos" = 0,
    "capital" = "Canberra"
  )
) -> copas2
library(purrr)
copas2
list(
  214.3*10^6,
  45.81*10^6,
  25.69*10^6
) -> pops2
map(pops, mean) 
map(pops2, mean) # não tem mais o nome do elemento e é necessário acessar via [[]]

# Entenda o uso das funções da família walk do purrr e dê um exemplo de função
# para o qual esse funcional pode ser útil. Que tipo de função corresponde a esse uso?
walk(pops, function(x) print(x))
walk(copas, function(x) print(x))
### é útil para funções com efeito
### uma aplicação é iterar em uma lista de tabelas e salvar cada uma como .csv
### separadamente