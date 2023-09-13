# Crie uma função emv_df que recebe uma lista de amostras na estrutura da saída da função list_rpois da
# parte anterior, e retorne um data.frame com as seguintes colunas:
#   • replica: O índice da réplica (amostra) na lista.
# • emv: O estimador de máxima verossimilhança para a réplica de simulação com aquele índice.
# Não se esqueça de adicionar tags de @importFrom e modificar o arquivo DESCRIPTION caso decida usar funções
# de outros pacotes.


#' @importFrom dplyr transmute
#' @importFrom dplyr %>%


emv_df <- function(amostras){
  amostras_df <- do.call(rbind, amostras) %>% as.data.frame()
  amostras_df %>% transmute(replica = 1:nrow(amostras_df),
                            emv = rowMeans(amostras_df))
}
