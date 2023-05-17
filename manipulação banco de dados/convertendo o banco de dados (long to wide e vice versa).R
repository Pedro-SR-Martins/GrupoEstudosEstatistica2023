
# Manipulacao dos bancos de dados

# vamos ver como trocar o banco de wide pra long e long pra wide

# lendo o banco de dados

base <- readRDS(url("https://github.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/blob/main/bancos/long_data.rds?raw=true", method="libcurl"))
base
library(tidyverse)
# convertendo do formato longo para formato largo 
# esse eh o mais facil


basewide <- base %>% 
  pivot_wider(id_cols = c(id,sexo),
              names_from = time,
              values_from = c(dep, ext))
basewide


# convertendo do formato wide para o formato longo
# esse eh um pouquinho mais confuso
# as colunas ta facil, quais sao minhas medidas repetidas
# agora, como isso vai ser convertido?
# names_to = c(".value", "time")
# aqui temos duas coisa, um ".value" indicando que vamos criar valores
# o ponto aqui indica que pode ser criada mais de uma coluna
# o time so indica que vamos criar uma variavel indexadora das medidas repetidas
# que vai ser chamar time, poderia ser qualquer outra coisa
# ultima coisa, como essas colunas vao ser transformadas? os dados vao ser diferenciados pelo _, entao temos o simbolo "\\_"
# isso ocorre porque as variaveis estao nomeadas como medida_t1
# se tivessemos o cenario de "medida.1" deveriamos editar para "\\."
# e assim sucessivamente...

base_long <- basewide %>% 
  pivot_longer(cols = c(dep_t1, dep_t2, dep_t3, dep_t4, ext_t1:ext_t4),
               names_to = c(".value", "time"),
               names_sep = "\\_") 
base_long


# e se nao tivesse separador? so o numero
# a linha abaixo remve os separadores para efeito de visualizacao

basewide1 <- basewide %>% rename_with(~str_remove_all(.x, "_t")) 
basewide1



# entao vamos separar usando um jeito mais complicado, vamos dividir pelo padroes dos nomes
# o que significa?(.*?)(\\d)
# a primeira parte nos parenteses significa qualquer padrao de texto, 
# a segunda parte nos parenteses indica digitos, indicando que isso e o separador
baselong1 <- basewide1 %>% 
  pivot_longer(cols  = c(dep1:dep4, ext1:ext4),
               names_to = c(".value", "time"),
               names_pattern = "(.*?)(\\d)",
               )
baselong1
# complicado e chatinho, vamos confessar que eh bem pouco intuitivo
# mais exemplos em:
# https://stringr.tidyverse.org/articles/regular-expressions.html
# https://dcl-wrangle.stanford.edu/pivot-advanced.html
# https://stackoverflow.com/questions/61570710/how-to-use-pivot-longer-to-reshape-from-wide-type-data-to-long-type-data-with-mu
# a verdade eh que nao tem solucao que se aplique a todos os casos D:


basewide %>% 
  pivot_longer(cols  = c(dep_t1:dep_t4, ext_t1:ext_t4),
               names_to = c(".value", "time"),
               names_pattern = "(.*?)(\\d)",
  )
