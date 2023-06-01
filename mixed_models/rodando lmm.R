
# carregando pacotes 
pacman::p_load(tidyverse, #manipulacao dos dados
               easystats, #apresentacao dos coeficientes
               car, # testes de pressupostos
               janitor, # descritivas
               emmeans) # post hoc e medias estimadas
# carregando os pacotes para os testes propriamente ditos
# aqui carreguei desse jeito diferente porque esses dois pacotes tem um conflito nas funcoes
# dessa forma, é importante que sejam carregados nessa ordem
# o pacote lmerTest apresenta de forma automatica os valores de p dos coeficientes
library(lme4)
library(lmerTest)
# reparem que esse conflito aparece no console com esse aviso "The following object is masked from ‘package:lme4’: lmer"

# carregando o banco de dados
base <- readRDS(url("https://github.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/raw/main/bancos/baseRT.rds", method="libcurl"))

# vendo o banco de dados
glimpse(base)

# descritivas gerais
psych::describeBy(base$RT,
                  base$category)

tabyl(base,
      subj_id,
      item_id)
options(max.print = 10000) #qualquer valor grande para que ele mostre todo o output

tabyl(base,
      item_id,
      category)

# regressao normal (aqui praticamente um teste t ou anova)

resultadoregresao <- lm(RT ~ category, data = base)
summary(resultadoregresao)
# LEMBRETE: nao faca isso em casa. eh apenas um exemplo



# modelo multinivel ----------------
# primeiro um modelo apenas com intercepto aleatorio para o sujeito
# a logica do modelo eh bem parecida com a de regressao
# DV ~ IVS
# acrescentamos, agora, os efeitos aleatorios entre parenteses!
# a notacao eh basicamente assim: 1|variavel indica que essa variavel eh o intercepto aleatorio
# se quisessemos acrescentar um efeito aleatorio para o slope seria assim:
# (1 + slope|intercepto)
# exemplo, para um efeito aleatorio no tempo seria algo como:
# (1 + time|id)


fit0 <- lmer(RT ~ category + (1|subj_id), # relacao VD com VIS e efeito aleatorio
             data = base, # banco de dados
             REML = FALSE, # permite que os modelos possam ser comparados
             control = lmerControl(optimizer = c("bobyqa")) # estabilizador (padrao no jamovi/jasp)
             )
summary(fit0)

# dica de analise: nao use direto o pacote jtools para apresentar os coeficientes
# as vezes, o modelo apresenta algum aviso de convergencia que nao aparece com a funcao summ
# esses avisos aparecem apenas na funcao summary
# como nao tivemos aviso nenhum, vamos continuar com a parte abaixo
library(jtools)
summ(fit0)
# importante, aqui ainda temos a possibilidade de ver outras coisas do modelo, como BIC, AIC, uma medida de pseudo R ao quadrado
parameters(fit0)
performance(fit0)
standardize_parameters(fit0, method = "smart")
# method = "smart" "...As a results, the coefficients for effects of factors are similar to a Glass' delta."
# o que significa isso de desvio padrao e variancia do intercepto na pratica?
coef(fit0)

# podemos apresentar os resultados na mesma logica de anova (i.e., efeitos principais)?
anova(fit0)
# SIM!



# modelo com slopes aleatorios ------
fit1 <- lmer(RT ~ category + (1 + category|subj_id),
             data = base,
             REML = FALSE, 
             control = lmerControl(optimizer = c("bobyqa")))
#apresentacao dos coeficientes...
summary(fit1)
summ(fit1)
# comparando os dois modelos
anova(fit0,fit1)

coef(fit1)
anova(fit1)


# posthoc -----

# ate aqui tudo certo, posthoc feito como nas outras analises
meanscategories <- emmeans(fit1, "category") 
meanscategories
pairs(contrast(meanscategories), 
      adjust = "bonferroni")


# para fazer a parte de tamanho de efeito, vamos fazer um pouco diferente!
# no manual da funcao, ha uma exposicao sobre como fazer isso usando modelos multinivel
# o importante eh: vamos usar a funcao VarCorr para descobrir as variancias estimadas do modelo
# vamos usar todas as fontes de variancia (no caso, o dp elevado ao quadrado) e depois tirando a raiz quadrada
VarCorr(fit1)
dptotal <- sqrt((106.786^2) + (88.269^2) + (27.802^2))

eff_size(contrast(meanscategories),
         sigma = dptotal,
         edf = Inf)
standardize_parameters(fit1, method = "smart")
# adendos importantes --------
# aquela coisa toda sobre matriz de correlacao se aplica aqui tambem?
# SIM
# use esse pacote abaixo para utilizar matrizes diferentes
# a notacao eh um pouco diferente, mas fica bem simples de ver
# uma coisa a mais, pela minha experiencia atual, esse pacote pode ser um pouco mais interessante para dados longitudinais
# motivo: ele parece ter mais flexibilidade para fazer slopes aleatorios :D

# library(nlme)
# fit11 <- lme(fixed = RT ~ category ,
#              random = ~ 1 |subj_id,
#              data = base,
#              method = "ML",
#              correlation = corAR1())
# summary(fit11)


# para dados desse tipo com experimentos o mais correto seria controlar para o efeito aleatorio dos itens tambem
# justificativa: https://doi.org/10.1037/a0028347
# seria feito como apresentado abaixo:


# fit <- lmer(RT ~ category + (1|subj_id)+(1|item_id),
#              REML = FALSE,
#              control = lmerControl(optimizer = c("bobyqa")),
#              data = base)
# summary(fit)