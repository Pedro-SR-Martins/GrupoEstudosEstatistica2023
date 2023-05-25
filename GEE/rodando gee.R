# primeiro instale o pacote pacman, ele facilita a instalação de novos pacotes e para carrega-los
# install.packages("pacman")
# o comentario ao lado dos pacotes indica o para que vamos usa-los

pacman::p_load(tidyverse, #manipulacao dos dados
               easystats, #apresentacao dos coeficientes
               car, # testes de pressupostos
               janitor, # descritivas
               geepack, # para rodar o gee
               emmeans) # post hoc e medias estimadas

# lendo o banco de dados do github
base <- readRDS(url("https://github.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/blob/main/bancos/long_data2.rds?raw=true", method="libcurl"))
# averiguando as variaveis e suas classes
glimpse(base)



# descritivas ----------


# pedindo descritivas das variaveis, para evitar carregar mais um pacote chamei a funcao do psych direto no codigo. isso eh possivel usando essa estrutura nome_pacote::
psych::describeBy(x = base$extroversao,
                  group = base$time)


# para ver a distribuicao da variavel dependente, usei um ggplot. tentei fazer o mais simples possivel
# usa a seguinte logica: funcao ggplot --> colocamos os dados e especificamos o eixo x. como queria ver o plot com cores diferentes para cada tempo, pedi para prencher (fill) com o tempo
# no comando geom_density, temos um grafico de densidade, relativamente mais bonitinho pra ver a distribuicao (que parece nao ser normal)
# o argumento alpha no geom_density coloca um pouco de transparencia, para que seja possivel ver as partes que estao sobrepostas
ggplot(data = base,
       aes(x = extroversao,
           fill = time))+
  geom_density(alpha = .5)

# quantidade de pessoas
# vamos usar essa funcao pra criar uma tabela e ver quantas pessoas tem em cada um dos tempos
tabyl(base,
      id,
      time)
# nem todas as pessoas tem todas as informacoes completas
# N total sem missing: 17 (contei no olho)
# o id de numero 10 nao existe!


# grafico de trajetorias individuais


ggplot(data = base,
       aes(y = extroversao,
           x = time,
           # color = id,
           group = id
           ))+
  geom_line()


# rodando o modelo -----------

# a logica para construir o modelo eh bem similar a de outros
# usamos a funcao geeglm, escrevemos a relacao entre vd e vi's
# determinamos nosso banco de dados e a  distribuicao
# importante: aqui tambem eh necessario indicar qual a matriz de covariancia dos dados
# as possiveis sao: '"independence"', '"exchangeable"', '"ar1"', '"unstructured"' and '"userdefined"'
# eh preciso, ainda, indicar a variavel de identificacao!


fit1 <- geeglm(extroversao ~ time + satisfacao + depressao_z,
               data = base,
               id = id,
               family = gaussian("identity"),
               corstr = "unstructured")

# para podermos comparar o modelo, usamos a funcao QIC, lembrando: valores a serem COMPARADOS, sozinhos nao significam nada e quanto menor melhor
QIC(fit1)
# apresentacao dos coeficientes, como pedimos link identity, lemos como um modelo de regressao linear
summary(fit1)
jtools::summ(fit1, digits = 3)
# qqplot dos residuos usando o pacote car 
qqPlot(fit1$residuals, col.lines = "red")


# tentando outra distribuicao --------

fit2 <- geeglm(extroversao ~ time + satisfacao + depressao_z,
               data = base,
               id = id,
               family = Gamma("identity"),
               corstr = "ar1")


QIC(fit2)
summary(fit2)

# Y_bar = mean(fit2$y, na.rm = T)
# rsquare_gee <- 1-(sum(fit2$weights * (fit2$y - fit2$fitted.values)^2, na.rm = T)/sum(fit2$weights*(fit2$y - Y_bar)^2, na.rm = T))
# https://pubmed.ncbi.nlm.nih.gov/10814976/


# aqui o comando anova (tudo minusculo) apresenta os efeitos principais!
anova(fit2)
qqPlot(fit2$residuals, col.lines = "red")
hist(fit2$residuals)
shapiro.test(fit2$residuals) #Shapiro-Wilk test


# post hoc? --------

# o tempo era nossa unica VI categorica e nao foi significativa, mas poderiamos querer fazer um posthoc e explorar todas as comparacoes possiveis entre grupos

# como fazer? 
# um dos jeitos mais legais eh usar as medias estimadas do modelo
# para isso, vamos usar o pacote emmeans

# a ideia aqui eh a seguinte, a funcao emmeans precisa de dois argumentos principais, resultado do seu modelo e a VI que voce quer que ele apresente as medias estimadas
means_time <- emmeans(fit2, "time") 
means_time
# por se tratar de modelos generalizados, em alguns casos pode ser necessario transformar as medias estimadas de volta para a metrica original dos dados, o comando regrid faz isso
means_time <- regrid(means_time)

# para fazer o post hoc, basta usar essa combinacao de duas funcoes, pairs(contrast(medias))
# ainda vamos corrigir para multiplas comparacoes com bonferrroni
pairs(contrast(means_time), 
      adjust = "bonferroni")
# como esperado, nada eh significativo


emmeans(fit2, ~1)



# bonus 1: tamanho de efeito---------
# usamos o pacote fitdistrplus para explorar melhor as propriedades da variavel como uma distribuicao especifica
# veja mais em: https://www.r-bloggers.com/2022/09/gamma-distribution-in-r/


library(fitdistrplus)
fit <- fitdist(base$extroversao,
               distr = "gamma",method = "mle")

summary(fit)
plot(fit)

# tendo os parametros da distribuicao, podemos calcular a media e desvio padrao (lembrando que eh a raiz quadrada da variancia)
# veja: https://en.wikipedia.org/wiki/Gamma_distribution#Mean_and_variance
28.63/1.62
# dp 
desvio <- sqrt(28.63/(1.62^2))
desvio

psych::describe(x = base$extroversao)
# legal, uma vez que temos esses valores, podemos usa-los para calcular um d de cohen
# apenas como exemplo, vamos considerar o dp calculado em cima, que eh da amostra como um todo
# usando a funcao eff_size do pacote emmeans, conseguimos calcular os efeitos para todas as comparacoes :)
eff_size(contrast(means_time),
         sigma = desvio,
         edf = fit2$df.residual)

