
# primeiro instale o pacote pacman, ele facilita a instalação de novos pacotes e para carrega-los
# install.packages("pacman")
# o comentario ao lado dos pacotes indica o para que vamos usa-los
pacman::p_load(tidyverse, #manipulacao dos dados
               easystats, #apresentacao dos coeficientes
               car, # testes de pressupostos
               janitor, #descritvas
               jtools) # apresetacao dos resultados
# lendo o banco de dados do github
base <- readRDS(url("https://github.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/blob/main/bancos/bancolog.rds?raw=true", method="libcurl"))
# averiguando as variaveis e suas classes
glimpse(base)


# aqui usei uma estrutura de programacao relacionada ao tidyverse para transformar a variavel interessemat em factor.
# usei o verbo mutate e outras informacoes podem ser encontradas em https://r4ds.hadley.nz/
base <- base %>% 
  mutate(interessemat = factor(interessemat,
                               levels = c(0,1,2),
                               labels = c("Nenhum",
                                          "Moderado",
                                          "Muito")))
head(base)

# pedindo descritivas das variaveis, para evitar carregar mais um pacote chamei a funcao do psych direto no codigo. isso eh possivel usando essa estrutura nome_pacote::
psych::describe(base)


# para as variaveis categoricas, vamos usar uma funcao diferente:

tabyl(dat = base,
      var1 = interessemat,
      var2 = passoumat)



# finalmente rodando o modelo:
# usamos a funcao glm (Fitting Generalized Linear Models)
# ela serve para varios modelos generalizados, por isso precisamos especificar qual vamos fazer, no caso, regressao logistica. Fazemos isso no argumento de family, onde indicamos que temos uma VD binomial e link logit. Isso do link logit nao foi explicado no GE, por ser um pouco mais avancado. Quem tiver curiosidade, pode olhar https://www.linkedin.com/pulse/logit-vs-probit-models-satyadeep-behera/
fit1 <- glm(passoumat ~ sexo + estudo + 
              notaport + interessemat,
            data = base,
            family = binomial("logit"))

# avaliacao dos pressupostos
# para identificar a linearidade entre os preditores continuos e o logit usamos a funcao abaixo
boxTidwell(fit1$linear.predictors ~ estudo + notaport,
           data = base)
# valores significativos indicam que nao ha linearidade.essa funcao nao aceita os preditores categoricos juntos, o que parece estar prejudicando a avaliacao. quando rodamos um scatter plot, parece que **ha uma relacao linear**
plot(fit1$linear.predictors ~ base$notaport)

# caso essa relacao nao fosse linear visualmente, deveriamos acrescentar uma interacao entre a variavel independente e seu logaritmo, como mostrado abaixo
# as razoes para isso sao bem detalhadas no livro do andy field

fit2 <- glm(passoumat ~ sexo + estudo + 
              notaport + 
              (notaport * log(notaport))+
              + interessemat,
            data = base,
            family = binomial("logit"))

# independencia dos erros pode ser inspecionada visualmente com o codigo abaixo, como o grafico indica um padrao de correlacao proximo de zero, poderiamos aceitar que ha independencia
# lembrete, normalmente esse tipo de pressuposto pode ser garantido com a metodologia da coleta de dados, como coleta com amostra aleatoria e transversal
plot(residuals.glm(fit1, "deviance"))


# Ausencia de multicolinearidade

# valores de vif ate 5 pode ser considerados aceitaveis. Mas os valores poderiam ser melhores...

vif(fit1)


# outliers
# essa funcao do pacote easystats facilita o trabalho. Quem tiver curiosidade, pode ler o manual da funcao para ver quais sao os valores default considerados como outliers e como modifica-los
check_outliers(fit1)


# apresentacao dos coeficientes!!!
summary(fit1)
# aqui os resultados sao apresentados em log da chance, muito dificil de interpretar, para interpretar, vamos usar a funcao abaixo que apresenta mais resultados e os coeficientes em odds (exponencial do coeficiente)

summ(fit1, exp = T, confint = T, digits = 3)


# rapida interpretacao considerando as notas do slide.
# como temos uma variavel categorica de 3 niveis, vamos sempre interpretar os coeficientes com base na comparacao com o nivel de referencia. no caso, baixo interesse em matematica

# interpretacao do coeficiente de estudo (horas de estudo)
# a cada 1h de estudo, a pessoa tem 2.61 chance a mais de ser aprovada em matematica. OU em formato ~cientifico~, Horas de estudo estava positivamente relacionada a aprovacao em matematica, Odds-Ratio = 2.61 (95%IC 2.09 - 3.26) z = 8.41 p < 0.001.

# interpretacao do coeficiente de interesse em matematica

# Essa variavel foi significativa apenas na comparacao entre quem tem muito interesse e quem tem pouco interesse, entao vamos focar nessa comparacao
# Pessoas com muito interesse em matematica tem 4.40 de chance a mais de passar em matematica do que quem tem pouco interesse em matematica.
# OU: Pessoas com muito interesse em matematica tiveram maiores chances de passar, comparadas com quem tem pouco interesse Odds-Ratio = 4.40 (95%IC 1.57 - 12.33) z = 2.82 p = 0.005.


# para ter os efeitos principais, como no SPSS ou jamovi
# atencao, aqui eh a funcao Anova com A maiusculo, essa funcao eh do pacote car
Anova(fit1)
# a acuracia geral do modelo pode ser vista usando essa funcao do easystats
performance_pcp(fit1, method = "Gelman-Hill")
#method = "Gelman-Hill" -- > acima de .5 é predito como 1



# Visualizando o modelo com o pacote jtools


effect_plot(fit1, pred = notaport,
            plot.points = T,
            interval = T)


# Visualizando o modelo com o pacote jtools


effect_plot(fit1, pred = sexo,
            interval = T)
