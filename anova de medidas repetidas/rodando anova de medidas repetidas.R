
# carregando pacotes
pacman::p_load(tidyverse, #manipulacao dos dados
               easystats, #apresentacao dos coeficientes
               car, # testes de pressupostos
               janitor, # descritivas
               afex) # para rodar a anova

# lendo o banco de dados do github
base <- readRDS(url("https://github.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/blob/main/bancos/long_data.rds?raw=true", method="libcurl"))


# descritivas
psych::describeBy(x = base$dep,
                  group = base$time)

# para ver a distribuicao da variavel dependente, usei um ggplot. tentei fazer o mais simples possivel
# usa a seguinte logica: funcao ggplot --> colocamos os dados e especificamos o eixo x. como queria ver o plot com cores diferentes para cada tempo, pedi para prencher (fill) com o tempo
# no comando geom_density, temos um grafico de densidade, relativamente mais bonitinho pra ver a distribuicao (que parece nao ser normal)
# o argumento alpha no geom_density coloca um pouco de transparencia, para que seja possivel ver as partes que estao sobrepostas
ggplot(data = base,
       aes(x = dep,
           fill = time))+
  geom_density(alpha = .5)


shapiro.test(base$dep)


# quantidade de pessoas
# vamos usar essa funcao pra criar uma tabela e ver quantas pessoas tem em cada um dos tempos
tabyl(base,
      id,
      time)
# funciona melhor para poucos sujeitos
# vemos que temos 350 pessoas
base %>% group_by(id) %>% count(time)

# rodando a anova ----------
fit1 <- aov_ez(id = "id",#variavel de identificacao
               dv = "dep",#variavel dependente
               within = "time",# variavel intra sujeitos (medida repetida)
               # between = "sexo", #possivel: variavel entre sujeitos (fixa no tempo)
               data = base, # banco de dados
               anova_table = list(correction = "none") # correcao esfericidade (GG, HF, ou none)
               )
# para especificar por formula, deveria ser assim, um pouco menos intituitivo
# fit2 <- aov_car(dep ~ Error(id/time), data = base)

# normalidade dos residuos
qqPlot(fit1$lm$residuals)
shapiro.test(fit1$lm$residuals)


# apresentando o modelo
summary(fit1)
fit1


# refazendo o modelo com a correcao Huynh–Feldt 
fit2 <- aov_ez(id = "id",
               dv = "dep",
               within = "time",
               # between = "sexo",
               data = base, 
               anova_table = list(correction = "HF")
               )

summary(fit2)
fit2


library(emmeans)
# a ideia aqui eh a seguinte, a funcao emmeans precisa de dois argumentos principais, resultado do seu modelo e a VI que voce quer que ele apresente as medias estimadas
means_time <- emmeans(fit2, "time") 


# para fazer o post hoc, basta usar essa combinacao de duas funcoes, pairs(contrast(medias))
# ainda vamos corrigir para multiplas comparacoes com bonferrroni
pairs(contrast(means_time), 
      adjust = "Bonferroni")


# para fazer o tamanho de efeito, vamos usar essa combinacao de funcoes abaixo
# eff_size(contrast(medias),
#          sigma = desvio-padrao,
#          edf = grau-liberdade-erro)
eff_size(contrast(means_time),
         sigma = sqrt(mean(sigma(fit1$lm)^2)),# como no manual da funcao
         edf = 349)


# alternativa para encontrar os tamanhos de efeito direto dos valores de t
t_to_d(t = c(-22.443,11.422,-36.542,
             36.133, -20.444, -76.284), 
       paired = TRUE,
       df_error  = 349)

# nota, aqui ha uma leve discordancia entre as duas formas. a primeira, usando a funcao eff_size eh a utilizada pelo jasp




# graficos do slide ------------------------

pacman::p_load(tidyverse, ggdensity)

data.frame(t1 = rnorm(500, mean = 500, sd = 40),
           t2 = rnorm(500, mean = 600, sd = 50),
           t3 = rnorm(500, mean = 700, sd = 100)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, y = name, fill = name, group = name))+
  ggridges::geom_density_ridges(scale = 1, alpha = .6,
                                color = "white")+
  ggridges::stat_density_ridges(quantile_lines = TRUE)+
  theme_classic()+ 
  theme(legend.position = "none", 
        axis.text.y = element_text(colour="black",size=11), 
        axis.text.x = element_text(colour="black",size=10))+
  labs(x=NULL, y = NULL)+
  scale_fill_manual(values = c("#ffad08","#bfbfbf","#73a5f5"))+
  # facet_wrap(test~., ncol = 3)+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="red", size =  1.4, linewidth = 1.4)
