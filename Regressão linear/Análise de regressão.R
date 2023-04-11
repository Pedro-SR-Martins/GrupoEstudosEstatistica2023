## Instalar e carregar os pacotes que serão utilizados
install.packages("psych")
install.packages("car")
install.packages("lm.beta")
install.packages("lmtest")
install.packages("tidyverse")
install.packages("MVN")
library(psych)
library(car)
library(lmtest)
library(lm.beta)
library(tidyverse)
library(MVN)

## Antes de iniciar a análise de regressão, é importante fazer o cálculo do tamanho amostral e definir qual será a variável dependente e a(s) independente(s)

## Abrir o banco de dados
banco <- read.csv('https://raw.githubusercontent.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/main/bancos/bancoreg.csv')

## Visualizar os dados
describe(banco)

## Investigação de outliers através da distância de Mahalanobis:
mvn(banco[,5:7], showOutliers = TRUE, multivariateOutlierMethod = "adj")

## Verficiar a relação linear entre a VD e a VI:
plot(banco$neuroticismo, banco$ansiedade)

## Construção do modelo:
fit <- lm(data = banco, ansiedade ~ neuroticismo)

## Análise gráfica:
par(mfrow=c(2,2))
plot(fit)

## Interpretação dos gráficos: https://data.library.virginia.edu/diagnostic-plots/
## Residuals vc Fitted:
    # Permite avaliar a linearidade e a homocedasticidade.
    # Quando a linha vermelha estiver aproximadamente horizontal, há uma relação horizontal.
    # Homocedasticidade (normalidade dos resíduos): a quantidade de pontos de cima da reta é próximo da quantidade de baixo.
## Normal Q-Q:
    # Se os resíduos apresentarem distribuição normal, os pontos devem seguir a linha pontilhada do gráfico.
## Scale-Location:
    # O gráfico mais adequado para ver a homocedasticidade. A linha vermelha deve estar aproximadamente horizontal e pontos dispersos próximos de um retângulo.
## Residuals vs Leverage
    # Verifica se existem resíduos que são outliers. Observamos se existem pontos de alavancagem. Pode existir valores discrepantes, desde que não sejam influentes.
    # Se tiver valores fora do intervalo -3 a 3, significa que são outliers. Observar casos que estão fora da linha vermelha pontilhada.

## Normalidade dos resíduos:
shapiro.test(fit$residuals)
    # Se valor de p for >0,05, a distribuição é normal.

## Outliers nos resíduos:
summary(rstandard(fit))
    # valores mínimo e máximo entre -3 e 3.

## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(fit)
    # valor da estatística próxima de 2 para indicar independência dos resíduos. Valor de p não significativo.

## Homocedasticidade (Breusch-Pagan):
bptest(fit)
    # se o valor de p é maior do que 0,05, indica que há homocedasticidade.

summary(fit)
betas <- lm.beta(fit)$standardized.coefficients
betas
library(easystats)
parameters(fit, standardize = "smart")
library(jtools)
summ(fit,
     digits = 3,
     scale = T,
     confint = T,
     vifs = F, # não funciona aqui por ser apenas 1 preditor
     part.corr = T)
