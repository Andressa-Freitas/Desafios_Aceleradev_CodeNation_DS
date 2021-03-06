# Desafio da Semana 2 Codenation
setwd("C:/Users/Andressa/ACELERADEV_CODENATION/Desafios_Aceleradev_CodeNation_DS")


#Pacotes
library(data.table)
library(tidyverse)
library(dplyr)


# Carregar arquivo
bfriday <- fread("black_friday.csv", sep = ",", head = T, encoding = 'UTF-8')


# Verificar as primeiras 6 linhas do dataframe
head(bfriday)


# 1 N� de linhas e colunas: 537577 linhas/obs e 12 colunas/vari�veis.
dim(bfriday)


# 2 H� quantas mulheres com idade entre 26 e 35 anos no dataset? 49348 mulheres
Fem <- sum(bfriday$Gender == "F" & bfriday$Age == "26-35"); Fem 
PorcFem <- Fem/length(bfriday$Gender); PorcFem # H� 0.2459127 de mulheres


# 3 Quantidade de usu�rios �nicos no dataset: 5891 usu�rios �nicos.
QtdUsuaUnic <- length(unique(bfriday$User_ID)); QtdUsuaUnic


# 4 Quantos tipos de dados diferentes existem no dataset? Responda como um �nico escalar.
str(bfriday)


# 5 Qual porcentagem dos registros possui ao menos um valor null (`None`, `??aN` etc)? Responda como um �nico escalar entre 0 e 1.
n=nrow(bfriday)
round(colSums(is.na(bfriday))*100/n, 2)


# 6 Quantos valores null existem na vari�vel (coluna) com o maior n�mero de null? Responda como um �nico escalar.
sum(is.na(bfriday$Product_Category_3))


# 7 Qual o valor mais frequente (sem contar nulls) em `Product_Category_3`? Responda como um �nico escalar.
#N�o consegui! Se conseguir me avise!


# Qual a nova m�dia da vari�vel (coluna) `Purchase` ap�s sua normaliza��o? Responda como um �nico escalar."
a = bfriday$Purchase
Purchase = bfriday$Purchase
Normalizando = (Purchase - min(Purchase))/ (max(Purchase)-min(Purchase))
mediaNova = mean(Normalizando) # nova m�dia ap�s normalizar 0.3847939


# Quantas ocorr�ncias entre -1 e 1 inclusive existem da vari�el `Purchase` ap�s sua padroniza��o? Responda como um �nico escalar."
media = mean(Purchase)
desvio = sd(Purchase)
Padron = (Purchase - media)/desvio
sum(between(Padron, -1, 1))


# Podemos afirmar que se uma observa��o � null em `Product_Category_2` ela tamb�m o � em `Product_Category_3`? Responda com um bool (`True`, `False`).
if((is.na(bfriday$Product_Category_2)) == (is.na(bfriday$Product_Category_3))) {
  print("TRUE")
} else {
  print("FALSE")
}
