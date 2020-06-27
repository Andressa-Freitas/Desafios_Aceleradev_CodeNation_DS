# Desafio da Semana 2 Codenation
setwd("C:/Users/Andressa/codenation/data-science-0")


#Pacotes
library(data.table)
library(tidyverse)
library(dplyr)


# Carregar arquivo
bfriday <- fread("black_friday.csv", sep = ",", head = T, encoding = 'UTF-8')


# Verificar as primeiras 6 linhas do dataframe
head(bfriday)


# 1 N° de linhas e colunas: 537577 linhas/obs e 12 colunas/variáveis.
dim(bfriday)


# 2 Há quantas mulheres com idade entre 26 e 35 anos no dataset? 49348 mulheres
Fem <- sum(bfriday$Gender == "F" & bfriday$Age == "26-35"); Fem 
PorcFem <- Fem/length(bfriday$Gender); PorcFem # Há 0.2459127 de mulheres


# 3 Quantidade de usuários únicos no dataset: 5891 usuários únicos.
QtdUsuaUnic <- length(unique(bfriday$User_ID)); QtdUsuaUnic


# 4 Quantos tipos de dados diferentes existem no dataset? Responda como um único escalar.
str(bfriday)


# 5 Qual porcentagem dos registros possui ao menos um valor null (`None`, `??aN` etc)? Responda como um único escalar entre 0 e 1.
n=nrow(bfriday)
round(colSums(is.na(bfriday))*100/n, 2)


# 6 Quantos valores null existem na variável (coluna) com o maior número de null? Responda como um único escalar.
sum(is.na(bfriday$Product_Category_3))


# 7 Qual o valor mais frequente (sem contar nulls) em `Product_Category_3`? Responda como um único escalar.
#Não consegui! Se conseguir me avise!


# Qual a nova média da variável (coluna) `Purchase` após sua normalização? Responda como um único escalar."
a = bfriday$Purchase
Purchase = bfriday$Purchase
Normalizando = (Purchase - min(Purchase))/ (max(Purchase)-min(Purchase))
mediaNova = mean(Normalizando) # nova média após normalizar 0.3847939


# Quantas ocorrências entre -1 e 1 inclusive existem da variáel `Purchase` após sua padronização? Responda como um único escalar."
media = mean(Purchase)
desvio = sd(Purchase)
Padron = (Purchase - media)/desvio
sum(between(Padron, -1, 1))


# Podemos afirmar que se uma observação é null em `Product_Category_2` ela também o é em `Product_Category_3`? Responda com um bool (`True`, `False`).
if((is.na(bfriday$Product_Category_2)) == (is.na(bfriday$Product_Category_3))) {
  print("TRUE")
} else {
  print("FALSE")
}
