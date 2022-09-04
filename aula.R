## Estudos Avançados de Metodologia de Pesquisa (PPGCP/UFPE) ##
## Professor: Hugo Medeiros ##
## Aluno: Rodrigo Lins ##

## Pacotes e opções ####

library('pacman')
library("eeptools")
library("caret")
library("dplyr")
library("readxl")
library("rjson")
library("XML")
library("microbenchmark")


## T1 - INTRODUCAO AO R/RSTUDIO ####

## Criando objeto simples ##
vetor <- c(1,2,3,5,7)
str(vetor)

## Criando objeto complexo ##
reg1 <- lm(mpg~ ., mtcars)
reg1
str(reg1)

## T1 - PROGRAMACAO BASICA COM R ####

## Tipos de objetos

## Criando dataframe ##
## Vetor com nome dos paises
nomePaises <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Equador", "Paraguai", "Peru",
                "Uruguai", "Venezuela")

## Vetor com data de democratizacao
anoPaises <- as.Date(c("1983-01-01", "1982-01-01", "1985-01-01", "1990-01-01", "1958-01-01", "2002-01-01",
                       "1989-01-01", "2001-01-01", "1985-01-01", "1959-01-01"))

## Vetor com idade do regime
idadePais <- round( age_calc(anoPaises, units = 'years'))

## Dataframe com base nos vetores
listaPaises <- data.frame(
  nome = nomePaises,
  nascimentoDem = anoPaises,
  idadeDem = idadePais
)

str(listaPaises)

write.csv(listaPaises, "/Users/rodrigolins/Google Drive/UFPE/Estudos Avancados/aula/listaPaises.csv", row.names = T)

## Simulacoes e sequencias

## Atribuir e chamar setseed
tarefaSemente <- addTaskCallback(function(...) {set.seed(123); TRUE})
tarefaSemente

## Distribuicao normal
distNormal <- rnorm(100)

## Distribuicao binomial
distBinomial <- rbinom(100, 1, 0.7)

## Repeticoes
classeSimulacao <- c(rep("Democracia", length(distBinomial)/2), rep("Ditadura", length(distBinomial)/2))

## Sequencias
indexSimulacao <- seq(1, length(distNormal))

## Remover tarefa setseed
removeTaskCallback(tarefaSemente)

## Amostragem e bootstrapping

## Bootstrapping
set.seed(2022)

## Replicar a amostra 10x

## EXEMPLO DO PROFESSOR ##

bootsDistNormal10 <- replicate(10, sample(distNormal, 10, replace = TRUE))
bootsDistNormal10

mediaBootsNormal10 <- replicate(10, mean(sample(distNormal, 10, replace = TRUE))) ## 10 amostras de 10 casos
mediaBootsNormal50 <- replicate(50, mean(sample(distNormal, 10, replace = TRUE))) ## 50 amostras de 10 casos
mediaBootsNormal100 <- replicate(100, mean(sample(distNormal, 10, replace = TRUE))) ## 100 amostras de 10 casos

mean(mediaBootsNormal10)
mean(mediaBootsNormal50)
mean(mediaBootsNormal100)
mean(distNormal)

## FEITO POR MIM, A PARTE DA BASE IRIS ##

irisBoots <- replicate(10, sample(iris$Sepal.Length, 10, replace = TRUE))
irisBoots

mediaIris10 <- replicate(10, mean(sample(iris$Sepal.Length, 10, replace = TRUE))) ## 10 amostras de 10 casos
mediaIris50 <- replicate(50, mean(sample(iris$Sepal.Length, 10, replace = TRUE))) ## 50 amostras de 10 casos
mediaIris1100 <- replicate(100, mean(sample(iris$Sepal.Length, 10, replace = TRUE))) ## 100 amostras de 10 casos

mean(mediaIris10)
mean(mediaIris50)
mean(mediaIris1100)
mean(iris$Sepal.Length)

## Calculando

binominalnegSimulacao <- rnbinom(300, mu = 3, size = 10)
poissonSimulacao <- rpois(300,3)

hist(binominalnegSimulacao)
hist(poissonSimulacao)

binominalnegSimulacao + poissonSimulacao

poissonSimulacao + 100
poissonSimulacaoˆ2
poissonSimulacao*binominalnegSimulacao

round(distNormal, 0)
ceiling(distNormal)
floor(distNormal)

distNormal %% poissonSimulacao

mean(poissonSimulacao)
median(poissonSimulacao)
sd(poissonSimulacao)
var(poissonSimulacao)

poissonSimulacaoCentral <- poissonSimulacao - mean(poissonSimulacao)
hist(poissonSimulacaoCentral)
hist(binominalnegSimulacao)

## Normalizando variável de banco criado por mim
dataframe <- read_csv("dataframe.csv")
dataframe <- subset(dataframe, select = -X1)

testeNormal <- dataframe$v2x_polyarchy - mean(dataframe$v2x_polyarchy)
hist(dataframe$v2x_polyarchy)
hist(testeNormal)

## Index e Operadores Lógicos

##Index
poissonSimulacao[1]
poissonSimulacao[c(1:10, 15)]

matrix1[1, ] #linha
matrix[ 0,1] #colina
matrix[1,1] #linha e coluna

iris$Species
iris[ ,5]
iris[1:10, 2:5]
iris[ ,'Species']
iris[ ,'Species', drop = FALSE]
iris[ , -5] # retorna todas as colunas, exceto a 5

reg1$coefficients
reg1$coefficients[1]
reg1[['coefficients']][1]
reg1[[1]][1]

##Operadores lógicos
a <- 5
b <- 7
c <- 5

a < b
a <= b
a > b
a >= b
a == b
a != b
a %in% c(b, c)
a == c & a < b
a != c | a > b
xor(a != c, a < b)
!a != c
any(a != c, a < c, a == c)
all(a != c, a < c, a == c)

##Operadores lógicos na prática
iris$Sepal.Length <= 0
iris$Sepal.Length >= 0 & iris$Sepal.Width <= 0.2

which(iris$Sepal.Length <= 5)
match(iris$Species, 'setosa')

##Usando indexacao
listaPaises[1, ]
listaPaises[ ,1]
listaPaises[1, 1]

listaPaises[3,3] < listaPaises[9, 3]
listaPaises[3,3] == listaPaises[9, 3]
listaPaises[3,3] != listaPaises[5, 3]

'Brazil' %in% dataframe$country
match(dataframe$country, 'Brazil')

## Estruturas de controle

x <- runif(1, 0, 5)
x

if(x > 3) {
  y <- 5
} else{
  y <- 0
}
y

irisCopia$SpeciesDummy <- ifelse(irisCopia$Species == 'setosa', 1, 0)

par(mfrow = c(2,2))

for (i in 1:4) {
  x <- iris [ , i]
  hist(x,
       main = paste("variável", i, names(iris)[i]),
       xlab = "Valores da variável",
       xlim = c(0, 10))
}

lapply(iris[, 1:4], hist)

##Controle condicional ou controle de repetição

listaPaises$Brasil <- ifelse(listaPaises$nome == 'Brasil', 1, 0)

lapply(listaPaises[,3:4], hist)

## Funcoes

f <- function() {
  cat("Hello, world!\n")
}
f()

# função com estrutura de repetição
f <- function(nro) {
  for(i in 1:nro) {
    cat("Hello, world!\n")
  }
}
f(3)

# função com estrutura condicional e de repetição
f <- function(nro) {
  if(nro < 100) {
    for(i in 1:nro) {
      cat("Hello, world!\n")
    }
  } else {
    cat("Tá demais")
  }
}
f(99)
f(100)

centralizacao <- function(x) {
  x <- x - mean(x)
  return(x)
}

centralizacao(irisCopia$Sepal.Length)
centroTeste <- centralizacao(irisCopia$Sepal.Length)

# Funcoes de Repeticao - Familia Apply

apply(iris[ , -5], 2, mean)

lapply(iris[, -5], mean)

sapply(iris[, -5], mean)

par(mfrow = c(2,2))

sapply(iris[, 1:4], hist)
mapply(hist, iris[ , 1:4], MoreArgs=list(main='Histograma', xlab = 'Valores', ylab = 'Frequência'))

for (i in 1:4) { # cria o loop, que deve ir de 1 a 4
  x <- iris[ , i] # atribui as colunas da base de dados a uma variável temporária
  hist(x,
       main = names(iris)[i], # atribui o nome ao gráfico de forma incremental, passando coluna por coluna
       xlab = "Valores da Variável", # rótulo eixo x
       ylab = 'Frequência', # rótulo eixo y
       xlim = c(min(iris[, i]), max(iris[, i]))) # limites do eixo x
}

## Funcao de repeticao

for (i in 3) {
  x <- listaPaises[, i]
  hist(x,
       main = names(listaPaises)[i],
       xlab = "Valores da Variável",
       ylab = 'Frequência',
       xlim = c(min(listaPaises[, i]), max(listaPaises[, i])))
}

## T2 - SISTEMAS COMPLEXOS ####

##
## T3 - EXTRACAO, TRANSFORMACAO E LEITURA - ETL ####

## ETL na prática ##

## Importando dados de sinistros da PCR ##
sinistrosRecife2018Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2485590a-3b35-4ad0-b955-8dfc36b61021/download/acidentes_2018.csv',
                                   sep = ';', encoding = 'UTF-8') ## 2018 ##

sinistrosRecife2019Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/3531bafe-d47d-415e-b154-a881081ac76c/download/acidentes-2019.csv',
                                   sep = ';', encoding = 'UTF-8') ## 2019 ##

sinistrosRecife2020Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/fc1c8460-0406-4fff-b51a-e79205d1f1ab/download/acidentes_2020-novo.csv',
                                    sep = ';', encoding = 'UTF-8') ## 2020 ##

sinistrosRecife2021Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2caa8f41-ccd9-4ea5-906d-f66017d6e107/download/acidentes2021.csv',
                                    sep = ';', encoding = 'UTF-8') ## 2021 (ate julho) ##

## Concatenando ##

## Transformando nome da coluna 'data' para 'DATA' que possam ser mergidas sem a criacao de uma nova coluna ##
colnames(sinistrosRecife2020Raw)[which(names(sinistrosRecife2020Raw) == "data")] <- "DATA"

sinistrosRecifetotal <- bind_rows(sinistrosRecife2018Raw, sinistrosRecife2019Raw, sinistrosRecife2020Raw)

## Ajustando o formato de data ##
sinistrosRecifetotal$DATA <- as.Date(sinistrosRecifetotal$DATA, format = "%Y-%m-%d")

## Alterando variavel categorica para fator ##
sinistrosRecifetotal$tipo <- as.factor(sinistrosRecifetotal$tipo) ## Tipo do sinistro ##
sinistrosRecifetotal$situacao <- as.factor(sinistrosRecifetotal$situacao) ## Situacao da chamada ##

# Funçao para substituir NA por 0
naZero <- function(x) {
  x <- ifelse(is.na(x), 0, x)
}

## Extracao ##

ls()

for (itm in ls()) { 
  print(formatC(c(itm, object.size(get(itm))), 
                format="d", 
                width=30), 
        quote=F)
}

rm(list = ls()[!ls() %in% c("sinistrosRecifetotal", "naZero")])

## Principais fontes para extracao ##

## Exemplo de extracao .csv (Relacao das pessoas vacinadas - Covid-19)
vacinados_covid <- read.csv2('http://dados.recife.pe.gov.br/dataset/f381d9ea-4839-44a6-b4fe-788239189900/resource/966e9c4c-df45-40d7-9c58-2f13c61a6d28/download/vacinados.csv',
                                                       sep = ';', encoding = 'UTF-8') ## Vacinados Covid-19 ##

## Exemplo de extracao JSON (Malha cicloviaria do Recife - metadados das rotas operacionais)

malha_cicloviaria <- fromJSON(file = "http://dados.recife.pe.gov.br/dataset/667cb4cf-fc93-4687-bb8f-431550eeb2db/resource/f0fdaad1-080e-40d0-a712-c152509d6f99/download/metadados-malhaoperacionalrecife.json")
malha_cicloviaria <- as.data.frame(malha_cicloviaria)

## Exemplo de extracao de XML (WSU cursos)

WSU_xml <- xmlToDataFrame("http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/courses/wsu.xml")


## Leitura ##

## Exportando o banco de dados nas versoes RDS e CSV 
saveRDS(sinistrosRecifetotal, "bases_tratadas/sinistrosRecife.rds")
write.csv2(sinistrosRecifetotal, "bases_tratadas/sinistrosRecife.csv")

## Carregando as bases de dados
sinistrosRecife <- readRDS('bases_tratadas/sinistrosRecife.rds')
sinistrosRecife <- read.csv2('bases_tratadas/sinistrosRecife.csv', sep = ';')

## Comparando processos
microbenchmark(a <- saveRDS(sinistrosRecifetotal, "bases_tratadas/sinistrosRecife.rds"),
               b <- write.csv2(sinistrosRecifetotal, "bases_tratadas/sinistrosRecife.csv"),
               times = 10L)

microbenchmark(a <- readRDS('bases_tratadas/sinistrosRecife.rds'),
               b <- read.csv2('bases_tratadas/sinistrosRecife.csv', sep = ';'),
               times = 10L)

## Tipos de dados no R ##

## Estrutura de fatores
country <- c(1, 2, 3, 4, 5, 1, 2, 3)
recode <- c(Argentina = 1, Bolivia = 2, Brazil = 3, Chile = 4, Colombia = 5)

(country <- factor(country, levels = recode, labels = names(recode)))

