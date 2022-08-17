## Estudos Avançados de Metodologia de Pesquisa (PPGCP/UFPE) ##
## Professor: Hugo Medeiros ##
## Aluno: Rodrigo Lins ##

library('pacman')
library("eeptools")
library("caret")

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
bootsDistNormal10 <- replicate(10, sample(distNormal, 10, replace = TRUE))
bootsDistNormal10

mediaBootsNormal10 <- replicate(10, mean(sample(distNormal, 10, replace = TRUE))) ## 10 amostras de 10 casos
mediaBootsNormal50 <- replicate(50, mean(sample(distNormal, 10, replace = TRUE))) ## 50 amostras de 10 casos
mediaBootsNormal100 <- replicate(100, mean(sample(distNormal, 10, replace = TRUE))) ## 100 amostras de 10 casos

mean(mediaBootsNormal10)
mean(mediaBootsNormal50)
mean(mediaBootsNormal100)
mean(distNormal)
