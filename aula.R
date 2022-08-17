## Estudos Avançados de Metodologia de Pesquisa (PPGCP/UFPE) ##
## Professor: Hugo Medeiros ##
## Aluno: Rodrigo Lins ##

library('pacman')
library("eeptools")

## Criando objeto simples ##
vetor <- c(1,2,3,5,7)
str(vetor)

## Criando objeto complexo ##
reg1 <- lm(mpg~ ., mtcars)
reg1
str(reg1)

## Criando dataframe ##
## Vetor com nome dos paises
nomePaises <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Equador", "Paraguai", "Peru", "Uruguai", "Venezuela")

## Vetor com data de democratizacao
anoPaises <- as.Date(c("1983-01-01", "1982-01-01", "1985-01-01", "1990-01-01", "1958-01-01", "2002-01-01", "1989-01-01", "2001-01-01", "1985-01-01",
                       "1959-01-01"))

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
