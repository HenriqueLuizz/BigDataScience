#Estatistica Descritiva

#Baixa o pacote GGPLOT2
install.packages("ggplot2")
#Carrega na memória o pacote GGPLOT2
library(ggplot2)
#Atrbuir Diamonds para D
d <- diamonds
mean(d$price) #Media (aritimetica)
median(d$price) #Mediana
sd(d$price) #Desvio Padrao
summary(d) #Resumo estatistico

#Amostragem
a1 <- d[1:3000,]
a2 <- d[3001:6000,]

summary(a1$price)
summary(a2$price)
summary(d$price)

#Amostra aleatoria
sample(3) #Geração de numeros aleatorios de 1 a 3
set.seed(33) #Define a semente para geração de numero aleatorio
sample(3)

va <- sample(nrow(d)) #Cria um vetor com o total de linhas embaralhado

a3 <- d[va[1:3000],]
summary(a3)
summary(d)
summary(a3$price)
summary(d$price)

#Histograma
hist(d$depth)

par(mfrow=c(2,2)) #Janela grafica 2x2
hist(d$depth)
hist(a1$depth)
hist(a2$depth)
hist(a3$depth)

hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

#Exercicio Est. Descritiva
a <- anscombe
mean(a$x1)
mean(a$y1)
mean(a$x2)
mean(a$y2)
mean(a$x3)
mean(a$y3)
mean(a$x4)
mean(a$y4)

sd(a$x1)
sd(a$y1)
sd(a$x2)
sd(a$y2)
sd(a$x3)
sd(a$y3)
sd(a$x4)
sd(a$y4)

summary(a)

plot(a$x1 ~ a$y1)
cor(a$x1, a$y1)
cor(a$x2, a$y2)
cor(a$x4, a$y4)
par(mfrow=c(2,2))
plot(a$y1 ~ a$x1)
plot(a$y2 ~ a$x2)
plot(a$y3 ~ a$x3)
plot(a$y4 ~ a$x4)
#Grafico de dispersão
#Verifica de forma intuitiva se tem relação entre as duas variaveis
par(mfrow=c(1,1)) #Janela grafica 1x1
mtcars
plot(mtcars$mpg ~ mtcars$wt)

#Calcular a correlação entre as variaveis
cor(mtcars$mpg , mtcars$wt)
cor(mtcars$mpg , mtcars$hp)
cor(mtcars) #Matriz de correlação

cor(d$y , d$z)
help(mtcars)

v2 <- c(1,2,3,4,5)
sum(v2)
v3 <- c(2,3,4,5,6)
c(v2,v3)
rbind(v2,v3)
cbind(v2,v3)
help("rbind")
plot(t)

t <- c(1, 2, 5, 6, 7, 3, 9, 12, 19)
d <- sd(t)
m <- mean(t)
d
v1 <- sqrt(((t - m)/d)^2)
v1 > 2
v1

t6 <- c(3, 7, 2)
(3*1+7*1+2*2)/(sum(1,1,2))
median(t6)

mean(c(1, 1000, 23, 27))

median(c(1, 1, 1, 1, 1, 11, 11, 11, 1, 1, 1, 1, 1, 1, 1, 11, 11, 11, 11, 11, 11, 2, 11, 11, 11))

hist(mtcars$mpg)
summary(mtcars$mpg)

sd(mtcars$wt)
summary(mtcars$wt)

cor(mtcars$mpg , mtcars$wt)
plot(mtcars$mpg, mtcars$wt)
