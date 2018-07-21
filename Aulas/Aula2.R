# Estatística Descritiva
install.packages("ggplot2")
library(ggplot2)
d <- diamonds

mean(d$price)   # Média (aritmética)
median(d$price) # Mediana
sd(d$price)     # Desvio Padrão
summary(d)      # Resumo estatístico

# Amostragem
a1 <- d[1:3000,]
a2 <- d[3001:6000,]

# Amostras aleatórias
sample(3)     # Geração de números aleatórios de 1 a 3
set.seed(33)  # Definição da semente para geração de números aleatórios
sample(3)

set.seed(33)
va <- sample(nrow(d))
a3 <- d[va[1:3000],]  

summary(d[,1])
summary(d[,"price"])
summary(d[,1:3])
summary(d$price)
          
summary(a3$price)

set.seed(33)
a4 <- sample(d$price, 3000)

# Histograma
hist(d$depth)
par(mfrow=c(2,2)) # Janela gráfica 2x2
hist(d$depth)
hist(a1$depth)
hist(a2$depth)
hist(a3$depth)

hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Exercício Est. Descritiva
a <- anscombe
mean(a$x1)
sd(a$x1)
cor(a$x1, a$y1)
cor(a$x2, a$y2)
cor(a$x3, a$y3)
cor(a$x4, a$y4)
par(mfrow=c(2,2))
plot(a$y1 ~ a$x1)
plot(a$y2 ~ a$x2)
plot(a$y3 ~ a$x3)
plot(a$y4 ~ a$x4)

# Gráfico de dispersão
par(mfrow=c(1,1))
mtcars
plot(mtcars$mpg ~ mtcars$wt)
cor(mtcars$mpg, mtcars$wt)
cor(mtcars$mpg, mtcars$hp)
cor(mtcars)    # Matriz de correlação
