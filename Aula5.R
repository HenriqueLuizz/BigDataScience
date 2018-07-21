#Aula 5
#Mod 1 = RLS WT
#Utilizar a base mtcars
m <- mtcars
#Criar a semente para reproduzir o embaralhamento novamento
set.seed(33)
#Embaralhar a base de dados
vn <- sample(nrow(m))
#Criar a base de treino e teste
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
#Criar 
mod <- lm(mpg~wt, data = treino)
#Listar os dados
summary(mod)

#Criar a primeira previsão
p <- predict(mod, newdata = teste)

#Comparar as previsoes com a teste
head(cbind(p, teste$mpg))
#Calcular o erro
e <- p - teste$mpg
#Soma dos quadrados dos erros (SSE)
sum(e^2)

cor(m)
#-------------------------------------------
#Mod 2 = RLS CYL x Comparativos
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~cyl, data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))


#Mod 3 = RLS WT com Log de Wt
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~log(wt), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))

plot(m$mpg~m$wt)
plot(m$mpg~log(m$wt))

#Mod 4 - Polinomial de ordem 2 WT
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~poly(wt,2), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))

#Mod 5 - Polinomial de ordem 3 WT
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~poly(wt,3), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))

#Mod 5 - Polinomial de ordem 4 WT
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~poly(wt,4), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))

#Mod 5 - Polinomial de ordem 5 WT
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~poly(wt,5), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))

#Mod 6 - Polinomial de ordem 8 WT
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~poly(wt,8), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))

#Mod 7 - Polinomial de ordem 15 WT
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~poly(wt,15), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))


#Variavel Categoria
plot(m$mpg~m$wt, col=m$cyl+5, pch=m$cyl)

#Mod 8 - WT + Cyl
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]
mod <- lm(mpg~wt+as.factor(cyl), data = treino)
summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))

#Mod 9 - Torneio teste
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24],]
teste <- m[vn[25:32],]

#mod <- lm(mpg~wt+cyl+disp+hp, data = treino)
#mod <- lm(mpg~cyl+poly(wt,2), data = treino)

mod <- lm(mpg~poly(wt,2)+poly(carb,2), data = treino)
log(wt)+log(hp)
poly(dist,2)

summary(mod)
p <- predict(mod, newdata = teste)
e <- p - teste$mpg
sum(e^2)
head(cbind(p, teste$mpg))
m
cor(m$)
help(mtcars)


#Parte 2
#Classificador
# Leitura dos arquivos
train <- read.csv("/home/henrique/Documents/BigData/Desafio1/train.csv",stringsAsFactors = FALSE)
test <- read.csv("/home/henrique/Documents/BigData/Desafio1/test.csv", stringsAsFactors = FALSE)

#Probabilidade de sobrevivencia no Titanic
nrow(train[train$Survived==1,])/nrow(train)

#Probabilidade de sobrevivencia Feminino no Titanic
nrow(train[train$Sex=='female',])/nrow(train)

install.packages("sqldf")
library(sqldf)

sqldf('select Sex, count(Survived) from train group by Sex')

install.packages("plyr")
library(plyr)
count(train$Sex)

#Probabilidade de mulheres serem sobreviventes no Titanic
count(train[train$Survived==1,],"Sex")

count(train[train$Pclass==1 & train$Sex=='female',],"Survived")
