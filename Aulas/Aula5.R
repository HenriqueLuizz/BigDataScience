# Transformações Lineares
# Mod 1 - RLS wt
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24] ,]
teste  <- m[vn[25:32],]
mod <- lm(mpg~wt, data=treino)
summary(mod)
p <- predict(mod, newdata=teste)
head(cbind(p, teste$mpg))
e <- p - teste$mpg
sum(e^2)
# Mod 2 - RLS cyl x comparativos
m <- mtcars
set.seed(33)
vn <- sample(nrow(m))
treino <- m[vn[1:24] ,]
teste  <- m[vn[25:32],]
#mod <- lm(mpg~wt, data=treino)
#mod <- lm(mpg~log(wt), data=treino)
#mod <- lm(mpg~poly(wt,2), data=treino)
#mod <- lm(mpg~poly(wt,3), data=treino)
mod <- lm(mpg~wt+as.factor(cyl), data=treino)
summary(mod)
p <- predict(mod, newdata=teste)
e <- p - teste$mpg
sum(e^2)

# Variavel categoria
plot(m$mpg~m$wt, col=m$cyl+5, pch=m$cyl)

# PARTE 2
# Classificadores
t <- read.csv("train.csv")

# Probabilidade de sobrevivência no Titanic
nrow(t[t$Survived==1,])/nrow(t)

# Probabilidade passageiro gênero feminino no Titanic
nrow(t[t$Sex=="female",])/nrow(t)

install.packages("sqldf")
library(sqldf)

sqldf('select Sex,count(Survived) from t group by Sex')

install.packages("plyr")
library(plyr)
count(t, c("Sex"))

# Probabilidade de mulheres serem sobreviventes no Titanic
count(t[t$Survived==1,],"Sex")

count(t[t$Pclass==1 & t$Sex=='female',] ,"Survived")


