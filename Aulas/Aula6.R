# Aula 6
t <- read.csv("train.csv")

install.packages("plyr")
library(plyr)
count(t,c("Sex","Survived"))

# Árvores de decisão
install.packages("party")
library(party)
mod <- ctree(Survived ~ Sex+Pclass+Age, data=t)
plot(mod,type="simple")

# ML - Árvores de decisão
t <- read.csv("train.csv")
set.seed(33)
va <- sample(nrow(t))
treino <- t[va[1:691]  ,]
teste  <- t[va[692:891],]
mod <- ctree(Survived~Sex+Pclass+Age, data=treino)
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)
head(cbind(prev,teste$Survived, ifelse(prev==teste$Survived,1,0)))
# Acertos
sum( ifelse(prev==teste$Survived,1,0))
# Matriz de Confusão
table(teste$Survived, prev)




