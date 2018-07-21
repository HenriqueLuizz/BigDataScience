# Leitura dos arquivos
t <- read.csv("/home/henrique/Documents/BigData/Desafio1/train.csv",stringsAsFactors = FALSE)
install.packages("plyr")
library(plyr)

count(t,c("Sex","Survived"))

#Arvore de decisão
install.packages("party")
library(party)

t$Embarked
typeof(t$Embarked)

t$Embarked

for (i in 1:nrow(t)) {
  if (t$Embarked[i] == "S") {
    t$Embarked <- 0
  } else if (t$Embarked[i] == "C") {
    t$Embarked[i] <- 1
  } else {
    t$Embarked[i] <- 2
  }
}

typeof(t$Sex)
t$Sex <- type.convert(t$Sex,numerals = "no.loss")

t$Sex <- type.convert(t$Embarked,numerals = "no.loss")

mod <- ctree(Survived ~ Sex+Pclass+Age, data=t)
plot(mod,type="simple") 

summary(t)

#ML - Arvore de decisão
t <- read.csv("/home/henrique/Documents/BigData/Desafio1/train.csv",stringsAsFactors = FALSE)
set.seed(33)
va <- sample(nrow(t))
treino <- t[va[1:691],]
teste <- t[va[692:891],]

#mod <- ctree(Survived~Sex+Pclass, data = treino)
#mod <- ctree(Survived~Sex+Pclass+Age, data = treino)
#mod <- ctree(Survived~Sex+Pclass+Age+SibSp+Parch+Fare, data = treino)
mod <- ctree(Survived~Sex+Pclass+Age+SibSp+Parch+Fare, data = treino)

p <- predict(mod, newdata=teste)

prev <- ifelse(p<.5,0,1)
prev[9]

head(cbind(prev,teste$Survived,ifelse(prev == teste$Survived,1,0)))

#Somar os acertos
acertos <- sum(ifelse(prev == teste$Survived,1,0))
#Acurácia (ACC) - Assertividade
acertos/200
#Matriz de Confusão
table(prev, teste$Survived)

#Prediction
35/(35+3)
35/(35+53)
#Recall
38/(38+6)
38/(38+50)
