# Leitura dos arquivos
train <- read.csv("/home/henrique/Documents/BigData/Desafio1/train.csv",stringsAsFactors = FALSE)
test <- read.csv("/home/henrique/Documents/BigData/Desafio1/test.csv", stringsAsFactors = FALSE)

# Unir os conjuntos para tratar os dados
test$Survived <- NA
combi <- rbind(train, test)

#Which encontra os dados que falta em uma coluna no Dataframe
which(combi$Embarked == "")
which(is.na(combi$Fare))

# Corrija única Fare ausente
#Retorna a 
combi$Fare[1044] <- mean(combi[combi$Pclass == 3 & combi$Embarked == 'S' & combi$Age > 60, 'Fare'], na.rm = TRUE)

# Corrija Embarked ausentes
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

typeof(combi$Sex)

combi$Sex[combi$Sex %in% c('female')] <- 1
combi$Sex[combi$Sex %in% c('male')] <- 2

combi$Sex <- type.convert(combi$Sex,numerals = "no.loss")

# Extraia o sobrenome e o título
aux <- strsplit(combi$Name, "(,\\s)|[.]")

# O split divide um dos nomes em muitas partes. Corrija com a linha abaixo.
aux[514][[1]] <- aux[514][[1]][-4]
aux <- do.call('rbind', aux)

combi$Surname <- aux[,1]
combi$Title <- aux[,2]

# Apresente em tabela
table(combi$Title)

# Calcule a média de idade dos passageiros por título e atribua aos passageiros sem idade
for (n in 1:nrow(combi)) {
  if (is.na(combi$Age[n])) {
    combi$Age[n] <- mean(combi$Age[combi$Title == combi$Title[n]], na.rm = TRUE)
  } else {
    combi$Age[n] <- combi$Age[n]
  }
}

# Una títulos semelhantes
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

# Defina como categorias
combi$Title <- factor(combi$Title)
combi$Pclass <- factor(combi$Pclass)

# Agrupe pessoas em famílias
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$FamilyID <- paste(combi$FamilySize, combi$Surname, sep="")

# Priorize famílias grandes
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# Retorne aos dois conjuntos iniciais
train <- combi[1:891,]
test <- combi[892:1309,]

install.packages("rpart")
library(rpart)

arvore <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID, data = train, method="class")

previsao <- predict(arvore, test, type="class")

submissao <- data.frame(PassengerId=test$PassengerId, Survived=test$Survived)

submissao$Survived <- previsao

submissao$Survived
submissao$PassengerId

#Somar os acertos
acertos <- sum(ifelse(submissao$Survived == test$Survived,1,0))
ifelse(submissao$Survived == test$Survived,1,0)
#Acurácia (ACC) - Assertividade
acertos/200
#Matriz de Confusão
table(prev, teste$Survived)

write.csv(submissao, file="/home/henrique/Documents/BigData/Desafio1/predict.csv", row.names=FALSE)
