# Leitura dos arquivos
train <- read.csv("/home/henrique/Documents/BigData/Desafio1/train.csv",stringsAsFactors = FALSE)
test <- read.csv("/home/henrique/Documents/BigData/Desafio1/test.csv", stringsAsFactors = FALSE)


#**** ESTOU TRABALHANDO AQUI!!!! ********
# Junte os dois conjuntos
#test$Survived <- NA
#combi <- rbind(train, test)

#Encontrar os dados que falta no Dataframe
#which(combi$Embarked == "")
#which(is.na(combi$Fare))
which(train$Embarked == "")
which(is.na(train$Fare))

# Corrija única Fare ausente
#combi$Fare[1044] <- mean(combi[combi$Pclass == 3 & combi$Embarked == 'S' & combi$Age > 60, 'Fare'], na.rm = TRUE)

# Corrija Embarked ausentes
#combi$Embarked[c(62,830)] = "S"
train$Embarked[c(62,830)] = "S"
#combi$Embarked <- factor(combi$Embarked)
train$Embarked <- factor(train$Embarked)

sapply(train,class)
#typeof(combi$Sex)
combi$Sex[combi$Sex %in% c('female')] <- 1
combi$Sex[combi$Sex %in% c('male')] <- 2
combi$Sex <- type.convert(combi$Sex,numerals = "no.loss")
#typeof(train$Sex)
train$Sex[train$Sex %in% c('female')] <- 1
train$Sex[train$Sex %in% c('male')] <- 2
train$Sex <- type.convert(train$Sex,numerals = "no.loss")


# Extraia o sobrenome e o título
#aux <- strsplit(combi$Name, "(,\\s)|[.]")
aux <- strsplit(train$Name, "(,\\s)|[.]")

# O split divide um dos nomes em muitas partes. Corrija com a linha abaixo.
aux[514][[1]] <- aux[514][[1]][-4]
aux <- do.call('rbind', aux)
combi$Surname <- aux[,1]
combi$Title <- aux[,2]

aux[514][[1]] <- aux[514][[1]][-4]
aux <- do.call('rbind', aux)
train$Surname <- aux[,1]
train$Title <- aux[,2]

# Apresente em tabela
#table(combi$Title)
table(train$Title)

# Calcule a média de idade dos passageiros por título e atribua aos passageiros sem idade
#for (n in 1:nrow(combi)) {
#  if (is.na(combi$Age[n])) {
#    combi$Age[n] <- mean(combi$Age[combi$Title == combi$Title[n]], na.rm = TRUE)
#  } else {
#    combi$Age[n] <- combi$Age[n]
#  }
#}
for (n in 1:nrow(train)) {
  if (is.na(train$Age[n])) {
    train$Age[n] <- mean(train$Age[train$Title == train$Title[n]], na.rm = TRUE)
  } else {
    train$Age[n] <- train$Age[n]
  }
}

# Una títulos semelhantes
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

train$Title[train$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
train$Title[train$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
train$Title[train$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'


# Defina como categorias
#combi$Title <- factor(combi$Title)
train$Title <- factor(train$Title)

# Agrupe pessoas em famílias
#combi$FamilySize <- combi$SibSp + combi$Parch + 1
#combi$FamilyID <- paste(combi$FamilySize, combi$Surname, sep="")

train$FamilySize <- train$SibSp + train$Parch + 1
train$FamilyID <- paste(train$FamilySize, train$Surname, sep="")

# Priorize famílias grandes
#combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
#combi$FamilyID <- factor(combi$FamilyID)

train$FamilyID[train$FamilySize <= 2] <- 'Small'
train$FamilyID <- factor(train$FamilyID)

# Retorne aos dois conjuntos iniciais
train <- combi[1:891,]
test <- combi[892:1309,]

install.packages('party')
library(party)

set.seed(456)
# Parâmetro usado no gerador de números aleatórios.
# Guarde este número se quiser reproduzir os resultados futuramente!

# Crie o modelo de predição
fit <- cforest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID, data = train, controls = cforest_unbiased(ntree=2000,mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- as.data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/home/henrique/Documents/BigData/Desafio1/predict1.csv", row.names = FALSE)



# Montagem do dataframe para saída de dados
pr_data <- cbind(test$PassengerId, Prediction)
pr_data
pr_data <- as.data.frame(pr_data)

# Nomes das colunas
names(pr_data) <- c("PassengerId", "Survived")

pr_data$Survived[pr_data$Survived %in% c(1)] <- 0
pr_data$Survived[pr_data$Survived %in% c(2)] <- 1


# Gravação em disco do arquivo a ser submetido no site Kaggle

write.csv(pr_data, file="/home/henrique/Documents/BigData/Desafio1/predict1.csv", row.names = FALSE)



#**** ESTOU TRABALHANDO ATE AQUI!!!! ********

# Execução do modelo
m1<-lm(Survived~Embarked, data=train)

# Previsão dos dados
p<-predict(m1, newdata=test)

# Classificação de Survived, através do gatilho 0.5
pr <- ifelse(p<0.5, 0, 1)

# Montagem do dataframe para saída de dados
pr_data <- cbind(test$PassengerId, pr)
pr_data <- as.data.frame(pr_data)

# Nomes das colunas
names(pr_data) <- c("PassengerId", "Survived")

# Gravação em disco do arquivo a ser submetido no site Kaggle
write.csv(pr_data, file="/home/henrique/Documents/BigData/Desafio1/predict.csv", row.names = FALSE)
