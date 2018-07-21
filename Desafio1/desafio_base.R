# Leitura dos arquivos
train <- read.csv("/home/henrique/Documents/BigData/Desafio1/train.csv")
test <- read.csv("/home/henrique/Documents/BigData/Desafio1/test.csv")

# Execução do modelo
m1<-lm(Survived~Pclass + Age + SibSp + Parch + Fare + Embarked, data=train)

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

