# Aula 4 - regress�es em R
d <- read.csv("PIB.csv", sep=";", dec=",")

# Regress�o linear simples, PIB explicado por BRP
mod <- lm(PIB~BR_PESADOS, data=d)

# Resumo par�metros da regress�o
summary(mod)
mod$residuals

# Regress�o linear multiv, PIB explicado por BRP e SPT
mod2 <- lm(PIB~BR_PESADOS+SP_TOTAL, data=d)
summary(mod2)
