# Aula 4 - regressões em R
d <- read.csv("PIB.csv", sep=";", dec=",")

# Regressão linear simples, PIB explicado por BRP
mod <- lm(PIB~BR_PESADOS, data=d)

# Resumo parâmetros da regressão
summary(mod)
mod$residuals

# Regressão linear multiv, PIB explicado por BRP e SPT
mod2 <- lm(PIB~BR_PESADOS+SP_TOTAL, data=d)
summary(mod2)
