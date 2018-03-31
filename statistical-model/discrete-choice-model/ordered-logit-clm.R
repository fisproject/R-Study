library(ordinal)

head(MASS::housing)

class(MASS::housing$Sat)

# ordered logit mode
house.logit <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
summary(house.logit, digits = 3)

confint(house.logit)

predict(house.logit, MASS::housing, type = "p")

texreg::screenreg(house.logit)
