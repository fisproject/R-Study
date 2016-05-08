require(MASS)

head(housing)

class(housing$Sat)

# ordered logit model (proportional odds model)
house.logit <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
summary(house.logit, digits = 3)

predict(house.logit, housing, type = "p")

texreg::screenreg(house.logit)

# ordered probit model
house.probit <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, method = "probit")
summary(house.probit, digits = 3)

predict(house.probit, housing, type = "p")

texreg::screenreg(house.probit)
