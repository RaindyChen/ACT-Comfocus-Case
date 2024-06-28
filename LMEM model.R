install.packages("lme4")
library(lme4)

# LMEM model + Interaction
formula <- as.formula("Preference ~ EDA_AR + RMSSD + PFD + Ave_Surprised + Ave_Angry + Ave_Disgusted + Ave_Happy + Ave_Sad + Ave_Scared + Ave_Angry:Ave_Disgusted + (1 | Session) + (1 | EventRene)")
model_1 <- glmer(formula, data = LMEM1, family = binomial)
summary(model_1)


# LMEM model
formula <- as.formula("Preference ~ EDA_AR + RMSSD + PFD + Ave_Surprised + Ave_Angry + Ave_Disgusted + Ave_Happy + Ave_Sad + Ave_Scared + (1 | Session) + (1 | EventRene)")
model_2 <- glmer(formula, data = LMEM1, family = binomial)
summary(model_2)

