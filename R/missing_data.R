library(tidyverse)
library(mice)

titanic_train <- read.csv("data/train.csv")

# Me fijo los datos que faltan
md.pattern(titanic_train, rotate.names = T)

# Build mice object
mice_obj <- mice(titanic_train, m = 50, 
                 dependen = dependent, independen = independent, seed = 1234)
summary(mice_obj)

mice_obj_f <- filter(mice_obj, Age<50)

titanic_train_full <- complete(mice_obj, action="long")
titanic_train_full_f <- complete(titanic_train_full_f, action="long")

sum(titanic_train_full$Age>50)
sum(titanic_train_full_f$Age>50)
         
# Chequeo los datos que faltan
md.pattern(titanic_train_full, rotate.names = T)

head(titanic_train_full)
first_model <- glm(Survived ~ Age + Sex, data = titanic_train_full, family = "binomial")

library(performance)
library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = first_model, plot = F)
plot(simulationOutput)

