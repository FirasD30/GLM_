# Fit multiple regression to schoenemann data

# Load the data. You will have to change the directory here
sch.df <- read.csv("Schoenemann.csv", header = T)

# Summarize the data
summary(sch.df)

# Plot suggests a need to transform data
plot(sch.df$Fat, sch.df$CNS, pch = 19)

# Add transformed variables to the data frame
sch.df <- cbind(sch.df, lCns = log(sch.df$CNS))
sch.df <- cbind(sch.df, lFat = log(sch.df$Fat))
sch.df <- cbind(sch.df, lMass = log(sch.df$Mass), lMuscle = log(sch.df$MUSCLE), lHeart = log(sch.df$HEART), lBone = log(sch.df$BONE))

# Note that often it is useful to attach a specific data frame (e.g. attach(sch.df) to stop having to type sch.df all the tim)
attach(sch.df)

# Conduct the full multiple regression of explanatory variables on the central nervous system mass
m.full<-lm(lCns ~ lMass + lFat + lMuscle + lHeart + lBone, data = sch.df) 
summary(m.full)
#It doesn't make sense that lmass is negative!

# Add an interaction term to the model
m.full<-lm(lCns ~ lMass + lFat + lMass:lFat + lMuscle + lHeart + lBone, data = sch.df) 
summary(m.full)
#You need a reason to include the interaction term. Biological reasoning

# This is also equivalent to the above
m.full<-lm(lCns ~ lMass*lFat + lMuscle + lHeart + lBone, data = sch.df) 
summary(m.full)

# Add a polynomial (quadratic) term to the model
m.full<-lm(lCns ~ lMass + lFat + lMuscle + lHeart + lBone + I(lMuscle^2), data = sch.df) 
summary(m.full)
# I in I(lMuscle^2) indicates that this is one term
# Add it only when you have a strong biological reasoning. Example with elephants!


# Add a categorical predictor variable to the model. Here it is location. Location is a character
# A few more examples. Add in a categorical variable. Note that the results are differences from the first
# level of the factor (typically the one earliest in the alphabet). If you want to change this, read about
# contrasts. Furthermore, be careful if you are using categorical predictor variables which are indexed with 
# a number (e.g. 1,2,3,4 and 5 levels). You may need to use I()
m.full <- lm(lCns ~ lMass + lFat + lMuscle + lHeart + lBone + I(lMuscle^2) + sch.df$Location, data = sch.df) 
summary(m.full)

# When there is categorical variable, there is a reference level. Reference is what comes first in the alphabet (e.g. Brazil comes before Virginia).
# Intercept is what happens when you independent variable is at 0. E.g. if you set temp to 0 in a plant growth and temperature data +rainfall.
# Effect of rainfall is different on plant growth at low and high temperature. 
# Even if you don't understand these, still follow it and you'll understand it eventually! Just keep repeating information

# Fit a normal GLM to the data
m.glm<-glm(lCns ~ lMass + lFat + lMuscle + lHeart + lBone, data = sch.df) 
summary(m.glm)
