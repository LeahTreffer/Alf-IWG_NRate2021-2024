# Year 3 hulled vs cleaned 
# can we predict cleaned weights for grain using a subsample
# weighed before and after cleaning
# Jake suggested a R^2 of >0.8 would be an acceptable threshold

library(lme4)
library(lmerTest)
library(MuMIn)

##### KS

grainweights <- read.csv('data/Year 3 hulled vs cleaned - KS.csv')
grainweights$Rep <- as.factor(grainweights$Rep)
grainweights$Treatment <- as.factor(grainweights$Treatment)

plot(grainweights$Cleaned.weight, grainweights$Threshed.Weight)

model <- lm(Cleaned.weight~Threshed.Weight, data=grainweights)
model2 <- lm(Cleaned.weight~G.from.MasterDataSpreadsheet, data=grainweights)
model3 <- lm(Cleaned.weight~G.from.MasterDataSpreadsheet + Treatment, data=grainweights)
model4 <-lmer(Cleaned.weight~G.from.MasterDataSpreadsheet + Treatment+ (1|Rep), data=grainweights)

summary(model)
summary(model2)
summary(model3)
summary(model4)
r.squaredGLMM(model4)

AIC(model2,model3)

grainweights$Predicted_Dehulled <- predict(model2, newdata = grainweights) # intercept + Threshed Weights*Estimate

out <- grainweights[,c("Sample", "Predicted_Dehulled")]

colnames(out)[1] <- "Plot"
colnames(out)[2] <- "G"

write.csv(out, 'KS_Year_3_G.csv')

##### MN

grainweights <- read.csv('data/Year 3 hulled vs cleaned - Sheet2.csv')

plot(grainweights$Cleaned.weight, grainweights$Threshed.Weight)

model <- lm(Cleaned.weight~Threshed.Weight, data=grainweights)
model2 <- lm(Cleaned.weight~G.from.MasterDataSpreadsheet, data=grainweights)

summary(model)
summary(model2)

grainweights$Predicted_Dehulled <- predict(model2, newdata = grainweights)

out <- grainweights[,c("Sample", "Predicted_Dehulled")]

colnames(out)[1] <- "Plot"
colnames(out)[2] <- "G"

write.csv(out, 'MN_Year_3_G.csv')

##### WI

grainweights <- read.csv('data/Year 3 hulled vs cleaned - WI.csv')
grainweights$Rep <- as.factor(grainweights$Rep)
grainweights$Treatment <- as.factor(grainweights$Treatment)

plot(grainweights$Cleaned.weight, grainweights$Threshed.Weight)

model <- lm(Cleaned.weight~Threshed.Weight, data=grainweights)
model2 <- lm(Cleaned.weight~G.from.MasterDataSpreadsheet, data=grainweights)
model3 <-lmer(Cleaned.weight~G.from.MasterDataSpreadsheet + (1|Rep), data=grainweights)

summary(model)
summary(model2)
summary(model3)
r.squaredGLMM(model3)

AIC(model2,model3)

grainweights$Predicted_Dehulled <- predict(model2, newdata = grainweights) # intercept + Threshed Weights*Estimate

out <- grainweights[,c("Sample", "Predicted_Dehulled")]

colnames(out)[1] <- "Plot"
colnames(out)[2] <- "G"

write.csv(out, 'WI_Year_3_G.csv')
