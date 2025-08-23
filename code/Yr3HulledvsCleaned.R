# Year 3 hulled vs cleaned 
# can we predict cleaned weights for grain using a subsample
# weighed before and after cleaning
# Jake suggested a R^2 of >0.8 would be an acceptable threshold

##### KS

grainweights <- read.csv('data/Year 3 hulled vs cleaned - Sheet1.csv')

plot(grainweights$Cleaned.weight, grainweights$Threshed.Weight)

model <- lm(Cleaned.weight~Threshed.Weight, data=grainweights)
model2 <- lm(Cleaned.weight~G.from.MasterDataSpreadsheet, data=grainweights)

summary(model)
summary(model2)

grainweights$Predicted_Dehulled <- predict(model2, newdata = grainweights)

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


