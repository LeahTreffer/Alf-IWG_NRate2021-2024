### May 29 2026
mod<-lmer(G~N*P*CROP*as.factor(Year) + (1|Rep:P) + (1|Plot:Rep:P), data=subset(Nd, P != "MN")) # unique combos of plot-rep-location #could also include harvest as a fixed effect
mod<-lmer(sqrt(G)~N*P*CROP*as.factor(Year) + (1|Rep:P) + (1|Plot:Rep:P), data=subset(Nd, P != "MN")) # unique combos of plot-rep-location #could also include harvest as a fixed effect
# singularity : varience on plot level is low compared to residule (1.8 e-6) or rep so plot-to-plot var is estentially zero
Nd %>% group_by(P,Rep,Plot)%>%count()%>%View()

with(subset(Nd, !is.na(G)), table(CROP, N, P, Year)) 
with(subset(Nd, !is.na(G)), table(P, Year))# don't have NY or WI 2025, MN 2024, 2023, 2022

hist(residuals(mod))
plot(predict(mod),residuals(mod)) # untransformed or sqrt is okay

# could go back and make rep unique 

ggplot(subset(Nd, P != "MN"), aes(x=N, y=G, color=as.factor(Year)))+geom_point()+facet_grid(CROP~P)+geom_smooth(method="lm", se=FALSE)

anova(mod)
emmeans(mod, ~N*P*CROP*I(Year-2020), cov.reduce=FALSE)
emmip(mod, Year~N | CROP*P, cov.reduce=FALSE, CIs = TRUE) #model predicted vales
emmip(mod, N~Year | CROP*P, cov.reduce=FALSE, CIs = TRUE) #model predicted vales
emtrends(mod, ~Year+CROP+P, var="N", infer=TRUE) # slope different than zero
emtrends(mod, pairwise~Year|CROP+P, var="N", infer=TRUE) # slope different by cropP

