# Start of chunk
# NdZFA is the data table for Nrate data (Nd) isolation
# it looks like only the shockwave alfalfa was included in this analysis
# filtered to the third year (Z), fall harvest (F), plots with alfalfa present (A)

# N = N rate 
# P = Location
# CROP = Plot type (mono/inter)
# Rep = Replicate

#LINEAR
modNZFA<-lmer(PlotAlfDryKgHa~N*P*CROP + (1|Rep), data=NdZFA)
modNZFA<-lmer(PlotAlfDryKgHa~N*P*CROP + (1|P:Rep), data=NdZFA)
# boundary (singular) fit: see help('isSingular')
# location fixed effect
# account for differences bewteen locations/reps ; reps in one location shouldn't be names the same as another location
## paste togehter locRep
## or 1|P/rep

# varience on boundry of what is possible (0)
# rand effect with few levels, rand eff var is biased downwards (estimate is too small)
# plot variation between reps (box plots: are reps different or not), if there isn't any could ignore; if it's really small its not adding to model; could also maybe be treat as fixed effect
# Effects standard errors but usualy arn't trying to get those random effect estimates so it's often ignored 
# rand effect levels > 10

anova(modNZFA)
#Check assumptions
NZFA_resid<-resid(modNZFA)
qqnorm(NZFA_resid)
qqline(NZFA_resid)
plot(NZFA_resid)

#log transform 
modNZFA1<-lmer(log(PlotAlfDryKgHa)~N*P*CROP + (1|Rep), data=NdZFA)
# boundary (singular) fit: see help('isSingular')

anova(modNZFA1)
#Check assumptions
NZFA1_resid<-resid(modNZFA1)
qqnorm(NZFA1_resid)
qqline(NZFA1_resid)
plot(modNZFA1)


# Trying to troubleshoot

# confirmed only locations with values were included
# no NAs in response column (PlotAlfDryKgHa)

class(NdZFA$N) # numeric
class(NdZFA$P) # factor
class(NdZFA$CROP) # character
NdZFA$CROP <- as.factor(NdZFA$CROP) # try as factor
# nope, didn't fix it

# NY has two plots with 0 as the yield
# try changing the 0s in NY to 1-when log transforming the 0s get set to NA which would mess up stuff
NdZFA[1,40] <- 1
NdZFA[30,40] <- 1
# didn't fix it

# value distributions
NdZFA_NY <- NdZFA %>% filter(Location == "NY")
hist(NdZFA_NY$PlotAlfDryKgHa)
NdZFA_WI <- NdZFA %>% filter(Location == "WI")
hist(NdZFA_WI$PlotAlfDryKgHa)

# how correlated values across sites or across treatment (any categorical variable)
# alf biomass in WI cor to alf biomass in NY
# is alf biomass in WI different than alf biomass in NY
aov1 = aov(PlotAlfDryKgHa ~ P, data=NdZFA) # between locations
summary(aov1)
cor(NdZFA_NY$PlotAlfDryKgHa,NdZFA_WI$PlotAlfDryKgHa)

# alf biomass in monoA cor to alf biomass in inter
# is alf biomass in monoA different than alf biomass in inter
NdZFA_mA <- NdZFA %>% filter(CROP == "monoA")
NdZFA_I <- NdZFA %>% filter(CROP == "inter")
aov2 = aov(PlotAlfDryKgHa ~ CROP, data=NdZFA) # between crops
summary(aov2)
cor(NdZFA_mA$PlotAlfDryKgHa,NdZFA_I$PlotAlfDryKgHa)

# alf biomass correlation in reps
# is there a difference between the mean alf biomass of different reps
aov3 = aov(PlotAlfDryKgHa ~ Rep, data=NdZFA) # between reps
summary(aov3)

NdZFA$ID <- paste0(NdZFA$Location, "_", NdZFA$Treatment)
test <- NdZFA[,c("ID", "Rep", "PlotAlfDryKgHa")]
df_wide <- test %>%
  pivot_wider(names_from = Rep, values_from = PlotAlfDryKgHa, names_prefix = "rep_")
correlation_matrix <- df_wide %>%
  select(starts_with("rep_")) %>%
  cor()
correlation_matrix



# low correlation for locations, higher correlation for crop type (CROP) and for some Reps
# Could that correlation for Reps be the issue? 


