# remove MN X S plot 1320 & 1201
# Source - https://stackoverflow.com/a/43446592
# Posted by Erdem Akkas
# Retrieved 2026-07-10, License - CC BY-SA 3.0

test <- forage_data_fall[!(forage_data_fall$Location == "MN" & forage_data_fall$YearCode == "X" & forage_data_fall$Harvest == "Fall" & forage_data_fall$Plot == 1122),] 

test <- test[!(test$Location == "MN" & test$YearCode == "X" & test$Harvest == "Fall" & test$Plot == 1201),] 

test <- test[!(test$Location == "MN" & test$YearCode == "X" & test$Harvest == "Fall" & test$Plot == 1320),] 

test <- test[!(test$Location == "MN" & test$YearCode == "X" & test$Harvest == "Fall" & test$Plot == 1404),] 

# Nd SW 0 
#IWG partial LER 
NdSW0IWGLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERIWGSW0 = (PlotIWGDryKgHa[Treatment == 10]) / (PlotIWGDryKgHa[Treatment == 5])
  )
#Alf partial LER 
NdSW0ALFLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERAlfSW0 = (PlotAlfDryKgHa[Treatment == 10]) / (PlotAlfDryKgHa[Treatment == 15])
  )
#Total LER 
NdSW0TotalLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    TotalLERSW0 = (PlotIWGDryKgHa[Treatment == 10]) / (PlotIWGDryKgHa[Treatment == 5]) + (PlotAlfDryKgHa[Treatment == 10]) / (PlotAlfDryKgHa[Treatment == 15])
  ) 
#Join together first two tables
LERresults1<-left_join(NdSW0IWGLER, NdSW0ALFLER, by = c("Rep", "P", "YearCode", "Harvest"))
#join together new joined table and third table 
LERresultsNdSW0<-left_join(LERresults1, NdSW0TotalLER, by = c("Rep", "P", "YearCode", "Harvest"))

# Nd SW 40 
#IWG partial LER 
NdSW40IWGLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERIWGSW40 = (PlotIWGDryKgHa[Treatment == 11]) / (PlotIWGDryKgHa[Treatment == 6])
  )
#Alf partial LER 
NdSW40ALFLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERAlfSW40 = (PlotAlfDryKgHa[Treatment == 11]) / (PlotAlfDryKgHa[Treatment == 16])
  )
#Total LER 
NdSW40TotalLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    TotalLERSW40 = (PlotIWGDryKgHa[Treatment == 11]) / (PlotIWGDryKgHa[Treatment == 6]) + (PlotAlfDryKgHa[Treatment == 11]) / (PlotAlfDryKgHa[Treatment == 16])
  ) 
#Join together first two tables
LERresults2<-left_join(NdSW40IWGLER, NdSW40ALFLER, by = c("Rep", "P", "YearCode", "Harvest"))
#join together new joined table and third table 
LERresultsNdSW40<-left_join(LERresults2, NdSW40TotalLER, by = c("Rep", "P", "YearCode", "Harvest"))

#Nd SW 80
#IWG partial LER 
NdSW80IWGLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERIWGSW80 = (PlotIWGDryKgHa[Treatment == 12]) / (PlotIWGDryKgHa[Treatment == 7])
  )
#Alf partial LER 
NdSW80ALFLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERAlfSW80 = (PlotAlfDryKgHa[Treatment == 12]) / (PlotAlfDryKgHa[Treatment == 17])
  )
#Total LER 
NdSW80TotalLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    TotalLERSW80 = (PlotIWGDryKgHa[Treatment == 12]) / (PlotIWGDryKgHa[Treatment == 7]) + (PlotAlfDryKgHa[Treatment == 12]) / (PlotAlfDryKgHa[Treatment == 17])
  ) 
#Join together first two tables
LERresults3<-left_join(NdSW80IWGLER, NdSW80ALFLER, by = c("Rep", "P", "YearCode", "Harvest"))
#join together new joined table and third table 
LERresultsNdSW80<-left_join(LERresults3, NdSW80TotalLER, by = c("Rep", "P", "YearCode", "Harvest"))

#Nd SW 120
#IWG partial LER 
NdSW120IWGLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERIWGSW120 = (PlotIWGDryKgHa[Treatment == 13]) / (PlotIWGDryKgHa[Treatment == 8])
  )
#Alf partial LER 
NdSW120ALFLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERAlfSW120 = (PlotAlfDryKgHa[Treatment == 13]) / (PlotAlfDryKgHa[Treatment == 18])
  )
#Total LER 
NdSW120TotalLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    TotalLERSW120 = (PlotIWGDryKgHa[Treatment == 13]) / (PlotIWGDryKgHa[Treatment == 8]) + (PlotAlfDryKgHa[Treatment == 13]) / (PlotAlfDryKgHa[Treatment == 18])
  ) 
#Join together first two tables
LERresults4<-left_join(NdSW120IWGLER, NdSW120ALFLER, by = c("Rep", "P", "YearCode", "Harvest"))
#join together new joined table and third table 
LERresultsNdSW120<-left_join(LERresults4, NdSW120TotalLER, by = c("Rep", "P", "YearCode", "Harvest"))

#Nd SW 160
#IWG partial LER 
NdSW160IWGLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERIWGSW160= (PlotIWGDryKgHa[Treatment == 14]) / (PlotIWGDryKgHa[Treatment == 9])
  )
#Alf partial LER 
NdSW160ALFLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    PartialLERAlfSW160 = (PlotAlfDryKgHa[Treatment == 14]) / (PlotAlfDryKgHa[Treatment == 19])
  )
#Total LER 
NdSW160TotalLER <- test %>%
  group_by(Rep, P, YearCode, Harvest) %>%
  reframe(
    TotalLERSW160 = (PlotIWGDryKgHa[Treatment == 14]) / (PlotIWGDryKgHa[Treatment == 9]) + (PlotAlfDryKgHa[Treatment == 14]) / (PlotAlfDryKgHa[Treatment == 19])
  ) 
#Join together first two tables
LERresults5<-left_join(NdSW160IWGLER, NdSW160ALFLER, by = c("Rep", "P", "YearCode", "Harvest"))
#join together new joined table and third table 
LERresultsNdSW160<-left_join(LERresults5, NdSW160TotalLER, by = c("Rep", "P", "YearCode", "Harvest"))

#merge all LER results into one data frame
LERresults6<-left_join(LERresultsNdSW0, LERresultsNdSW40, by = c("Rep", "P", "YearCode", "Harvest"))
LERresults7<-left_join(LERresults6, LERresultsNdSW80, by = c("Rep", "P", "YearCode", "Harvest"))
LERresults8<-left_join(LERresults7, LERresultsNdSW120, by = c("Rep", "P", "YearCode", "Harvest"))
LERresultsNdSWALL<-left_join(LERresults8, LERresultsNdSW160, by = c("Rep", "P", "YearCode", "Harvest"))

#Rearrange dataframe
LERresultsNdSWALL_long <- LERresultsNdSWALL %>%
  pivot_longer(cols = starts_with("PartialLER") | starts_with("TotalLER"),
               names_to = "Nrate",
               values_to = "LER")

#try to make the dataframe more usable

#cut off total and partial 
LERresultsNdSWALL_long$Type <- sub("^(Partial|Total)", "", LERresultsNdSWALL_long$Nrate)
LERresultsNdSWALL_long$tot_or_part <- sub("^(Partial|Total).*", "\\1", LERresultsNdSWALL_long$Nrate)

#cut off LERIWGSW and LERAlfSW and LERSW
LERresultsNdSWALL_long$NRATE <- sub("^(LERIWGSW|LERAlfSW|LERSW)", "", LERresultsNdSWALL_long$Type)
LERresultsNdSWALL_long$PlotType <- sub("^(LERIWGSW|LERAlfSW|LERSW).*", "\\1", LERresultsNdSWALL_long$Type)

#get rid of pesky NA results (caused by missing plots inherent in year 1 KS summer data)
LERresultsNdSWALL_longNONA<- na.omit(LERresultsNdSWALL_long)

LERresultsNdSWALL_longNONA$NRATE<-as.numeric(LERresultsNdSWALL_longNONA$NRATE)

#includes total and partial until it doesn't...used for some pairwise comparison thingy. 
LERResultsAllX<-LERresultsNdSWALL_longNONA[LERresultsNdSWALL_longNONA$YearCode == 'X',]
LERResultsAllY<-LERresultsNdSWALL_longNONA[LERresultsNdSWALL_longNONA$YearCode == 'Y',]
LERResultsAllZ<-LERresultsNdSWALL_longNONA[LERresultsNdSWALL_longNONA$YearCode == 'Z',]
LERResultsAllXS<-LERResultsAllX[LERResultsAllX$Harvest == 'Summer',]
LERResultsTotXS<-LERResultsAllXS[LERResultsAllXS$tot_or_part == 'Total',]
LERResultsPartIWGXS<-LERResultsAllXS[LERResultsAllXS$tot_or_part == 'Partial' & LERResultsAllXS$PlotType == 'LERIWGSW' ,]
LERResultsPartAlfXS<-LERResultsAllXS[LERResultsAllXS$tot_or_part == 'Partial' & LERResultsAllXS$PlotType == 'LERAlfSW' ,]
LERResultsAllXF<-LERResultsAllX[LERResultsAllX$Harvest == 'Fall',]
LERResultsTotXF<-LERResultsAllXF[LERResultsAllXF$tot_or_part == 'Total',]
LERResultsPartIWGXF<-LERResultsAllXF[LERResultsAllXF$tot_or_part == 'Partial' & LERResultsAllXF$PlotType == 'LERIWGSW' ,]
LERResultsPartAlfXF<-LERResultsAllXF[LERResultsAllXF$tot_or_part == 'Partial' & LERResultsAllXF$PlotType == 'LERAlfSW' ,]
LERResultsAllYS<-LERResultsAllY[LERResultsAllY$Harvest == 'Summer',]
LERResultsTotYS<-LERResultsAllYS[LERResultsAllYS$tot_or_part == 'Total',]
LERResultsPartIWGYS<-LERResultsAllYS[LERResultsAllYS$tot_or_part == 'Partial' & LERResultsAllYS$PlotType == 'LERIWGSW' ,]
LERResultsPartAlfYS<-LERResultsAllYS[LERResultsAllYS$tot_or_part == 'Partial' & LERResultsAllYS$PlotType == 'LERAlfSW' ,]
LERResultsAllYF<-LERResultsAllY[LERResultsAllY$Harvest == 'Fall',]
LERResultsTotYF<-LERResultsAllYF[LERResultsAllYF$tot_or_part == 'Total',]
LERResultsPartIWGYF<-LERResultsAllYF[LERResultsAllYF$tot_or_part == 'Partial' & LERResultsAllYF$PlotType == 'LERIWGSW' ,]
LERResultsPartAlfYF<-LERResultsAllYF[LERResultsAllYF$tot_or_part == 'Partial' & LERResultsAllYF$PlotType == 'LERAlfSW' ,]
LERResultsAllZS<-LERResultsAllZ[LERResultsAllZ$Harvest == 'Summer',]
LERResultsTotZS<-LERResultsAllZS[LERResultsAllZS$tot_or_part == 'Total',]
LERResultsPartIWGZS<-LERResultsAllZS[LERResultsAllZS$tot_or_part == 'Partial' & LERResultsAllZS$PlotType == 'LERIWGSW' ,]
LERResultsPartAlfZS<-LERResultsAllZS[LERResultsAllZS$tot_or_part == 'Partial' & LERResultsAllZS$PlotType == 'LERAlfSW' ,]
LERResultsAllZF<-LERResultsAllZ[LERResultsAllZ$Harvest == 'Fall',]
LERResultsTotZF<-LERResultsAllZF[LERResultsAllZF$tot_or_part == 'Total',]
LERResultsPartIWGZF<-LERResultsAllZF[LERResultsAllZF$tot_or_part == 'Partial' & LERResultsAllZF$PlotType == 'LERIWGSW' ,]
LERResultsPartAlfZF<-LERResultsAllZF[LERResultsAllZF$tot_or_part == 'Partial' & LERResultsAllZF$PlotType == 'LERAlfSW' ,]

# change Inf values to NA
LERresultsNdSWALL_longNONA
LERresultsNdSWALL_longNONA['LER'][LERresultsNdSWALL_longNONA['LER'] == 'Inf'] <- NA

#okay now I should be able to split by total and partial
TotalLERresults<-LERresultsNdSWALL_longNONA[LERresultsNdSWALL_longNONA$tot_or_part == 'Total',]
TotLERX<-TotalLERresults[TotalLERresults$YearCode == 'X',]
TotLERY<-TotalLERresults[TotalLERresults$YearCode == 'Y',]
TotLERZ<-TotalLERresults[TotalLERresults$YearCode == 'Z',]
TotLERXS<-TotLERX[TotLERX$Harvest == 'Summer',]
TotLERXF<-TotLERX[TotLERX$Harvest == 'Fall',]
TotLERYS<-TotLERY[TotLERY$Harvest == 'Summer',]
TotLERYF<-TotLERY[TotLERY$Harvest == 'Fall',]
TotLERZS<-TotLERZ[TotLERZ$Harvest == 'Summer',]
TotLERZF<-TotLERZ[TotLERZ$Harvest == 'Fall',]
IWGPartialLERresults<-LERresultsNdSWALL_longNONA[LERresultsNdSWALL_longNONA$PlotType == 'LERIWGSW',]
IWGPartialLERresults$NRATE<- as.numeric(IWGPartialLERresults$NRATE)
IWGPLERX<-IWGPartialLERresults[IWGPartialLERresults$YearCode == 'X',]
IWGPLERY<-IWGPartialLERresults[IWGPartialLERresults$YearCode == 'Y',]
IWGPLERZ<-IWGPartialLERresults[IWGPartialLERresults$YearCode == 'Z',]
IWGPLERXS<-IWGPLERX[IWGPLERX$Harvest == 'Summer',]
IWGPLERXF<-IWGPLERX[IWGPLERX$Harvest == 'Fall',]
IWGPLERYS<-IWGPLERY[IWGPLERY$Harvest == 'Summer',]
IWGPLERYF<-IWGPLERY[IWGPLERY$Harvest == 'Fall',]
IWGPLERZS<-IWGPLERZ[IWGPLERZ$Harvest == 'Summer',]
IWGPLERZF<-IWGPLERZ[IWGPLERZ$Harvest == 'Fall',]
AlfPartialLERresults<-LERresultsNdSWALL_longNONA[LERresultsNdSWALL_longNONA$PlotType == 'LERAlfSW',]
AlfPartialLERresults$NRATE<- as.numeric(AlfPartialLERresults$NRATE)
AlfPLERX<-AlfPartialLERresults[AlfPartialLERresults$YearCode == 'X',]
AlfPLERY<-AlfPartialLERresults[AlfPartialLERresults$YearCode == 'Y',]
AlfPLERZ<-AlfPartialLERresults[AlfPartialLERresults$YearCode == 'Z',]
AlfPLERXS<-AlfPLERX[AlfPLERX$Harvest == 'Summer',]
AlfPLERXF<-AlfPLERX[AlfPLERX$Harvest == 'Fall',]
AlfPLERYS<-AlfPLERY[AlfPLERY$Harvest == 'Summer',]
AlfPLERYF<-AlfPLERY[AlfPLERY$Harvest == 'Fall',]
AlfPLERZS<-AlfPLERZ[AlfPLERZ$Harvest == 'Summer',]
AlfPLERZF<-AlfPLERZ[AlfPLERZ$Harvest == 'Fall',]

#Check for outliers
identify_outliers(TotalLERresults[, 6]) #33 outliers 
identify_outliers(IWGPartialLERresults[, 6]) #46 outliers
identify_outliers(AlfPartialLERresults[, 6]) #18 outliers 

#rename variables in dataframes to make life easier 
# Function to add new columns to a dataframe
add_new_columns <- function(df) {
  df$N <- df$NRATE
  return(df)
}
# List of dataframes (replace these with your actual dataframes)
df_list <- list(TotLERXS, TotLERYS, TotLERZS, 
                TotLERXF, TotLERYF, TotLERZF, 
                IWGPLERXS, IWGPLERYS, IWGPLERZS, 
                IWGPLERXF, IWGPLERYF, IWGPLERZF, 
                AlfPLERXS, AlfPLERYS, AlfPLERZS,
                AlfPLERXF, AlfPLERYF, AlfPLERZF)
# Apply the function to each dataframe in the list
df_list <- lapply(df_list, add_new_columns)
# Access the modified dataframes in df_list
TotLERXS <- df_list[[1]]
TotLERYS <- df_list[[2]]
TotLERZS <- df_list[[3]]
TotLERXF <- df_list[[4]]
TotLERYF<- df_list[[5]]
TotLERZF<- df_list[[6]]
IWGPLERXS<- df_list[[7]]
IWGPLERYS<- df_list[[8]]
IWGPLERZS<- df_list[[9]]
IWGPLERXF<- df_list[[10]]
IWGPLERYF<- df_list[[11]]
IWGPLERZF<- df_list[[12]]
AlfPLERXS<- df_list[[13]]
AlfPLERYS<- df_list[[14]]
AlfPLERZS<- df_list[[15]]
AlfPLERXF<- df_list[[16]]
AlfPLERYF<- df_list[[17]]
AlfPLERZF<- df_list[[18]]

TotalLERresults$N<-TotalLERresults$NRATE
TotalLERresults$N<- as.numeric(TotalLERresults$N)
#linear
modTotLER<-lmer(LER~N*P*YearCode + (1|Rep:P), data= TotalLERresults)
anova(modTotLER)



# SW0, SW40, SW80, SW120, SW160
CRresultsNdSWALL <- LERresultsNdSWALL %>%
  select(Rep, P, YearCode, Harvest, PartialLERIWGSW0, PartialLERAlfSW0, PartialLERIWGSW40, PartialLERAlfSW40, PartialLERIWGSW80, PartialLERAlfSW80, PartialLERIWGSW120, PartialLERAlfSW120, PartialLERIWGSW160, PartialLERAlfSW160)%>%
  mutate(CRSW0 = PartialLERIWGSW0 / PartialLERAlfSW0)%>%
  mutate(CRSW40 = PartialLERIWGSW40 / PartialLERAlfSW40)%>%
  mutate(CRSW80 = PartialLERIWGSW80 / PartialLERAlfSW80)%>%
  mutate(CRSW120 = PartialLERIWGSW120 / PartialLERAlfSW120)%>%
  mutate(CRSW160 = PartialLERIWGSW160 / PartialLERAlfSW160)%>%
  select(Rep, P, YearCode, Harvest, CRSW0, CRSW40, CRSW80, CRSW120, CRSW160)

#Rearrange dataframe
CRresultsNdSWALL_long <- CRresultsNdSWALL %>%
  pivot_longer(cols = starts_with("CRSW"),
               names_to = "Nrate",
               values_to = "CR")

#try to make the dataframe more usable

#cut off total and partial 
CRresultsNdSWALL_long$Type <- sub("^(CR)", "", CRresultsNdSWALL_long$Nrate)
#LERresultsNdSWALL_long$tot_or_part <- sub("^(Partial|Total).*", "\\1", LERresultsNdSWALL_long$Nrate)

#cut off SW
CRresultsNdSWALL_long$NRATE <- sub("^(SW|SW|SW)", "", CRresultsNdSWALL_long$Type)
CRresultsNdSWALL_long$PlotType <- sub("^(CRSW|CRSW|CRSW).*", "\\1", CRresultsNdSWALL_long$Type)

#get rid of pesky NA results (caused by missing plots inherent in year 1 KS summer data)
CRresultsNdSWALL_longNONA<- na.omit(CRresultsNdSWALL_long)

CRresultsNdSWALL_longNONA$NRATE<-as.numeric(CRresultsNdSWALL_longNONA$NRATE)

#includes total and partial until it doesn't...used for some pairwise comparison thingy. 
CRResultsAllX<-CRresultsNdSWALL_longNONA[CRresultsNdSWALL_longNONA$YearCode == 'X',]
CRResultsAllY<-CRresultsNdSWALL_longNONA[CRresultsNdSWALL_longNONA$YearCode == 'Y',]
CRResultsAllZ<-CRresultsNdSWALL_longNONA[CRresultsNdSWALL_longNONA$YearCode == 'Z',]
CRResultsAllXS<-CRResultsAllX[CRResultsAllX$Harvest == 'Summer',]
CRResultsAllXF<-CRResultsAllX[CRResultsAllX$Harvest == 'Fall',]
CRResultsAllYS<-CRResultsAllY[CRResultsAllY$Harvest == 'Summer',]
CRResultsAllYF<-CRResultsAllY[CRResultsAllY$Harvest == 'Fall',]
CRResultsAllZS<-CRResultsAllZ[CRResultsAllZ$Harvest == 'Summer',]
CRResultsAllZF<-CRResultsAllZ[CRResultsAllZ$Harvest == 'Fall',]

#rename variables in dataframes to make life easier 
# Function to add new columns to a dataframe
add_new_columns <- function(df) {
  df$N <- df$NRATE
  return(df)
}

# List of dataframes (replace these with your actual dataframes)
df_list <- list(CRResultsAllXS, CRResultsAllXF, 
                CRResultsAllYS, CRResultsAllYF, 
                CRResultsAllZS, CRResultsAllZF)

# Apply the function to each dataframe in the list
df_list <- lapply(df_list, add_new_columns)
# Access the modified dataframes in df_list
CRXS <- df_list[[1]]
CRXF <- df_list[[2]]
CRYS <- df_list[[3]]
CRYF <- df_list[[4]]
CRZS <- df_list[[5]]
CRZF <- df_list[[6]]

CRresultsNdSWALL_longNONA$N<-CRresultsNdSWALL_longNONA$NRATE
CRresultsNdSWALL_longNONA$N<- as.numeric(CRresultsNdSWALL_longNONA$N)
#linear
modCR<-lmer(CR~N*P*YearCode + (1|Rep:P), data= CRresultsNdSWALL_longNONA)
anova(modCR)


