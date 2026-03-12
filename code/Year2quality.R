library(readxl)
library(dplyr)

YS_IWG_NIR <- read_excel("~/Desktop/YS_IWG_NIR.xlsx")

master <- YS_IWG_NIR %>%
  select(NumPlot) %>%     # select the column
  dplyr::rename(Plot = NumPlot)    # rename it

YS_IWG_NIR <- YS_IWG_NIR[,-1]

colnames(YS_IWG_NIR)[1] <- "Plot"

aligned <- master %>%
  left_join(YS_IWG_NIR, by = "Plot")

#write.csv(aligned, "data/Intermediate_Data/YS_IWG_NIR.csv", row.names = FALSE)

YS_ALF_NIR <- read_excel("~/Desktop/YS_ALF_NIR.xlsx")

master <- YS_ALF_NIR %>%
  select(NumPlot) %>%     # select the column
  dplyr::rename(Plot = NumPlot)    # rename it

YS_ALF_NIR <- YS_ALF_NIR[,-1]

colnames(YS_ALF_NIR)[1] <- "Plot"

aligned <- master %>%
  left_join(YS_ALF_NIR, by = "Plot")

#write.csv(aligned, "data/Intermediate_Data/YS_ALF_NIR.csv", row.names = FALSE)

YF_IWG_NIR <- read_excel("~/Desktop/YF_IWG_NIR.xlsx")

master <- YF_IWG_NIR %>%
  select(NumPlot) %>%     # select the column
  dplyr::rename(Plot = NumPlot)    # rename it

YF_IWG_NIR <- YF_IWG_NIR[,-1]

colnames(YF_IWG_NIR)[1] <- "Plot"

aligned <- master %>%
  left_join(YF_IWG_NIR, by = "Plot")

#write.csv(aligned, "data/Intermediate_Data/YF_IWG_NIR.csv", row.names = FALSE)

YF_ALF_NIR <- read_excel("~/Desktop/YF_ALF_NIR.xlsx")

master <- YF_ALF_NIR %>%
  select(NumPlot) %>%     # select the column
  dplyr::rename(Plot = NumPlot)    # rename it

YF_ALF_NIR <- YF_ALF_NIR[,-1]

colnames(YF_ALF_NIR)[1] <- "Plot"

aligned <- master %>%
  left_join(YF_ALF_NIR, by = "Plot")

#write.csv(aligned, "data/Intermediate_Data/YF_ALF_NIR.csv", row.names = FALSE)