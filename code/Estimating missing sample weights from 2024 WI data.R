# Estimating missing IWG sample weights from 2024 WI data; ChatGPT prompt: use linear regression to find estimates of all missing data 
# Step 1: Create the dataset
data <- data.frame(
  T1 = c(251, 254, 254, 256, 267, 270, 275, 283, 284, 286, 286, 288, 288, 292, 293, 293,
         294, 296, 297, 297, 300, 302, 303, 305, 305, 309, 311, 313, 317, 320, 323, 325, 
         328, 329, 333, 335, 338, 339, 340, 348, 360, 360, 369, 370, 375, 378, 379, 385, 
         395, 400, 401, 410, 412, 426, 433, NA),
  T2 = c(96, 101, 98, 89, 92, 101, 106, 111, 101, NA, 117, 90, 123, 112, 125, 110, 86, 
         106, NA, 129, NA, 97, NA, 112, 114, 128, 132, 113, NA, 124, 132, 113, 131, 144, 
         117, 141, 144, 118, NA, 156, 140, 131, 138, 147, 138, 143, 170, 160, 140, NA, 
         132, 150, 182, 170, 167, 145)
)

# Step 2: Split into known and unknowns
known <- na.omit(data)
missing_T2 <- data[!is.na(data$T1) & is.na(data$T2), ]
missing_T1 <- data[is.na(data$T1) & !is.na(data$T2), ]

# Step 3: Linear regression to predict T2
model_T2 <- lm(T2 ~ T1, data = known)
predicted_T2 <- predict(model_T2, newdata = missing_T2)

# Step 4: Linear regression to predict T1
model_T1 <- lm(T1 ~ T2, data = known)
predicted_T1 <- predict(model_T1, newdata = missing_T1)

# Step 5: Fill in the missing values
data[which(is.na(data$T2) & !is.na(data$T1)), "T2"] <- predicted_T2
data[which(is.na(data$T1) & !is.na(data$T2)), "T1"] <- predicted_T1

# Step 6: View the completed dataset
data <- data[order(data$T1), ]
print(data)

