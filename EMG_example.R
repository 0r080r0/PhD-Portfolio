# EMG Data Analysis Example

# install packages
install.packages("biosignalEMG")

# load libraries
library(biosignalEMG)
 
# import EMG data (extracted features & labeled)
df <- read.csv("Downloads/EMG_data/extracted_features_labeled/emg_all_features_labeled.csv")
head(df)
View(df)

# 