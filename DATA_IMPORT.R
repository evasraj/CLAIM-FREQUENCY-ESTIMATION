# DATA IMPORT

source("LIBRARIES.R")

dataL <- read.csv("L_Group_Data.csv")
dataT <- read.csv("T_Group_Data.csv")

data1 <- dataL

# categorical variables as factors
data1 <- within(data1, {
  F1 <- factor(F1)
  F3 <- factor(F3)
  F4 <- factor(F4)
  F5 <- factor(F5)
  F8 <- factor(F8)
  F9 <- factor(F9)
  F10 <- factor(F10)
  F11 <- factor(F11)
  F12 <- factor(F12)
  F13 <- factor(F13)
})

lapply(data1, class)

# dataT noted dataset from given T-Group.csv
# categorical variables as factors
dataT <- within(dataT, {
  F1 <- factor(F1)
  F3 <- factor(F3)
  F4 <- factor(F4)
  F5 <- factor(F5)
  F8 <- factor(F8)
  F9 <- factor(F9)
  F10 <- factor(F10)
  F11 <- factor(F11)
  F12 <- factor(F12)
  F13 <- factor(F13)
})

lapply(dataT, class)
