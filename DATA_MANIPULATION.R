# DATA MANIPULATION

source("DATA_IMPORT.R")

# remove losses with the highest exposures (top 1%)
top_one_percent <- quantile(data1$EXPOSURE, .99)
top_one_percent

# data_drop = dataset without the most exposed policies
data_drop <- data1 %>%  filter(EXPOSURE < top_one_percent)
dim(data_drop)

# select categorical columns
factor <- data.frame(select_if(data_drop, is.factor))
ncol(factor)

# recast_data1 = dataset grouped by similar claim frequencies
# columns F1, F5, F10, F11, F12 are OK
recast_data1 <- data_drop

recast_data1 <- recast_data1 %>% # F1 = {G0, G1, G2, G3, G4}
  mutate(F3 = factor(ifelse(F3 == "1" , "G0", ifelse(F3 == "2" | F3 == "3"  | F3 == "4"  | F3 == "5"  |
                                                       F3 == "6" | F3 == "7" | F3 == "8", "G1", 
                                                     ifelse(F3 == "9" |F3 == "10"| F3 == "11", "G2", 
                                                            ifelse(F3 == "12" |F3 == "13",  "G3", "G4"))))))

recast_data1 <- recast_data1 %>% # F3 = {B0, B1, B2, B3}
  mutate(F4 = factor(ifelse(F4 == "0" | F4 == "20" , "B1", ifelse(F4 == "8" | F4 == "14", "B3", "B2"))))

recast_data1 <- recast_data1 %>% # F8 = {C, H, M}
  mutate(F8 = factor(ifelse(F8 == "M1" | F8 == "M2" , "M", ifelse(F8 == "H1" | F8 == "H2", "H", "C"))))

recast_data1 <- recast_data1 %>% # F9 = {S0, S1_3, S5}
  mutate(F9 = factor(ifelse(F9 == "S1" | F9 == "S2" | F9 == "S3" , "S1_3", ifelse(F9 == "S0", "S0", "S5"))))

recast_data1 <- recast_data1 %>% # F13 = {F0_1, F2_4, F5}
  mutate(F13 = factor(ifelse(F13  == "0" | F13 == "1" , "F0_1", ifelse(F13 == "5", "F5", "F2_4"))))

# training1 = training dataset constructed by bootstrap (80%) 
# testing1 = testing dataset (20%)

set.seed(1234) # generates the same partition
data_partition1 <- createDataPartition(data_drop$CLAIMNO, times = 1,p = 0.8,list = FALSE)
training1 <- data_drop[data_partition1,] # 80% of data_drop
testing1  <- data_drop[-data_partition1,] # 20% of data_drop


