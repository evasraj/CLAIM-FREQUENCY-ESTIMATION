# DATA DESCRIPTION

source("DATA_MANIPULATION.R")

summary(data1)

# table data1_ind includes not only data1 but also a column Claim Occurence Indicator
claims <- which(data1$CLAIMNO > 0)
data1_ind <- data1
data1_ind$indicator <- 0
data1_ind$indicator[claims] <- 1

data1_ind$indicator <- factor(data1_ind$indicator, labels=c("No claim","At least one claim"))

# graph of Claim occurence indicator
set_theme(base = theme_light())
plot_frq(data1_ind$indicator,geom.colors = "orangered") +
  labs(y="Number of Policies", x="") +
  ggtitle("Claim Occurence Indicator") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=0, vjust=0.5, colour = "black"), 
        axis.text.y = element_text(angle=0, vjust=0.5, colour = "black"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black")) 

# graph of claim count distribution (on data1)
ggplot(data1, aes(x=CLAIMNO)) + 
  geom_bar(fill = "orangered", colour = "black", width = 1) +
  labs(y="Number of Policies", x="Number of Claims") +
  ggtitle("Claim Count")+
  theme_classic() +
  theme(axis.text.x = element_text(angle=0, vjust=0.5, colour = "black"), 
        axis.text.y = element_text(angle=0, vjust=0.5, colour = "black"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black")) 

# graph of claim count distribution (on data_drop)
ggplot(data_drop, aes(x=CLAIMNO)) + 
  geom_bar(fill = "orangered", colour = "black", width = 1) +
  labs(y="Number of Policies", x="Number of Claims") +
  ggtitle("Claim Count")+
  theme_classic() +
  theme(axis.text.x = element_text(angle=0, vjust=0.5, colour = "black"), 
        axis.text.y = element_text(angle=0, vjust=0.5, colour = "black"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black")) 

# construct a table with number of claims, number of policies and total exposure
claims <- unique(sort(data1$CLAIMNO))
number_of_policies <- c()
expo <- c()
for(i in 1:length(claims)){
  number_of_policies[i] = sum(data1$CLAIMNO == claims[i])
  expo[i] = sum(data1[data1$CLAIMNO == claims[i],]$EXPOSURE)
}

table_claims_exposure <- data.frame(claims, number_of_policies, expo)
names(table_claims_exposure) <- c("Number of Claims", "Number of Policies", "Total Exposure")

# INDEPENDENCE DATA & correlation among variables (F1-F13)

# convert data1 (only the categorical variables) to numeric
corr <- data.frame(lapply(data1[,1:10], as.integer))
# plot the graph of correlation
ggcorr(corr,method = c("pairwise", "spearman"), nbreaks = 6, hjust = 0.8, label = TRUE, 
       label_size = 3, color = "grey50")
# correlation F4 & F13 = -0.5

# convert data1 (with all categorical variables, exposure and number of claims) to numeric
CORR <- data.frame(lapply(data1, as.integer))
# plot the graph of correlation
ggcorr(CORR,method = c("pairwise", "spearman"), nbreaks = 6, hjust = 0.8, label = TRUE, 
       label_size = 3, color = "grey50")
# correlation F4 & F13 = -0.5

# Risk classification: Possible interaction between F4 and F13?
combo.claims <- xtabs(data1$CLAIMNO ~ data1$F13 + data1$F4) # upper triangular matrix
combo.expo <- xtabs(data1$EXPOSURE ~ data1$F13 + data1$F4) 
claimFrequency.combo <- combo.claims/combo.expo

par(mfrow=c(1,1))
barplot(combo.claims,xlab="Category F4",
        main="Policyholders - Interaction F4 and F13",
        ylab="Number of policyholders",
        beside=TRUE,legend = rownames(claimFrequency.combo),
        args.legend=list(title="F13"))
barplot(claimFrequency.combo,xlab="Category F4",
        main="Claim Frequency - Interaction F4 and F13",
        ylab="Annual claim frequency",
        beside=TRUE,legend = rownames(claimFrequency.combo),
        args.legend=list(title="F13"))

# copy categorical covariates to separate variables
freq <- table(data1$CLAIMNO)
yearlyExposure <- xtabs(data1$EXPOSURE ~ data1$CLAIMNO)

weights <- as.numeric(names(freq))
meanFreq <- as.numeric((freq %*% weights)/sum(data1$EXPOSURE))

# Annual claim frequency
meanFreq

# Risk classification for different classes of F1, F3, F4, F5, F8, F9, F10, F11, F12, F13
F1.claims <- xtabs(data1$CLAIMNO ~ data1$F1)
F1.expo <- xtabs(data1$EXPOSURE ~ data1$F1)
F3.claims <- xtabs(data1$CLAIMNO ~ data1$F3)
F3.expo <- xtabs(data1$EXPOSURE ~ data1$F3)
F4.claims <- xtabs(data1$CLAIMNO ~ data1$F4)
F4.expo <- xtabs(data1$EXPOSURE ~ data1$F4)
F5.claims <- xtabs(data1$CLAIMNO ~ data1$F5)
F5.expo <- xtabs(data1$EXPOSURE ~ data1$F5)
F8.claims <- xtabs(data1$CLAIMNO ~ data1$F8)
F8.expo <- xtabs(data1$EXPOSURE ~ data1$F8)
F9.claims <- xtabs(data1$CLAIMNO ~ data1$F9)
F9.expo <- xtabs(data1$EXPOSURE ~ data1$F9)
F10.claims <- xtabs(data1$CLAIMNO ~ data1$F10)
F10.expo <- xtabs(data1$EXPOSURE ~ data1$F10)
F11.claims <- xtabs(data1$CLAIMNO ~ data1$F11)
F11.expo <- xtabs(data1$EXPOSURE ~ data1$F11)
F12.claims <- xtabs(data1$CLAIMNO ~ data1$F12)
F12.expo <- xtabs(data1$EXPOSURE ~ data1$F12)
F13.claims <- xtabs(data1$CLAIMNO ~ data1$F13)
F13.expo <- xtabs(data1$EXPOSURE ~ data1$F13)


# Annualized claimfrequency per class
claimFrequency.F1 <- F1.claims/F1.expo
claimFrequency.F3 <- F3.claims/F3.expo
claimFrequency.F4 <- F4.claims/F4.expo
claimFrequency.F5 <- F5.claims/F5.expo
claimFrequency.F8 <- F8.claims/F8.expo
claimFrequency.F9 <- F9.claims/F9.expo
claimFrequency.F10 <- F10.claims/F10.expo
claimFrequency.F11 <- F11.claims/F11.expo
claimFrequency.F12 <- F12.claims/F12.expo
claimFrequency.F13 <- F13.claims/F13.expo

# graphs of Annualized claimfrequency
coll <- c(1,2,3,4,5)
layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5), 
              2, 6, byrow = TRUE))
barplot(claimFrequency.F1,xlab="F1",
        main="Claim Frequency (F1)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[1])
barplot(claimFrequency.F3,xlab="F3",
        main="Claim Frequency (F3)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[2])
barplot(claimFrequency.F4,xlab="F4",
        main="Claim Frequency (F4)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[3])
barplot(claimFrequency.F5,xlab="F5",
        main="Claim Frequency (F5)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[4])
barplot(claimFrequency.F8,xlab="F8",
        main="Claim Frequency (F8)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[5])
coll <- c(6,7,8,9,10)
layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5), 
              2, 6, byrow = TRUE))
barplot(claimFrequency.F9,xlab="F9",
        main="Claim Frequency (F9)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[1])
barplot(claimFrequency.F10,xlab="F10",
        main="Claim Frequency (F10)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[2])
barplot(claimFrequency.F11,xlab="F11",
        main="Claim Frequency (F11)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[3])
barplot(claimFrequency.F12,xlab="F12",
        main="Claim Frequency (F12)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[4])
barplot(claimFrequency.F13,xlab="F13",
        main="Claim Frequency (F13)",
        ylab="Annual claim frequency",
        beside=TRUE,col=coll[5])

