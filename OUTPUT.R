# OUTPUT

source("MODEL_COMPARISON.R")

predict_ZInegbin_train1 <- predict(ZInegbin_train1, dataT, type = "response")
RESULT <- cbind(dataT, "Predicted_number_of_claims" = round(predict_ZInegbin_train1,0))

write.csv(RESULT, "RESULT.csv", row.names = FALSE, quote = F)
