# OUTPUT

source("MODEL_COMPARISON.R")

predict_ZInegbin_train1 <- predict(ZInegbin_train1, dataT, type = "response")
RESULT <- cbind(dataT, "Predicted_number_of_claims" = round(predict_ZInegbin_train1,0))

write.csv(RESULT, "RESULT.csv", row.names = FALSE)

RESULT <- within(RESULT, {
  F1 <- character(F1)
  F3 <- character(F3)
  F4 <- character(F4)
  F5 <- character(F5)
  F8 <- character(F8)
  F9 <- character(F9)
  F10 <- character(F10)
  F11 <- character(F11)
  F12 <- character(F12)
  F13 <- character(F13)
  EXPOSURE <- character(EXPOSURE)
  Predicted_number_of_claims <- integer(Predicted_number_of_claims)
})

lapply(RESULT, class)
