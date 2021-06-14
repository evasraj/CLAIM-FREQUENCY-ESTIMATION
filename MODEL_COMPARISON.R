# MODEL COMPARISON

source("DATA_DESCRIPTION.R")
# source("LOAD_MODELS.R")

## POISSON REGRESION

# data1
poisson_glm_baseRMSE <- rmse(poisson_glm_base$fitted.values, data1$CLAIMNO)
poisson_glm_baseRMSE

poisson_glm_baseAIC <- poisson_glm_base$aic
poisson_glm_baseAIC

# Test for dispersion
poisson_glm_baseDISPERSION <- dispersiontest(poisson_glm_base,trafo=1)
poisson_glm_baseDISPERSION
poisson_glm_baseALPHA <- poisson_glm_baseDISPERSION$estimate
poisson_glm_baseALPHA

# drop_data
poisson_glm_dropRMSE <- rmse(poisson_glm_drop$fitted.values, data_drop$CLAIMNO)
poisson_glm_dropRMSE

poisson_glm_dropAIC <- poisson_glm_drop$aic
poisson_glm_dropAIC

# Test for dispersion
poisson_glm_dropDISPERSION <- dispersiontest(poisson_glm_drop,trafo=1)
poisson_glm_dropDISPERSION
poisson_glm_dropALPHA <- poisson_glm_dropDISPERSION$estimate
poisson_glm_dropALPHA

# recast_data1
poisson_glm_recast1RMSE <- rmse(poisson_glm_recast1$fitted.values, recast_data1$CLAIMNO)
poisson_glm_recast1RMSE

poisson_glm_recast1AIC <- poisson_glm_recast1$aic
poisson_glm_recast1AIC

# Test for dispersion
poisson_glm_recast1DISPERSION <- dispersiontest(poisson_glm_recast1,trafo=1)
poisson_glm_recast1DISPERSION
poisson_glm_recast1ALPHA <- poisson_glm_recast1DISPERSION$estimate
poisson_glm_recast1ALPHA

# recast_data1 + groupiranje = revelel
summary(poisson_glm_relevel)

poisson_glm_relevelRMSE <- rmse(poisson_glm_relevel$fitted.values, recast_data1$CLAIMNO)
poisson_glm_relevelRMSE

poisson_glm_relevelAIC <- poisson_glm_relevel$aic
poisson_glm_relevelAIC

# Test for dispersion
poisson_glm_relevelDISPERSION <- dispersiontest(poisson_glm_relevel,trafo=1)
poisson_glm_relevelDISPERSION
poisson_glm_relevelALPHA <- poisson_glm_relevelDISPERSION$estimate
poisson_glm_relevelALPHA

# training1
poisson_glm_train1RMSE <- rmse(poisson_glm_train1$fitted.values, training1$CLAIMNO)
poisson_glm_train1RMSE

poisson_glm_train1AIC <- poisson_glm_train1$aic
poisson_glm_train1AIC

# Test for dispersion
poisson_glm_train1DISPERSION <- dispersiontest(poisson_glm_train1,trafo=1)
poisson_glm_train1DISPERSION
poisson_glm_train1ALPHA <- poisson_glm_train1DISPERSION$estimate
poisson_glm_train1ALPHA

# training1 + INTERAKCIJA
INTER_poisson_glm_train1RMSE <- rmse(INTER_poisson_glm_train1$fitted.values, training1$CLAIMNO)
INTER_poisson_glm_train1RMSE

INTER_poisson_glm_train1AIC <- INTER_poisson_glm_train1$aic
INTER_poisson_glm_train1AIC

# Test for dispersion
INTER_poisson_glm_train1DISPERSION <- dispersiontest(INTER_poisson_glm_train1,trafo=1)
INTER_poisson_glm_train1DISPERSION
INTER_poisson_glm_train1ALPHA <- INTER_poisson_glm_train1DISPERSION$estimate
INTER_poisson_glm_train1ALPHA

# table for Poisson regression

POIS_base <- c(poisson_glm_baseRMSE, poisson_glm_baseAIC, poisson_glm_baseDISPERSION$p.value, as.numeric(poisson_glm_baseALPHA))
POIS_drop <- c(poisson_glm_dropRMSE, poisson_glm_dropAIC, poisson_glm_dropDISPERSION$p.value, as.numeric(poisson_glm_dropALPHA))
POIS_relevel <- c(poisson_glm_relevelRMSE, poisson_glm_relevelAIC, poisson_glm_relevelDISPERSION$p.value, as.numeric(poisson_glm_relevelALPHA))
POIS_recast1 <- c(poisson_glm_recast1RMSE, poisson_glm_recast1AIC, poisson_glm_recast1DISPERSION$p.value, as.numeric(poisson_glm_recast1ALPHA))
POIS_train1 <- c(poisson_glm_train1RMSE, poisson_glm_train1AIC, poisson_glm_train1DISPERSION$p.value, as.numeric(poisson_glm_train1ALPHA))
INTER_POIS_train1 <- c(INTER_poisson_glm_train1RMSE, INTER_poisson_glm_train1AIC, INTER_poisson_glm_train1DISPERSION$p.value, as.numeric(INTER_poisson_glm_train1ALPHA))

POISSON <- round(data.frame(POIS_base, POIS_drop,POIS_recast1,POIS_relevel,POIS_train1),3)
row.names(POISSON) <- c("RMSE", "AIC", "p-value for Over-dispersion", "alpha")

POISSON1 <- round(data.frame(POIS_train1, INTER_POIS_train1),3)
row.names(POISSON1) <- c("RMSE", "AIC", "p-value for Over-dispersion", "alpha")

# GRAPHS FROM https://katrienantonio.github.io/Risk-modelling-in-insurance/glms.html

# ZELO SLABI GRAFI...

plot(poisson_glm_base$fitted.values, data1$CLAIMNO, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(poisson_glm_base$fitted ~ CLAIMNO, data=data1), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

plot(poisson_glm_drop$fitted.values, data_drop$CLAIMNO, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(poisson_glm_drop$fitted ~ CLAIMNO, data=data_drop), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

plot(poisson_glm_recast1$fitted.values, recast_data1$CLAIMNO, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(poisson_glm_recast1$fitted ~ CLAIMNO, data=recast_data1), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)


## NEGATIVE BINOMIAL REGRESSION with offset

# data1
negbin_glm_baseRMSE <- rmse(negbin_glm_base$fitted.values, data1$CLAIMNO)
negbin_glm_baseRMSE

negbin_glm_baseAIC <- negbin_glm_base$aic
negbin_glm_baseAIC

# drop_data
negbin_glm_dropRMSE <- rmse(negbin_glm_drop$fitted.values, data_drop$CLAIMNO)
negbin_glm_dropRMSE

negbin_glm_dropAIC <- negbin_glm_drop$aic
negbin_glm_dropAIC

# recast_data1
negbin_glm_recast1RMSE <- rmse(negbin_glm_recast1$fitted.values, recast_data1$CLAIMNO)
negbin_glm_recast1RMSE

negbin_glm_recast1AIC <- negbin_glm_recast1$aic
negbin_glm_recast1AIC

# training1
negbin_glm_train1RMSE <- rmse(negbin_glm_train1$fitted.values, training1$CLAIMNO)
negbin_glm_train1RMSE

negbin_glm_train1AIC <- negbin_glm_train1$aic
negbin_glm_train1AIC

# training1 + INTERAKCIJA
INTER_negbin_glm_train1RMSE <- rmse(INTER_negbin_glm_train1$fitted.values, training1$CLAIMNO)
INTER_negbin_glm_train1RMSE

INTER_negbin_glm_train1AIC <- INTER_negbin_glm_train1$aic
INTER_negbin_glm_train1AIC

# https://stackoverflow.com/questions/50099880/negative-binomial-model-with-interaction-in-r
AIC(negbin_glm_train1, INTER_negbin_glm_train1)

# table for Negative Binomial regression data

NEGBIN_base <- c(negbin_glm_baseRMSE, negbin_glm_baseAIC)
NEGBIN_drop <- c(negbin_glm_dropRMSE, negbin_glm_dropAIC)
NEGBIN_recast1 <- c(negbin_glm_recast1RMSE, negbin_glm_recast1AIC)
NEGBIN_train1 <- c(negbin_glm_train1RMSE, negbin_glm_train1AIC)
ITER_NEGBIN_train1 <- c(INTER_negbin_glm_train1RMSE, INTER_negbin_glm_train1AIC)

NEGBIN <- round(data.frame(NEGBIN_base, NEGBIN_drop,NEGBIN_recast1,NEGBIN_train1,ITER_NEGBIN_train1),3)
row.names(NEGBIN) <- c("RMSE", "AIC")

# NAJBOJLŠI je NEGBIN_train1

lrtest(poisson_glm_train1, negbin_glm_train1)

# ZERO-INFLATION POISSON MODEL with offset

# recast_data1
ZIpoisson_recast1RMSE <- rmse(ZIpoisson_recast1$fitted.values, recast_data1$CLAIMNO)
ZIpoisson_recast1RMSE

ZIpoisson_recast1LOGLIK <- ZIpoisson_recast1$loglik
ZIpoisson_recast1LOGLIK

# training1
ZIpoisson_train1RMSE <- rmse(ZIpoisson_train1$fitted.values, training1$CLAIMNO)
ZIpoisson_train1RMSE

ZIpoisson_train1LOGLIK <- ZIpoisson_train1$loglik
ZIpoisson_train1LOGLIK

# training1 + INTERAKCIJA
INTER_ZI.pois_glm_train1RMSE <- rmse(INTER_ZI.pois_glm_train1$fitted.values, training1$CLAIMNO)
INTER_ZI.pois_glm_train1RMSE

INTER_ZI.pois_glm_train1LOGLIK <- INTER_ZI.pois_glm_train1$loglik
INTER_ZI.pois_glm_train1LOGLIK

# table for Zero-Inflated Poisson regression data

ZI.pois_recast1 <- c(ZIpoisson_recast1RMSE, ZIpoisson_recast1LOGLIK)
ZI.pois_train1 <- c(ZIpoisson_train1RMSE, ZIpoisson_train1LOGLIK)
INTER_ZI.pois_train1 <- c(INTER_ZI.pois_glm_train1RMSE, INTER_ZI.pois_glm_train1LOGLIK)

ZI.POIS <- round(data.frame(ZI.pois_recast1,ZI.pois_train1, INTER_ZI.pois_train1),3)
row.names(ZI.POIS) <- c("RMSE", "LOGLIK")

# NAJBOJLŠI je ZI.pois_train1

# ZIpoisson_train1RMSE JE NAJBOJLŠI!

#ZERO-INFLATED NEGATIVE BINOMIAL MODEL with offset

# recast_data1
ZInegbin_recast1RMSE <- rmse(ZInegbin_recast1$fitted.values, recast_data1$CLAIMNO)
ZInegbin_recast1RMSE

ZInegbin_recast1LOGLIK <- ZInegbin_recast1$loglik
ZInegbin_recast1LOGLIK

# training1
ZInegbin_train1RMSE <- rmse(ZInegbin_train1$fitted.values, training1$CLAIMNO)
ZInegbin_train1RMSE

ZInegbin_train1LOGLIK <- ZInegbin_train1$loglik
ZInegbin_train1LOGLIK

# training1 + INTERAKCIJA
INTER_ZInegbin_glm_train1RMSE <- rmse(INTER_ZInegbin_glm_train1$fitted.values, training1$CLAIMNO)
INTER_ZInegbin_glm_train1RMSE

INTER_ZInegbin_glm_train1LOGLIK <- INTER_ZInegbin_glm_train1$loglik
INTER_ZInegbin_glm_train1LOGLIK

# table for Zero-Iflated Negative Binomial regression data

ZI.negbin_recast1 <- c(ZInegbin_recast1RMSE, ZInegbin_recast1LOGLIK)
ZI.negbin_train1 <- c(ZInegbin_train1RMSE, ZInegbin_train1LOGLIK)
INTER_ZI.negbin_train1 <- c(INTER_ZInegbin_glm_train1RMSE, INTER_ZInegbin_glm_train1LOGLIK)

ZI.NEGBIN <- round(data.frame(ZI.negbin_recast1,ZI.negbin_train1,INTER_ZI.negbin_train1),3)
row.names(ZI.NEGBIN) <- c("RMSE", "LOGLIK")

# ZInegbin_train1 is the best!

ZI_models <- round(data.frame(ZI.pois_recast1, ZI.pois_train1, ZI.negbin_recast1, ZI.negbin_train1),3)
row.names(ZI_models) <- c("RMSE", "LOGLIK")


# HURDLE POISSON MODEL with offset

# recast_data1
HURDLE_poisson_recast1RMSE <- rmse(HURDLE_poisson_recast1$fitted.values, recast_data1$CLAIMNO)
HURDLE_poisson_recast1RMSE

HURDLE_poisson_recast1LOGLIK <- HURDLE_poisson_recast1$loglik
HURDLE_poisson_recast1LOGLIK

# training1
HURDLE_poisson_train1RMSE <- rmse(HURDLE_poisson_train1$fitted.values, training1$CLAIMNO)
HURDLE_poisson_train1RMSE

HURDLE_poisson_train1LOGLIK <- HURDLE_poisson_train1$loglik
HURDLE_poisson_train1LOGLIK

# table for Hurdle Poisson regression data

HU.pois_recast1 <- c(HURDLE_poisson_recast1RMSE, HURDLE_poisson_recast1LOGLIK)
HU.pois_train1 <- c(HURDLE_poisson_train1RMSE, HURDLE_poisson_train1LOGLIK)

HURDLE.POISSON <- round(data.frame(HU.pois_recast1,HU.pois_train1),3)
row.names(HURDLE.POISSON) <- c("RMSE", "LOGLIK")


# HURDLE NEGATIVE BINOMIAL MODEL with offset

# recast_data1
HURDLE_negbin_recast1RMSE <- rmse(HURDLE_negbin_recast1$fitted.values, recast_data1$CLAIMNO)
HURDLE_negbin_recast1RMSE

HURDLE_negbin_recast1LOGLIK <- HURDLE_negbin_recast1$loglik
HURDLE_negbin_recast1LOGLIK

# training1
HURDLE_negbin_train1RMSE <- rmse(HURDLE_negbin_train1$fitted.values, training1$CLAIMNO)
HURDLE_negbin_train1RMSE

HURDLE_negbin_train1LOGLIK <- HURDLE_negbin_train1$loglik
HURDLE_negbin_train1LOGLIK

# table for Hurdle Negative Binomial regression data

HU.negbin_recast1 <- c(HURDLE_negbin_recast1RMSE, HURDLE_negbin_recast1LOGLIK)
HU.negbin_train1 <- c(HURDLE_negbin_train1RMSE, HURDLE_negbin_train1LOGLIK)

HURDLE.NEGBIN <- round(data.frame(HU.negbin_recast1,HU.negbin_train1),3)
row.names(HURDLE.NEGBIN) <- c("RMSE", "LOGLIK")

# HURDLE_negbin_train1 is the best :)

HURDLE_models <- round(data.frame(ZI.pois_recast1, ZI.pois_train1, HU.negbin_recast1,HU.negbin_train1),3)
row.names(HURDLE_models) <- c("RMSE", "LOGLIK")

# -> training1 is the best data set :)

# Codes to predict zero claims:
zero_counts <- data.frame(round(c("observations" = sum(training1$CLAIMNO < 1),
                                  "Poisson_glm" = sum(exp(-predict(poisson_glm_train1, training1, type = "response"))),
                                  "NegBin_glm" = sum(dnbinom(0, mu = fitted(negbin_glm_train1), size = negbin_glm_train1$theta)),
                                  "ZeroInflatedPois" = sum(predict(ZIpoisson_train1, training1,type = "prob")[,1]),
                                  "ZeroInflatedNegBin" = sum(predict(ZInegbin_train1, training1,type = "prob")[,1]),
                                  "HurdlePoisson" = sum(predict(HURDLE_poisson_train1, training1, type = "prob")[,1]),
                                  "HurdleNegPoisson" = sum(predict(HURDLE_negbin_train1, training1,type = "prob")[,1]))))
names(zero_counts) <- c("Number of Zero Claims")
zero_counts$"delta" <- zero_counts$`Number of Zero Claims` - rep(zero_counts[1,1], nrow(zero_counts))



# https://fukamilab.github.io/BIO202/04-C-zero-data.html
# https://api.rpubs.com/tomanderson_34/lrt
# library(lmtest)
# LR test compares 2 models

ZIpoisson_train1LOGLIK
ZIpoisson_train1DFnull <- ZIpoisson_train1$df.null
ZIpoisson_train1DFres <- ZIpoisson_train1$df.residual
ZIpoisson_train1DF <- ZIpoisson_train1DFnull - ZIpoisson_train1DFres

ZIpoisson_recast1LOGLIK
ZIpoisson_recast1DFnull <- ZIpoisson_recast1$df.null
ZIpoisson_recast1DFres <- ZIpoisson_recast1$df.residual
ZIpoisson_recast1DF <- ZIpoisson_recast1DFnull - ZIpoisson_recast1DFres

test.stat_ZIpoiss_train <- -2* (ZIpoisson_recast1LOGLIK - ZIpoisson_train1LOGLIK)
p.value_ZIpoiss_train <- pchisq(test.stat_ZIpoiss_train, df = ZIpoisson_train1DF - ZIpoisson_recast1DF, lower.tail = FALSE) 
p.value_ZIpoiss_train

lrtest(ZIpoisson_train1, ZInegbin_train1)

lrtest(ZInegbin_train1, ZIpoisson_train1)

lrtest(HURDLE_poisson_train1, HURDLE_negbin_train1)

# lrtest(ZInegbin_train1, HURDLE_negbin_train1)
# original model was of class "zeroinfl", updated model is of class "hurdle"

LR_testZI <- lrtest(ZIpoisson_train1,ZInegbin_train1)
LR_testZI
# ZInegbin_train1 je boljši

LR_testHU <- lrtest(HURDLE_poisson_train1,HURDLE_negbin_train1)
LR_testHU
# HURDLE_poisson_train1 je boljši

#LR_testZI.HU <- lrtest(ZIpoisson_train1,HURDLE_poisson_train1)
#LR_testZI.HU



library(countreg)
#par(mfrow = c(1, 2))
rootogram(poisson_glm_train1,max = 15,main="Poisson") # fit up to count 15
rootogram(negbin_glm_train1,max = 15,main="NegBin") # fit up to count 15
#par(mfrow = c(1, 1))
rootogram(ZIpoisson_train1,max = 15,main="ZI-Poisson") # fit up to count 15
rootogram(ZInegbin_train1,max = 15,main="ZI-NegBin") # fit up to count 10
#par(mfrow = c(1, 1))
rootogram(HURDLE_poisson_train1,max = 15,main="Hurdle-P")# fit up to count 15
rootogram(HURDLE_negbin_train1,max = 15,main="Hurdle-NB") # fit up to count 10
#par(mfrow = c(1, 1))

#Log likelyhood for all the models
models <- list("Pois" = poisson_glm_train1,
               "NegBin" = negbin_glm_train1, 
               "ZeroInflatedPois" = ZIpoisson_train1,
               "ZeroInflatedNegBin" = ZInegbin_train1,
               "HurdlePois" = HURDLE_poisson_train1,
               "Hurdle-NB" = HURDLE_negbin_train1)
table_LOGLIK_models_all <- data.frame(rbind(logLik = sapply(models, function(x) round(logLik(x), digits = 0)),
                                            X = sapply(models, function(x) round(-2*logLik(x), digits = 0)),
                                            Df = sapply(models, function(x) attr(logLik(x), "df"))))


library(MASS)

# POISSON: training1
estimat_poisson_glm_train1 <- fitted(poisson_glm_train1)
summary(estimat_poisson_glm_train1)
hist(estimat_poisson_glm_train1, breaks = 100, col="grey", main="Histogram of fitted mean values", xlab="Fitted mean values")
residuals_poisson_glm_train1 <-residuals(poisson_glm_train1)
summary(residuals_poisson_glm_train1)
hist(residuals_poisson_glm_train1, col="grey")
plot(density(residuals_poisson_glm_train1), xlab="Residuals", ylab="",main="", col = "red")

# NEGBIN: training1
estimat_negbin_glm_train1 <- fitted(negbin_glm_train1)
summary(estimat_negbin_glm_train1)
hist(estimat_negbin_glm_train1, breaks = 100, col="grey", main="Histogram of fitted mean values", xlab="Fitted mean values")
residuals_negbin_glm_train1 <-residuals(negbin_glm_train1)
summary(residuals_negbin_glm_train1)
hist(residuals_negbin_glm_train1, col="grey")
plot(density(residuals_negbin_glm_train1), xlab="Residuals", ylab="",main="", col = "red")

plot(density(residuals_poisson_glm_train1), xlab="Residuals", ylab="",main="", col = "darkgreen")
lines(density(residuals_negbin_glm_train1), col = "blue")

# GRAFI ZA NEG BIN!

residuals_negbin_glm_train1 <- residuals(negbin_glm_train1)
summary(residuals_negbin_glm_train1)
hist(residuals_negbin_glm_train1, col="grey")
plot(density(residuals_negbin_glm_train1), xlab="Residuals", ylab="",
     main="")

