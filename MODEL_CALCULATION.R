# MODEL CALCULATION

source("DATA_MANIPULATION.R")

## POISSON REGRESION

# data1
poisson_glm_base <- glm(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13, 
                        data=data1, family = "poisson", offset=log(EXPOSURE))

# drop_data
poisson_glm_drop <- glm(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13, data=data_drop,
                        family = "poisson", offset=log(EXPOSURE))

# recast_data1
poisson_glm_recast1 <- glm(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13, family=poisson(link="log"), 
                           data=recast_data1, offset=log(EXPOSURE))

# recast_data1 + grouping = revelel
poisson_glm_relevel <- glm(CLAIMNO ~ relevel(F1,"F") + relevel(F3,"G1") + relevel(F4,"B2") + relevel(F5,"2") + 
                             relevel(F8,"M") + relevel(F9,"S1_3") + relevel(F10,"M1") + relevel(F11,"L2") +
                             relevel(F12,"D") + relevel(F13,"F0_1"), family=poisson(link="log"), 
                           data=recast_data1, offset=log(EXPOSURE))

# training1
poisson_glm_train1 <- glm(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13, family=poisson(link="log"), 
                          data=training1, offset=log(EXPOSURE))

# training1 + INTERAKCIJA
INTER_poisson_glm_train1 <- glm(CLAIMNO ~ F1 + F3 + F4*F13 + F5 + F8 + F9 + F10 + F11 + F12, family=poisson(link="log"), 
                                data=training1, offset=log(EXPOSURE))

## NEGATIVE BINOMIAL REGRESSION with offset

# data1
negbin_glm_base <- glm.nb(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 + offset(log(EXPOSURE)),
                          data=data1)

# drop_data
negbin_glm_drop <- glm.nb(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 + offset(log(EXPOSURE)),
                          data=data_drop)
# recast_data1
negbin_glm_recast1 <- glm.nb(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 + offset(log(EXPOSURE)),
                             data=recast_data1)

# training1
negbin_glm_train1 <- glm.nb(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 + offset(log(EXPOSURE)),
                            data=training1)

# training1 + INTERAKCIJA
INTER_negbin_glm_train1 <- glm.nb(CLAIMNO ~ F1 + F3 + F4*F13 + F5 + F8 + F9 + F10 + F11 + F12 + offset(log(EXPOSURE)),
                                  data=training1)

# ZERO-INFLATION POISSON MODEL with offset

# recast_data1
ZIpoisson_recast1 <- zeroinfl(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                              offset=log(EXPOSURE),data=recast_data1,dist = "poisson",link= "logit")

# training1
ZIpoisson_train1 <- zeroinfl(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                             offset=log(EXPOSURE),data=training1,dist = "poisson",link= "logit")

# training1 + INTERAKCIJA
INTER_ZI.pois_glm_train1 <- zeroinfl(CLAIMNO ~ F1 + F3 + F4*F13 + F5 + F8 + F9 + F10 + F11 + F12 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                                     data=training1,dist = "poisson",link= "logit")

#ZERO-INFLATED NEGATIVE BINOMIAL MODEL with offset

# recast_data1
ZInegbin_recast1 <- zeroinfl(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                             offset=log(EXPOSURE),data=recast_data1,dist = "negbin",link= "logit")

# training1
ZInegbin_train1 <- zeroinfl(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                            offset=log(EXPOSURE),data=training1,dist = "negbin",link= "logit")

# training1 + INTERAKCIJA
INTER_ZInegbin_glm_train1 <- zeroinfl(CLAIMNO ~ F1 + F3 + F4*F13 + F5 + F8 + F9 + F10 + F11 + F12 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 +F13,
                                      data=training1, dist = "negbin",link= "logit")

# HURDLE POISSON MODEL with offset

# recast_data1
HURDLE_poisson_recast1 <- hurdle(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                                 offset=log(EXPOSURE),data=recast_data1,dist ="poisson",zero.dist = "poisson",link= "logit")

# training1
HURDLE_poisson_train1 <- hurdle(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                                offset=log(EXPOSURE),data=training1,dist ="poisson",zero.dist = "poisson",link= "logit")

# HURDLE NEGATIVE BINOMIAL MODEL with offset

# recast_data1
HURDLE_negbin_recast1 <- hurdle(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                                offset=log(EXPOSURE),data=recast_data1,dist ="negbin",zero.dist = "negbin",link= "logit")

# training1
HURDLE_negbin_train1 <- hurdle(CLAIMNO ~ F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13 |F1 + F3 + F4 + F5 + F8 + F9 + F10 + F11 + F12 + F13,
                               offset=log(EXPOSURE),data=training1,dist ="negbin",zero.dist = "negbin",link= "logit")

# training1 are the best data :)

# Save models

save(poisson_glm_base, file = "models/poisson_glm_base.rda")
save(poisson_glm_drop, file = "models/poisson_glm_drop.rda")
save(poisson_glm_recast1, file = "models/poisson_glm_recast1.rda")
save(poisson_glm_relevel, file = "models/poisson_glm_relevel.rda")
save(poisson_glm_train1, file = "models/poisson_glm_train1.rda")
save(INTER_poisson_glm_train1, file = "models/INTER_poisson_glm_train1.rda")

save(negbin_glm_base, file = "models/negbin_glm_base.rda")
save(negbin_glm_drop, file = "models/negbin_glm_drop.rda")
save(negbin_glm_recast1, file = "models/negbin_glm_recast1.rda")
save(negbin_glm_train1, file = "models/negbin_glm_train1.rda")
save(INTER_negbin_glm_train1, file = "models/INTER_negbin_glm_train1.rda")

save(ZIpoisson_recast1, file = "models/ZIpoisson_recast1.rda")
save(ZIpoisson_train1, file = "models/ZIpoisson_train1.rda")
save(INTER_ZI.pois_glm_train1, file = "models/INTER_ZI.pois_glm_train1.rda")

save(ZInegbin_recast1, file = "models/ZInegbin_recast1.rda")
save(ZInegbin_train1, file = "models/ZInegbin_train1.rda")
save(INTER_ZInegbin_glm_train1, file = "models/INTER_ZInegbin_glm_train1.rda")

save(HURDLE_poisson_recast1, file = "models/HURDLE_poisson_recast1.rda")
save(HURDLE_poisson_train1, file = "models/HURDLE_poisson_train1.rda")

save(HURDLE_negbin_recast1, file = "models/HURDLE_negbin_recast1.rda")
save(HURDLE_negbin_train1, file = "models/HURDLE_negbin_train1.rda")
