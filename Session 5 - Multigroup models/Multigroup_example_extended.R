##########################################
### MULTIGROUP EXAMPLE

## Holzinger and Swineford (1939) dataset included in the lavaan package:
  
summary(HolzingerSwineford1939)

## use a two-factor model with the first six indicators

HS.mod.2 <- '
              Visual =~ x1 + x2 + x3
              Textual =~ x4 + x5 + x6
'

HS.fit.2 <- cfa(HS.mod.2, data = HolzingerSwineford1939, std.lv=TRUE)
summary(HS.fit.2, standardized = TRUE, fit.measures = TRUE)


## there is also a variable sex in the dataset, that we will use as grouping variable
table(HS$sex)
# 1 = girls; 2 = boys

## Step 1: Configural invariance (baseline model)
## All measurement parameters free across groups, but the pattern is the same

HS.fit.mg <- cfa(HS.mod.2, 
                 data = HolzingerSwineford1939, 
                 group = "sex",
                 std.lv=TRUE) # to identify scale through UVI constraint

summary(HS.fit.mg, standardized = TRUE, fit.measures = TRUE)



## Step 2: Metric (weak) invariance - constrain loadings across groups

HS.mod.metric <- '
              # factor loadings
              Visual =~ c(L1, L1)*x1 + c(L2, L2)*x2 + c(L3, L3)*x3 #+ c(CL9, CL9)*x9
              Textual =~ c(L4, L4)*x4 + c(L5, L5)*x5 + c(L6, L6)*x6

              # Free latent variances in second group (boys)
              Visual ~~ c(1, NA)*Visual
              Textual ~~ c(1, NA)*Textual
'

HS.fit.metric <- cfa(HS.mod.metric, data = HolzingerSwineford1939, group = "sex", std.lv = TRUE)
summary(HS.fit.metric, standardized = TRUE, fit.measures=TRUE)

## significant decrement in fit?
anova(HS.fit.config, HS.fit.metric)
fitMeasures(HS.fit.config, "cfi") - fitMeasures(HS.fit.metric, "cfi")




### Try scalar (strong) invariance

## Step 3: Scalar (strong) invariance - also constrain intercepts across groups

HS.mod.scalar <- '
                # Factor loadings
                Visual =~ c(L1, L1)*x1 + c(L2, L2)*x2 + c(L3, L3)*x3 
                Textual =~ c(L4, L4)*x4 + c(L5, L5)*x5 + c(L6, L6)*x6

                ## Free latent variances in second group (boys)
                Visual ~~ c(1, NA)*Visual
                Textual ~~ c(1, NA)*Textual
                
                Visual ~~ c(COV_girls, COV_boys)*Textual
                
                # Intercepts
                x1 ~ c(T1, T1)*1
                x2 ~ c(T2, T2)*1
                x3 ~ c(T3, T3)*1
                x4 ~ c(T4, T4)*1
                x5 ~ c(T5, T5)*1
                x6 ~ c(T6, T6)*1

                # Free factor means second group
                Visual ~ c(k1, NA)*1
                Textual ~ c(k2, NA)*1
                
                
                mean_x1 := T1+L1*k1
                COV_girls == COV_boys
                
                
'

HS.fit.scalar <- cfa(HS.mod.scalar, data = HolzingerSwineford1939, group = "sex", std.lv = TRUE)
summary(HS.fit.scalar, standardized = TRUE, fit.measures=TRUE)

## significant decrement in fit?
anova(HS.fit.metric, HS.fit.scalar)
fitMeasures(HS.fit.metric, "cfi") - fitMeasures(HS.fit.scalar, "cfi")

## need to perform follow-up tests to find uniform DIF
resid(HS.fit.scalar, type = "cor")
#-> intercept x3?

modificationIndices(fit.scalar, free.remove=FALSE)
modificationIndices(fit.scalar, free.remove=FALSE, sort. = TRUE)[1:20,]
# -> intercept x4?


## alternative way of specifying
# configural invariance
fit1 <- cfa(HS.mod.config, data = HolzingerSwineford1939, group = "sex")

# weak invariance
fit2 <- cfa(HS.mod.config, data = HolzingerSwineford1939, group = "sex",
            group.equal = "loadings")

# strong invariance
fit3 <- cfa(HS.mod.config, data = HolzingerSwineford1939, group = "sex",
            group.equal = c("intercepts", "loadings"))

# model comparison tests
anova(fit1, fit2, fit3)



## partial invariance models

HS.mod.scalar.x3 <- '
                # Factor loadings
                Visual =~ c(L1, L1)*x1 + c(L2, L2)*x2 + c(L3, L3)*x3 
                Textual =~ c(L4, L4)*x4 + c(L5, L5)*x5 + c(L6, L6)*x6

                ## Free latent variances in second group (boys)
                Visual ~~ c(1, NA)*Visual
                Textual ~~ c(1, NA)*Textual
                
                # Intercepts
                # Free x3 intercept
                x1 ~ c(T1, T1)*1
                x2 ~ c(T2, T2)*1
                x3 ~ 1
                x4 ~ c(T4, T4)*1
                x5 ~ c(T5, T5)*1
                x6 ~ c(T6, T6)*1

                # Free factor means second group
                Visual ~ c(0, NA)*1
                Textual ~ c(0, NA)*1
'

HS.fit.scalar.x3 <- cfa(HS.mod.scalar.x3, data = HolzingerSwineford1939, group = "sex", std.lv = TRUE)
summary(HS.fit.scalar.x3, standardized = TRUE, fit.measures=TRUE)

## significant improvement in fit?
anova(HS.fit.scalar, HS.fit.scalar.x3)

## partial invariance tenable?
anova(HS.fit.metric, HS.fit.scalar.x3)



## partial invariance models

HS.mod.scalar.x4 <- '
                # Factor loadings
                Visual =~ c(L1, L1)*x1 + c(L2, L2)*x2 + c(L3, L3)*x3 
                Textual =~ c(L4, L4)*x4 + c(L5, L5)*x5 + c(L6, L6)*x6

                ## Free latent variances in second group (boys)
                Visual ~~ c(1, NA)*Visual
                Textual ~~ c(1, NA)*Textual
                
                # Intercepts
                # Free x3 intercept
                x1 ~ c(T1, T1)*1
                x2 ~ c(T2, T2)*1
                x3 ~ c(T3, T3)*1
                x4 ~ 1
                x5 ~ c(T5, T5)*1
                x6 ~ c(T6, T6)*1

                # Free factor means second group
                Visual ~ c(0, NA)*1
                Textual ~ c(0, NA)*1
'

HS.fit.scalar.x4 <- cfa(HS.mod.scalar.x4, data = HolzingerSwineford1939, group = "sex", std.lv = TRUE)
summary(HS.fit.scalar.x4, standardized = TRUE, fit.measures=TRUE)

## significant improvement in fit?
anova(HS.fit.scalar, HS.fit.scalar.x4)

## partial invariance tenable?
anova(HS.fit.metric, HS.fit.scalar.x4)

## -> the model with partial invariance where x4 intercept is allowed to differ gives best result
## -> interpretation???
## -> intercept value is higher (easier) for boys compared to girls with the same value of the latent trait (Textual IQ)
## -> biased against girls (e.g. might be gendered topic in text) 


