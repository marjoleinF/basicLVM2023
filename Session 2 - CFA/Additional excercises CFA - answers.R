##########################################
### ADDITIONAL EXCERCISES CFA

## 1. Holzinger and Swineford (1939) dataset included in the lavaan package:
  
summary(HolzingerSwineford1939)

HS.mod.1 <- '
              IQ =~ x1 + x2 + x3 + x4 + x5 + x6
'

HS.fit.1 <- cfa(HS.mod.1, data = HolzingerSwineford1939, std.lv=TRUE)
summary(HS.fit.1, standardized = TRUE, fit.measures = TRUE)

## a
## -> The model does not fit very well. Also, the standardized loadings of X1, X2 and X3 are quite low,
## compared to those of X4, X5 and X6; so these indicators do not seem well explained by the model.

## b

lavResiduals(HS.fit.1)$cov
# -> residuals between x1, x2 and x3 are all high

modificationIndices(HS.fit.1,sort.=TRUE)[1:10,]
# -> similar, three highest MIs are between x1, x2 and x3

## -> these relations are not well reprented by the 1-factor model
## -> maybe a 2-factor model fits better


## c

HS.mod.2 <- '
              IQ1 =~ x1 + x2 + x3
              IQ2 =~ x4 + x5 + x6
'

HS.fit.2 <- cfa(HS.mod.2, data = HolzingerSwineford1939, std.lv=TRUE)
summary(HS.fit.2, standardized = TRUE, fit.measures = TRUE)
anova(HS.fit.1,HS.fit.2)

## -> Model fit has improved substantially 
## -> Looking at the estimated parameters, the standardized factor loadings of X1, X2 and X3 have substantially increased.

lavResiduals(HS.fit.2)$cov
# -> still one high residual between x3 - x5

