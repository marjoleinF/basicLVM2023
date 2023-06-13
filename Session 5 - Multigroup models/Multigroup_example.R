##########################################
### MULTIGROUP EXAMPLE

## Holzinger and Swineford (1939) dataset included in the lavaan package:

summary(HolzingerSwineford1939)

## Step 1: Configural invariance (baseline model)
## All measurement parameters free across groups, but the pattern is the same

HS.mod.config <- '
                # regression equations
                Visual  =~ x1 + x2 + x3
                Textual =~ x4 + x5 + x6
'
HS.fit.config <- cfa(HS.mod.config, data = HolzingerSwineford1939, group = "sex", std.lv = TRUE)
summary(HS.fit.config, fit = TRUE, standardized = TRUE)




## Step 2: Metric (weak) invariance - constrain loadings across groups

HS.mod.metric <- '
              # factor loadings
              Visual =~ c(L1, L1)*x1 + c(L2, L2)*x2 + c(L3, L3)*x3 
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

## -> Is it tenable?
## -> If not, search for tenable partial measurement invariance model


### Try residual (strict) invariance

## -> Is it tenable?
## -> If not, search for tenable partial measurement invariance model

