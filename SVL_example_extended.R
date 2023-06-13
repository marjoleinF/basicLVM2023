## observed covariance matrix
obsnames <- c("learning","concentration","homework","fun",
              "acceptance","teacher","selfexpr","selfeff","socialskill")

values <- c(30.6301, 
            26.9452, 56.8918,
            24.1473, 31.6878, 53.2488,
            16.3770, 18.4153, 16.8599, 27.9758,
            7.8174,  9.6851, 10.0114, 12.8765, 47.0970,
            13.6902, 16.9232, 12.9326, 17.2880, 10.3672, 29.0119,
            15.3122, 24.2849, 21.4935, 10.9621, 13.9909, 11.6333,
            59.5343,
            13.4457, 21.8158, 18.8545,  7.3931, 10.2333,  7.1434,
            29.7953, 49.2213,
            6.6074, 12.7343, 10.5768,  6.4065, 13.4258,  6.1429,
            26.0849, 23.6253, 40.0922)

saqcov <- lav_matrix_lower2full(values)
dimnames(saqcov) <- list(obsnames,obsnames)
saqcov

## specify three-factor model
saqmodel <- '

		# regression equations
		Motivation =~ learning + concentration + homework
		Satisfaction =~ fun + acceptance + teacher
		SelfConfidence =~ selfexpr + selfeff + socialskill		
		'

## run model
saqmodelOut <- cfa(saqmodel, sample.cov = saqcov, sample.nobs = 915)

## output
summary(saqmodelOut)

## interpret the factor loadings of the first latent factor
# -> first factor loading  is 1, second and third are a bit higher
# -> interpretation similar to regression coefficients
# -> if motivation increases with 1, then .. increases with ...

## interpret the covariance between motivation and satisfaction
# -> covariance is 15,.. ; positive relationship

## interpret the firs estimate under "variances"
# -> variance of the residual factor of learning is 9,...
# -> this is the part of the variance of the indicator learning that is not explained by the model


##########################################
### ADDITIONAL PARAMETERS 

## specify all of the parameters in the model 

saqmodel <- '
		# regression equations
		Motivation =~ learning + concentration + homework
		Satisfaction =~ fun + acceptance + teacher
		SelfConfidence =~ selfexpr + selfeff + socialskill

		# residual variances observed variables 
		learning ~~ learning
		concentration ~~ concentration
		homework ~~ homework
		fun ~~ fun
		acceptance ~~ acceptance
		teacher ~~ teacher
		selfexpr ~~ selfexpr
		selfeff ~~ selfeff
		socialskill ~~ socialskill

		# variances common factors
    Motivation ~~ Motivation
    Satisfaction ~~ Satisfaction
    SelfConfidence ~~ SelfConfidence

		# covariances common factors
		Motivation ~~ Satisfaction
		Motivation ~~ SelfConfidence
		Satisfaction ~~ SelfConfidence
		'

## run model
saqmodelOut <- cfa(saqmodel, sample.cov = saqcov, 
                      sample.nobs = 915)

## output
summary(saqmodelOut)

# -> Model fit is the same


##########################################
### IDENTIFICATION

## how many parameters are being estimated in the model?
# -> count the number of arrows in the model (or parameters in the output)
# -> 21 parameters

## how many information statistics do we have?
# -> V = 9*10/2 = 45

## degrees of freedom are thus:
# -> DF 45 - 21 = 24

## is the model identified?
# -> Yes!

##########################################
### ULI vs UVI

## specify UVI instead of ULI

saqmodel <- '
		# regression equations
		Motivation =~ NA*learning + concentration + homework
		Satisfaction =~ NA*fun + acceptance + teacher
		SelfConfidence =~ NA*selfexpr + selfeff + socialskill

		# residual variances observed variables 
		learning ~~ learning
		concentration ~~ concentration
		homework ~~ homework
		fun ~~ fun
		acceptance ~~ acceptance
		teacher ~~ teacher
		selfexpr ~~ selfexpr
		selfeff ~~ selfeff
		socialskill ~~ socialskill

		# variances common factors
    Motivation ~~ 1*Motivation
    Satisfaction ~~ 1*Satisfaction
    SelfConfidence ~~ 1*SelfConfidence

		# covariances common factors
		Motivation ~~ Satisfaction
		Motivation ~~ SelfConfidence
		Satisfaction ~~ SelfConfidence
		'

## run model
saqmodelOut <- cfa(saqmodel, sample.cov = saqcov, 
                   sample.nobs = 915)

## output
summary(saqmodelOut)

## What happens to the model fit?
# -> Model fit is the same

## What happens to the parameter estimates?
# -> Model parameters are different

## standardized output
summary(saqmodelOut, standardized=TRUE)
# -> Standardized output is the same



##########################################
### MODEL FIT

## request additional model fit statistics

## output
summary(saqmodelOut, standardized=TRUE, fit.measures=TRUE)

## -> what is your conclusion with regards to model fit?
# -> chi-square test is significant, so hypothesis of exact fit is rejected
# -> CFI is > .95, indicating good fit (but TLI is lower)
# -> RMSEA is > .08, so fit is not acceptable (although hypothesis of close fit cannot be rejected)
# -> Conclusion about model fit is therefore a bit mixed

## residuals
lavResiduals(saqmodelOut)$cov 
# -> high residuals (>.1) between socialskill and acceptance, socialskill and learning, and acceptance and selfexpr

## modification indices
modificationIndices(saqmodelOut,sort.=TRUE)[1:10,]
# -> highest MI is between acceptance and socialskill


##########################################
### COMPARE MODEL FIT


## Add a parameter based on the results from the residuals and/or modification indices

saqmodel <- '

		# regression equations
		Motivation =~ learning + concentration + homework
		Satisfaction =~ fun + acceptance + teacher
		SelfConfidence =~ selfexpr + selfeff + socialskill
		
		# add residual covariance
		socialskill ~~ acceptance
		'

## run model
saqmodelOut_2 <- cfa(saqmodel, sample.cov = saqcov, sample.nobs = 915)

## compare models
anova(saqmodelOut,saqmodelOut_2)
# -> significant improvement in model fit

summary(saqmodelOut_2,fit.measures=TRUE)
# -> overall model fit also looks better


##########################################
### ADDING MEANS

saqmeans <- c(2.3,3.4,5.4,2.1,4.6,3.3,6,1,4.2,4.0)
names(saqmeans) <- obsnames

saqmodel <- '

		# regression equations
		Motivation =~ learning + concentration + homework
		Satisfaction =~ fun + acceptance + teacher
		SelfConfidence =~ selfexpr + selfeff + socialskill		
		'

## run model
saqmodelOut <- cfa(saqmodel, # model
                   sample.cov = saqcov, # sample covariance matrix
                   sample.mean = saqmeans, # sample means vector
                   sample.nobs = 915) # sample size

## output
summary(saqmodelOut)
