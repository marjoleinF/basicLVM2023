##########################################
### School Attitude Questionnaire 

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


##########################################
### FITTING THE MODEL

## load package lavaan
require(lavaan)

## specify three-factor model
saqmodel <- '

		# regression equations
		Motivation =~ learning + concentration + homework
		
		## AND ADD THE OTHER TWO FACTORS
		'

## run model
saqmodelOut <- cfa(saqmodel, sample.cov = saqcov, sample.nobs = 915)

## output
summary(saqmodelOut)

## interpret the factor loadings of the first latent factor

## interpret the covariance between motivation and satisfaction

## interpret the firs estimate under "variances"



##########################################
### ADDITIONAL PARAMETERS 

## specify all of the parameters in the model 
saqmodel <- '
		# regression equations

    # -> like above

		# residual variances observed variables 
		learning ~~ learning
		concentration ~~ concentration
    ## and so on...

		# variances common factors
    Motivation ~~ Motivation
    ## and so on...

		# covariances common factors
		Motivation ~~ Satisfaction
    ## and so on...
		'

## run model
saqmodelOut <- cfa(saqmodel, sample.cov = saqcov, 
                   sample.nobs = 915)

## output
summary(saqmodelOut)

## check your model fit, is it the same??



##########################################
### IDENTIFICATION

## how many parameters are being estimated in the model?

## how many information statistics do we have?

## degrees of freedom are thus:

## is the model identified?


##########################################
### ULI vs UVI

## Fit the same model with UVI instead of ULI
## What happens to the model fit?
## What happens to the parameter estimates?


## request the standardized output



##########################################
### MODEL FIT

## request additional model fit statistics

## -> what is your conclusion with regards to model fit?

## standardized residuals

resid(saqmodelOut)$cov # residuals
hist(resid(saqmodelOut)$cov) # histogram of residuals

cov2cor(saqcov)-cov2cor(fitted(saqmodelOut)$cov) # difference in correlation matrices
lavResiduals(saqmodelOut)$cov # function to get the same values

## -> see any patterns or high values? 
## -> what would be your solution?


## modification indices

modificationIndices(saqmodelOut)

# order by size
modificationIndices(saqmodelOut,sort.=TRUE)[1:10,]

## -> do you see any possible modification to improve model fit?
## -> what parameter would you add?


##########################################
### COMPARE MODEL FIT


## Add a parameter based on the results from the residuals and/or modification indices

## Compare the models in terms of model fit
anova(saqmodelOut, [improvedModel_Output])

## -> What is your conclusion?




