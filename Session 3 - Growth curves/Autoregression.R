
## Lavaan
library(lavaan)

## observed data
obsnames <- c("year1","year2","year3","year4","gender","famstat")

## This example is taken from Duncan and Duncan (1996), who conducted
## a longitudinal study of alcohol use among adolescents. A sample of 321 adolescents were
## surveyed annually over a 4-year period

values <- c(1.000,
     .640, 1.000,
     .586,	.670, 1.000,
     .454,	.566,	.621, 1.000,
     .001,	.038,	.118,	.091, 1.000,
     -.214, -.149, -.135, -.163, -.025, 1.000)

SD <- c(1.002,.960,.912,.920,.504,.498) 

duncanmeans <- c(2.271,2.560,2.694,2.965,.573,.554) 

duncancor <- lav_matrix_lower2full(values)
duncancov <- diag(SD)%*%duncancor%*%diag(SD)
dimnames(duncancov) <- list(obsnames,obsnames)
names(duncanmeans) <- obsnames


## define model
duncanmodel1 <- '
			# COVARIANCES
			# regression equations
			year4 ~ b43*year3
			year3 ~ b32*year2
			year2 ~ b21*year1

			# residual variances
			year4 ~~ p44*year4
			year3 ~~ p33*year3
			year2 ~~ p22*year2

			# variance exogenous variable
			year1 ~~ p11*year1

			# MEANS
			# intercepts endogenous variables
			year4 ~ a4*1
			year3 ~ a3*1
			year2 ~ a2*1

			# means exogenous variables
			year1 ~ a1*1
		      '

## run model
duncanmodel1Out <- sem(duncanmodel1, 
                       sample.cov=duncancov,
                       sample.mean=duncanmeans, 
                       sample.nobs=321)	

## output
summary(duncanmodel1Out, standardized=TRUE)


## add second order effects
duncanmodel2 <- '
			# COVARIANCES
			# regression equations
			year4 ~ b43*year3 + b42*year2
			year3 ~ b32*year2 + b31*year1
			year2 ~ b21*year1

			# residual variances
			year4 ~~ p44*year4
			year3 ~~ p33*year3
			year2 ~~ p22*year2

			# variance exogenous variable
			year1 ~~ p11*year1

			# MEANS
			# intercepts endogenous variables
			year4 ~ a4*1
			year3 ~ a3*1
			year2 ~ a2*1

			# means exogenous variables
			year1 ~ a1*1
		      '

## run model
duncanmodel2Out <- sem(duncanmodel2, 
                       sample.cov=duncancov,
                       sample.mean=duncanmeans, 
                       sample.nobs=321)	

## output
summary(duncanmodel2Out)
anova(duncanmodel1Out,duncanmodel2Out)




## test equality of first order effects
duncanmodel3 <- '
			# COVARIANCES
			# regression equations
			year4 ~ b1*year3 + b42*year2
			year3 ~ b1*year2 + b31*year1
			year2 ~ b1*year1

			# residual variances
			year4 ~~ p44*year4
			year3 ~~ p33*year3
			year2 ~~ p22*year2

			# variance exogenous variable
			year1 ~~ p11*year1

			# MEANS
			# intercepts endogenous variables
			year4 ~ a4*1
			year3 ~ a3*1
			year2 ~ a2*1

			# means exogenous variables
			year1 ~ a1*1
		      '

## run model
duncanmodel3Out <- sem(duncanmodel3, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel3Out,standardized=TRUE)
anova(duncanmodel3Out,duncanmodel2Out)






## test equality of second order effects
duncanmodel4 <- '
			# COVARIANCES
			# regression equations
			year4 ~ b1*year3 + b2*year2
			year3 ~ b1*year2 + b2*year1
			year2 ~ b1*year1

			# residual variances
			year4 ~~ p44*year4
			year3 ~~ p33*year3
			year2 ~~ p22*year2

			# variance exogenous variable
			year1 ~~ p11*year1

			# MEANS
			# intercepts endogenous variables
			year4 ~ a4*1
			year3 ~ a3*1
			year2 ~ a2*1

			# means exogenous variables
			year1 ~ a1*1
		      '

## run model
duncanmodel4Out <- lavaan(duncanmodel4, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel4Out)
anova(duncanmodel4Out,duncanmodel3Out)





## test equality of observed indicator means
duncanmodel5 <- '
			# COVARIANCES
			# regression equations
			year4 ~ b43*year3 + b42*year2
			year3 ~ b32*year2 + b31*year1
			year2 ~ b21*year1

			# residual variances
			year4 ~~ p44*year4
			year3 ~~ p33*year3
			year2 ~~ p22*year2

			# variance exogenous variable
			year1 ~~ p11*year1

			# MEANS
			# intercepts endogenous variables
			year4 ~ a4*1
			year3 ~ a3*1
			year2 ~ a2*1

			# means exogenous variables
			year1 ~ a1*1

			# define means
			mu1:= a1
			mu2:= b21*a1 + a2
			mu3:= b32*a2 + b32*b21*a1 + a3
			mu4:= b43*a3 + b43*b32*a2 + b43*b32*b21*a1 + a4

			# restrict means
			mu1 == mu2 
			mu2 == mu3
			mu3 == mu4
		      '

## run model
duncanmodel5Out <- sem(duncanmodel5, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel5Out)
anova(duncanmodel5Out,duncanmodel2Out)





## adding predictors of first measurement
duncanmodel6 <- '
			# COVARIANCES
			# regression equations
			year4 ~ b43*year3 + b42*year2
			year3 ~ b32*year2 + b31*year1
			year2 ~ b21*year1
			year1 ~ b15*gender + b16*famstat

			# residual variances
			year4 ~~ p44*year4
			year3 ~~ p33*year3
			year2 ~~ p22*year2
			year1 ~~ p11*year1

			# (co)variance exogenous variable
			gender ~~ p55*gender
			famstat ~~ p66*famstat
			gender ~~ p65*famstat

			# MEANS
			# intercepts endogenous variables
			year4 ~ a4*1
			year3 ~ a3*1
			year2 ~ a2*1
			year1 ~ a1*1

			# means exogenous variables
			gender ~ a5*1
			famstat ~ a6*1

			# define means
			mu1:= b15*a5 + b16*a6 + a1
			mu2:= b21*a1 + b21*b15*a5 + b21*b16*a6 + a2
			mu3:= b32*a2 + b31*a1 + b32*b21*a1 + b32*b21*b15*a5 +
                        b32*b21*b16*a6 + a3
			mu4:= b43*a3 + b42*a2 + b43*b32*a2 + b43*b32*b21*a1 +
                        b43*b32*b21*b15*a5 + b43*b32*b21*b16*a6 + a4
			mu5:= a5
			mu6:= a6
		      '

## run model
duncanmodel6Out <- lavaan(duncanmodel6, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel6Out,standardized=TRUE)



