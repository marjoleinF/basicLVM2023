### Additional excercise LGC

ccawLongG0.c <- c(3.59, 3.11, 3.10, 2.91, 2.80, 2.82, 3.22, 3.05, 2.86, 3.30, 2.88, 2.63, 2.62,
                  2.82, 2.71)
ccawLongG0.mean <- c(11.97 , 11.72 , 12.03 , 11.96 , 12.10)
ccawLongG1.c <- c(3.42, 3.03, 3.18, 2.62, 2.73, 2.69, 2.95, 2.97, 2.59, 3.02, 2.89, 2.91, 2.67,
                  2.83, 3.25)
ccawLongG1.mean <- c(9.80 , 12.00 , 13.94 , 15.96 , 18.10)

# place data in matrices
library(lavaan)
ccawLongG0.cov <- lav_matrix_lower2full(ccawLongG0.c)
ccawLongG1.cov <- lav_matrix_lower2full(ccawLongG1.c)

# name the variables
colnames(ccawLongG0.cov) <- rownames(ccawLongG0.cov) <- paste("T",seq(1,5,1), sep="")
colnames(ccawLongG1.cov) <- rownames(ccawLongG1.cov) <- paste("T",seq(1,5,1), sep="")
names(ccawLongG0.mean) <- paste("T",seq(1,5,1), sep="")
names(ccawLongG1.mean) <- paste("T",seq(1,5,1), sep="")

# combine the data into lists
ccawLong.cov <- list(G0=ccawLongG0.cov, G1=ccawLongG1.cov)
ccawLong.mean <- list(G0=ccawLongG0.mean, G1=ccawLongG1.mean)
ccawLong.n <- list(G0=30,G1=30)


### Now fit the series of models
### What model would you select?
### Interpret the model parameters











