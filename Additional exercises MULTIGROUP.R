##########################################
### Additional excercise multigroup

## From Beajean; Exercise 4.1

# Beaujean and Sheng (in press) compared the different editions of the Wechsler Intelligence Scale for Children
# (WISC) in a 7-year-old age group. The correlations, means and standard deviations for the of the WISC
# versions are given below:

  WISC.cor <- lav_matrix_lower2full(c(
    1.00,
    0.31, 1.00,
    0.36, 0.40, 1.00,
    0.51, 0.46, 0.45, 1.00,
    0.29, 0.40, 0.33, 0.43, 1.00,
    0.39, 0.29, 0.27, 0.36, 0.33, 1.00,
    0.32, 0.27, 0.29, 0.33, 0.24, 0.28, 1.00,
    0.22, 0.32, 0.15, 0.22, 0.27, 0.12, 0.26, 1.00
  ))
WISC.means <- c(7.83, 5.50, 5.67, 21.50, 7.67, 8.00, 6.50, 34.83)
WISC.sds <- c(2.69, 1.50, 2.36, 6.06, 1.85, 2.18, 5.97, 9.94)
WISC.cov <- cor2cov(WISC.cor, sds=WISC.sds)
WISCIV.cor <- lav_matrix_lower2full(c(
  1.00,
  0.46, 1.00,
  0.58, 0.55, 1.00,
  0.63, 0.43, 0.73, 1.00,
  0.27, 0.51, 0.37, 0.33, 1.00,
  0.45, 0.38, 0.37, 0.43, 0.13, 1.00,
  0.33, 0.52, 0.49, 0.41, 0.29, 0.43, 1.00,
  0.15, 0.27, 0.16, 0.09, 0.12, 0.25, 0.23, 1.00
))
WISCIV.means <- c(15.17, 15.00, 11.83, 21.67, 12.17, 17.83, 18.67, 45.83)
WISCIV.sds <- c(4.93, 4.10, 5.20, 6.54, 2.72, 5.35, 9.36, 10.44)
WISCIV.cov <- cor2cov(WISCIV.cor, sds=WISCIV.sds)
WISC.names <- c("Compr", "Arith", "Simil", "Vocab", "DigSpan", "PictCompl",
                "BlockDes", "Cod")
names(WISC.means) <- names(WISCIV.sds) <- names(WISC.sds) <-
  names(WISCIV.sds) <- rownames(WISC.cov) <- colnames(WISC.cov) <-
  rownames(WISCIV.cov) <- colnames(WISCIV.cov) <- WISC.names
WISC.cov.list <- list(WISC.cov, WISCIV.cov)
WISC.mean.list <- list(WISC.means, WISCIV.means)
WISC.n.list <- list(WISC.n = 200, WISCIV.n = 200)

###
## You are going to fit a model with the following three factors to the data:

  # verbal comprehension
  VC =~ Simil + Vocab + Compr
# perceptual reasoning
PR =~ PictCompl + BlockDes
# working memory
WM =~ Arith + DigSpan + Cod

## a) Test for invariance. Note that if we find more than half of the parameters to be unequal across groups,
# we may as well leave all parameters in the invariance-testing step freely estimated in both groups. For
# example, if 6 out of 8 loadings appear to be unequal across groups, it is probably better to estimate
# all loadings freely and try to interpret differences in parameter differences.

## b) Describe and interpret any differences in parameter estimates you found.