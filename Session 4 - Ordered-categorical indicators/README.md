As per lavaan 0.5-21, we should use the `robust`, not `scaled` fit indices:

robust RMSEA and CFI values are now computed correctly, following Brosseau-Liard, P. E., Savalei, V., and Li, L. (2012), and Brosseau-Liard, P. E. and Savalei, V. (2014); in the output of fitMeasures(), the ‘new’ ones are called cfi.robust and rmsea.robust, while the ‘old’ ones are called cfi.scaled and rmsea.scaled

source: https://lavaan.ugent.be/history/dot5.html
