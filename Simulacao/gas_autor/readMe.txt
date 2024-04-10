Hello,

This file includes Matlab code for estimating the Gaussian copula GAS model and the marked point process GAS model. The data included here for the Patton example is the same as in the paper. Unfortunately, the data for the credit risk application is confidential through Moody's and I cannot provide it. Instead, I have estimated the model on simulated data to give some idea of how it works.

Please also note that the code for the Gaussian copula GAS model is slightly different from the code in the paper (the reported results are from the Ox code). In particular, we estimated the initial condition f_{0} in the paper whereas the current version of the code uses the stationary distribution of the factor. Therefore, the log-likelihood will be slightly lower in this code than in the paper. In addition, the code used in the paper had a factor of 2 missing in the scaled score step. This is absorbed in the estimate of the coefficient ``A'' in the GAS recursion and has no effect on the likelihood.

Best wishes,

Drew  