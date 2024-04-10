function vRestrictedParms = restrictParmsGaussianCopula(vParms)

vRestrictedParms = vParms;
vRestrictedParms(2) = exp(vParms(2)); 
vRestrictedParms(3) = exp(vParms(3))/(1 + exp(vParms(3)));
