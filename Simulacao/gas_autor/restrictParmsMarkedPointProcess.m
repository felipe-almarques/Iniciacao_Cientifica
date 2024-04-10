function vRestrictedParms = restrictParmsMarkedPointProcess(vParms)

vRestrictedParms = vParms;

vRestrictedParms(4) = exp(vParms(4)); 
vRestrictedParms(5) = exp(vParms(5))/(1 + exp(vParms(5)));
