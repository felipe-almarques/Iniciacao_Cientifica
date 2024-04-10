function [dLoglike, vFactors, vRho] = gaussianCopulaGASmodel(vParms, info, mData)

dLoglike = 0;
iT = size(mData,2);
vFactors = zeros(1,iT+1);
vRho = zeros(1,iT+1);

vOmega = vParms(1,1);
mA = vParms(2,1);
mB = vParms(3,1);

% initial condition
vFactor = vOmega;
vOmega = vOmega*(1 - mB);
	
for t = 1:iT

	% compute the copula parameters based on the factors
	rho = (1 - exp(-vFactor)) / (1 + exp(-vFactor));
	rho2 = rho*rho;
    vFactors(t) = vFactor;
    vRho(t) = rho;
	
	% quantile functions (inverse normal CDF)
	qu = norminv(mData(:,t));
	x = qu(1)^2 + qu(2)^2;
	y = qu(1)*qu(2);
		
    % get the log pdf of the copula, and its gradient with respect to the copula parameters
	dLoglike = dLoglike -0.5 * log(1 - rho2) - 0.5 * (rho2*x - 2*rho*y) / (1 - rho2);

    % scaled score function
    vSt = (2/(1-rho2))*(y - rho - rho*(x-2)/(1+rho2));
		
    % GAS recursion
    vFactor = vOmega + mA * vSt + mB * vFactor;
end


vFactors(iT+1) = vFactor;
vRho(iT+1) = rho;
    

return;