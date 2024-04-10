function dLoglike = loglikelihoodMarkedPointProcessGASmodel(vParms, info, mY)

iT = size(mY,2);
mR = info.mR;     % number of firms that can transition
vTau = info.vTau; % duration between transitions

vRestrictedParms = restrictParmsMarkedPointProcess(vParms);

% parameters of the model
vOmega = 0; % for identification
vEta = vRestrictedParms(1:2);
mAlpha = [1 ; vRestrictedParms(3)]; % factor loadings
mA = vRestrictedParms(4); 
mB = vRestrictedParms(5);

dLoglike = 0;
vFactor = 0;

for t=1:iT
    
    vLambda = vEta + mAlpha*vFactor;
	vExpLambda = exp(vLambda);

    % compute the log-likelihood
	dLoglike = dLoglike + mY(:,t)'*vLambda - vTau(t)*mR(:,t)'*vExpLambda;
		
    % compute the score and information matrix
	vScore = mAlpha'*(mY(:,t)-vTau(t)*mR(:,t).*vExpLambda);
    mInfoMatrix = mAlpha'*((mR(:,t).*vExpLambda/(mR(:,t)'*vExpLambda)).*mAlpha);
 	vSt = mInfoMatrix\vScore;

    % GAS recursion
	vFactor = vOmega + mA*vSt + mB*vFactor;
end

% return negative the loglikelihood scaled by the observations
dLoglike = -dLoglike/iT;