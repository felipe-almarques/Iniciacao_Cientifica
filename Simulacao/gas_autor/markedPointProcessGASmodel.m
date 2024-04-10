function [dLoglike, vFactors, vLam] = markedPointProcessGASmodel(vParms, info, mY)

iT = size(mY,2);
mR = info.mR;     % number of firms that can transition
vTau = info.vTau; % duration between transitions

% parameters of the model
vOmega = 0; % for identification
vEta = vParms(1:2);
mAlpha = [1 ; vParms(3)]; % factor loadings
mA = vParms(4); 
mB = vParms(5);

dLoglike = 0;
vFactors = zeros(1,iT);
vLam = zeros(2,iT);
vFactor = 0;

for t=1:iT
    
    vLambda = vEta + mAlpha*vFactor;
	vExpLambda = exp(vLambda);

    % store these
    vFactors(t) = vFactor;
    vLam(:,t) = vLambda;
    
    % compute the log-likelihood
	dLoglike = dLoglike + mY(:,t)'*vLambda - vTau(t)*mR(:,t)'*vExpLambda;
		
    % compute the score and information matrix
	vScore = mAlpha'*(mY(:,t)-vTau(t)*mR(:,t).*vExpLambda);
    mInfoMatrix = mAlpha'*((mR(:,t).*vExpLambda/(mR(:,t)'*vExpLambda)).*mAlpha);
 	vSt = mInfoMatrix\vScore;

    % GAS recursion
	vFactor = vOmega + mA*vSt + mB*vFactor;
end