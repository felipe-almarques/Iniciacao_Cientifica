function [mY, mR, vTau, vFactors] = simulateMarkedPointProcessModel(iN, iMaxT, vEta, mAlpha, mA, mB)


% initialize
vR = [iN ; iN];  % mumber of firms exposed to each type of risk (i.e. default or not)
	
vOmega = 0; % set to zero for identification
vFactor = 0;
vFactors = zeros(1,50000);
vTau = zeros(1,50000);
mY = zeros(2,50000);
mR = zeros(2,50000);
dSpellTotal = 0;
t = 1;

while dSpellTotal<=iMaxT
	% extend sample matrices if necessary
    iT = size(vTau,2);
	if(t >= iT)
		vTau = [vTau zeros(1,1000)];
		mY = [mY zeros(2,1000)];
		mR = [mR zeros(2,1000)];
    end

	% compute intensities
    vFactors(t) = vFactor;
	vLambda = vEta + mAlpha * vFactor;
	vExpLambda = exp(vLambda);

	% set exposures
    mR(:,t) = vR;

	% generate spell length (duration between events)
	vTau(t) = -log(1-rand(1,1))/(mR(:,t)'*vExpLambda);
    % total amount of cumulated time
	dSpellTotal = dSpellTotal + vTau(t);

	% generate mark (i.e. default or no default)
    vProb = mR(:,t).*vExpLambda/(mR(:,t)'*vExpLambda);
	iD = length(find(cumsum(vProb)<rand(1,1))) + 1;
	mY(iD,t) = 1;

	% adjust exposures
    if iD==1
        vR(1,1) = vR(1,1) - 1;
        vR(2,1) = vR(2,1) + 1;
    else
        vR(1,1) = vR(1,1) + 1;
        vR(2,1) = vR(2,1) - 1;
    end
    
	% compute GAS step
	vLmax = max(vLambda);
	vScore = mAlpha'*(mY(:,t)-vTau(t)*mR(:,t).*vExpLambda);
    mInfoMatrix = mAlpha'*((mR(:,t).*exp(vLambda-vLmax)/(mR(:,t)'*exp(vLambda-vLmax))).*mAlpha);
    vSt = mInfoMatrix\vScore;
    
	% Gas recursion
	vFactor = vOmega + mA*vSt + mB*vFactor;

    % increment
    t = t + 1;
end

% trim to proper size
vTau = vTau(1:t);
mY = mY(:,1:t);
mR = mR(:,1:t);
vFactors = vFactors(:,1:t);