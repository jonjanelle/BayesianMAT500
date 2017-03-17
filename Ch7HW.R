binwidth = 1/2000
thetagrid = seq( from=binwidth/2 , to=1-binwidth/2 , by=binwidth )
relprob = ( cos( 4*pi*thetagrid ) + 1 )^2
prior = relprob / sum(relprob)
datavec = c( rep(1,8) , rep(0,4) )
posterior = BernGrid( Theta=thetagrid , pTheta=prior , Data=datavec )

