require(R2jags)
require(MASS)

drop <- read.csv(file.choose()) 
str(drop)
head(drop)

#Convert FR lunch + LEP to proportions
for (i in 5:6) {drop[,i]<-drop[,i]/drop[,7]}

#make some better column names
cnames <- c("state","PTR", "TRev","FGrad","FRLun","LEP", "Total","TotalState","PopDens")
colnames(drop) <- cnames
str(drop)

#BoxCox transformation suggests lambda = 3
modl1=lm(FGrad~PTR+TRev+FRLun+LEP+PopDens,drop)
summary(modl1)
fgrad3 <- drop$FGrad^3

# calculate summary stats for future use
ybar=mean(fgrad3)
sd.y=sd(fgrad3)
xbar=colMeans(drop[,c(2,3,5,6,9)])
sd.x=apply(drop[,c(2,3,5,6,9)],2,sd)
# fit the usual least squares regression model



# version 1 usual approach no standardization.
reg.mdl=function() {
  # likelihood
  for (i in 1:nData) {
    y[i]~dnorm(mu[i],tau)
    mu[i] <- inprod(B[],X[i,])
  }
  #priors
  for (j in 1:nParms) {
    B[j]~dnorm(0,1E-6)
  }
  tau ~ dgamma(0.01,0.01)
  sigma <- 1/sqrt(tau)
}
reg.data=with(drop,list(y=fgrad3,X=cbind(1,PTR,TRev,FRLun,LEP,PopDens),
                           nData=nrow(drop),nParms=6))
reg.inits=function() list(tau=rgamma(1,0.1,0.1),B=rnorm(6,0,10))
nChains=3
burnIn=5000
nIter=10000
nThin=1
params=c('sigma','B')
reg.fit=jags(reg.data,reg.inits,params,reg.mdl,nChains,nIter,burnIn,nThin)

print(reg.fit)
plot(reg.fit)
mcmcChain=as.mcmc(reg.fit$BUGSoutput$sims.matrix)
windows()
plot(mcmcChain)
colnames(mcmcChain)
colnames(mcmcChain) <- c("B0","B1","B2","B3","B4","B5","deviance","sigma")

# version 2 center and scale x vars
#fg.cntr=as.data.frame(scale(drop[,2:5],center=T,scale=T))

#reg.data=with(fg.cntr,list(y=fgrad3,X=cbind(1,PTR,TRev,FRLun,LEP),
#                            nData=nrow(drop),nParms=5))
#reg.fit=jags(reg.data,reg.inits,params,reg.mdl,nChains,nIter,burnIn,nThin)

#print(reg.fit)
#windows()
#plot(reg.fit)
#mcmcChain=as.mcmc(reg.fit$BUGSoutput$sims.matrix)
#plot(mcmcChain)


# version 3 different prior for error structure
reg.mdl=function() {
  # likelihood
  for (i in 1:nData) {
    y[i]~dnorm(mu[i],tau)
    mu[i] <- inprod(B[],X[i,])
  }
  #priors
  for (j in 1:nParms) {
    B[j]~dnorm(0,1E-6)
  }
  sigma~dunif(0,100)
  tau<- 1/sigma^2
}
reg.inits=function() list(sigma=runif(1,0,10),B=rnorm(6,0,10))
reg.fit=jags(reg.data,reg.inits,params,reg.mdl,nChains,nIter,burnIn,nThin)

print(reg.fit)
windows()
plot(reg.fit)
mcmcChain=as.mcmc(reg.fit$BUGSoutput$sims.matrix)
windows()
colnames(mcmcChain) <- c("B0","B1","B2","B3","B4","B5","deviance","sigma")
plot(mcmcChain)

B=mcmcChain[,1:6]
# compute the non standardized parameter estimates
b0=B[,1]*sd.y +ybar-B[,2:6]%*%(sd.y*xbar/sd.x)
bp=B[,2:6]*matrix(rep(sd.y/sd.x,nrow(B)),nrow=nrow(B),byrow=T)
betas.mod=cbind(b0,bp)
sig.orig=sd.y*mcmcChain[,'sigma']
coef(modl1)
colMeans(betas.mod)
sqrt(sum(resid(modl1)^2)/modl1$df.residual)
mean(sig.orig)

betas.mod=cbind(B[,1],B[,2:6])
# examine correlation structure of parameter ests.
round(cov(betas.mod),3)
round(vcov(modl1),3)
round(cov(B),3)
round(cor(betas.mod),3)
round(cor(B),3)


# checking model fit - residual analysis
beta.bar=colMeans(betas.mod)
mod.fitted=cbind(1,as.matrix(drop[,c(2,3,5,6,9)]))%*%beta.bar
mod.resid=fgrad3-mod.fitted
windows()
plot(density(mod.fitted))

par(mfrow=c(2,2))
plot(mod.resid~mod.fitted,main='Bayesian Model Residuals Vs. Fitted')
abline(h=0,lty=2)
windows()
par(mfrow=c(1,2))
plot(modl1,which=1)
qqnorm(mod.resid)
qqline(mod.resid)
plot(modl1,which=2)

# different approach to fitted values
mod.fit2=cbind(1,as.matrix(sav.cntr[,2:5]))%*% colMeans(B)
y.fit2=sd.y*mod.fit2+ybar
plot(y.fit2,mod.fitted)

# predicting new means
new.data=as.matrix(savings[sample(1:nrow(savings),5),2:5])
new.data

y.hat=cbind(1,new.data)%*%t(betas.mod)
dim(y.hat)
t(apply(y.hat,1,quantile,c(.025,.50,.975)))
predict(modl1,as.data.frame(new.data),interval='conf')[,c(2,1,3)]
