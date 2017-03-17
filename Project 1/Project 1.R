#Specify Prior
#------------------
#
#Vectors to hold the number of 1,2,3,and 4
#ratings for each part from M&H proof frames study
ScoreSeq <- c(13,23,25,40,25,20,22,34,16,16,32,37,11,15,30,45,12,30,25,34)
q1p <- matrix(ScoreSeq,4,5)
#Col order is Single Example, Big Number, Example and non-example,
#Many examples (pattern), and Particular proof (deductive)
q1A <- numeric(5)
#Calculate the total supporting (3 or 4) for each question
for (i in 1:5){q1A[i]<-sum(q1p[3:4,i])}
#N holds the total number of observations
N<- sum(q1p[,i])
#Vector of quantiles for beta priors
x<- seq(0,1,by = 0.001)
#Set up beta priors
for(i in 1:5) { assign(paste("prior",i, sep = ""), dbeta(x,q1A[i],N-q1A[i]))}
priors <- cbind(prior1,prior2,prior3,prior4,prior5)

#Sample size of 55
Ns<-55
#data contains the number of successes
data <- c(28,33,32,42,31)

#Plot posterior/HDI
#######################
par(mfrow = c(2,2))
for(i in 1:4){assign(paste("post",i, sep = ""),
                     BernBeta(c(q1A[i],N-q1A[i]),c(rep(1,data[i]),rep(0,Ns-data[i]))))}

for(i in 1:5){assign(paste("post",i, sep = ""),
                     BernBeta(c(1,1),c(rep(1,data[i]),rep(0,Ns-data[i]))))}
#######################

#Create posterior distributions
post <- dbeta(x,q1A[1]+data[1], Ns+N-q1A[1]-data[1])
for (i in 2:5){
  temppost <- dbeta(x,q1A[i]+data[i], Ns+N-q1A[i]-data[i])
  post <- cbind(post,temppost)
}


#For labeling which argument is being shown in graph. 
labels <- c("A","B","C","D")

windows()
par(mfrow = c(2,2))
for (i in 1:4){
    #Plot prior
  plot(x,priors[,i], type = 'l', ylim = c(0,12), 
       main = paste("Posterior Distribution\n for Argument", labels[i]), 
       col = "red", lwd = 1, ylab=expression(paste("p(",  theta , "|D)" )),
       xlab = expression(theta), lty = 2)
  #Add posterior
  lines(x,post[,i], type = 'l', col = "blue", lwd=2, lty = 1)
  
  legend('topleft',legend=c('Prior','Posterior'),lty=c(2,1), col=c("red","blue"))
  
  #Get HDI
  hpdLim = HDIofICDF( qbeta , shape1=q1A[i]+data[i] , 
                      shape2=Ns+N-q1A[i]-data[i] , credMass=0.95 )
  # Mark the HDI in the posterior.
  hpdHt = mean( c( dbeta(hpdLim[1],q1A[i]+data[i],Ns+N-q1A[i]-data[i]) , 
                   dbeta(hpdLim[2],q1A[i]+data[i],Ns+N-q1A[i]-data[i]) ) )
  lines( c(hpdLim[1],hpdLim[1]) , c(-0.5,hpdHt) , type="l" , lty=2 , lwd=1.5 )
  lines( c(hpdLim[2],hpdLim[2]) , c(-0.5,hpdHt) , type="l" , lty=2 , lwd=1.5 )
  lines( hpdLim , c(hpdHt,hpdHt) , type="l" , lwd=2 )
  text( mean(hpdLim) , hpdHt , bquote( .(100*0.95) * "% HDI" ) ,
        adj=c(0.5,-1.0) , cex=1.5 )
  text( hpdLim[1] , hpdHt , bquote(.(round(hpdLim[1],3))) ,
        adj=c(1.1,-0.1) , cex=1.2 )
  text( hpdLim[2] , hpdHt , bquote(.(round(hpdLim[2],3))) ,
        adj=c(-0.1,-0.1) , cex=1.2 )
}


for (i in 1:4) { 
  temp = c(q1A[i]+data[i],Ns+N-q1A[i]-data[i])
  print(temp)
}

sum(post[x>0.5,2]/sum(post[,2]))