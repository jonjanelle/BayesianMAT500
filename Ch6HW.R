
#Use a linear combination of three equally weighted betas for 6.2 and 6.3 prior
x<-seq(0,1,by = .001)
b1 <- .001*(1/3*dbeta(x,2,18) + 1/3*dbeta(x, 40, 40) + 1/3*dbeta(x,18,2))
plot(x, b1, xlab = expression(theta), ylab = expression(paste("p(",theta,") Mass")),
  main = expression(paste("Prior Probability Masses for ", theta)))

#6.2 Plot formatting stuff
polygon(c(x[1:200],.2),c(b1[1:200],0), col = "blueviolet")
polygon(c(.4,x[400:600],.6),c(0,b1[400:600],0), col = "yellow")
polygon(c(.8,x[800:1000],1),c(0,b1[800:1000],0), col = "darkslategray1")
text(.08, .0001,expression(sum(paste("p(",theta,")=0.305"),theta)))
text(.48, .0001,expression(sum(paste("p(",theta,")=0.310"),theta)))
text(.88, .0001,expression(sum(paste("p(",theta,")=0.306"),theta)))

#6.2 Update prior using data with 15 heads, 5 tails
post = BernGrid( x , b1 , c(rep(1,15),rep(0,5)) )

#6.3A
post = BernGrid( x , b1 , c(rep(1,3),rep(0,1)) )
post1 = BernGrid(x,post,c(rep(1,12),rep(0,4)) )
# calculate the probability mass over an interval
prob = 0
for(i in 800:1000){
  prob = prob+.001*b1[i]  
  
}
sum(b1[1:1000])

#6.4
#Generate uniform pmf with 1000 bins on [0,1]
x<-seq(0,1,by = 0.001)
prior = rep(1/length(x),length(x))
sum(prior)
plot(x,prior, main = expression(paste("Prior Probability Mass of ",theta)), 
     xlab=expression(theta), ylab = expression(paste("p(",theta,")"))) 
post <- BernGrid(x,prior,c(rep(0,42),rep(1,58)))
#Show area theta > 0.5, the preference for candidate A region
plot(x,post, main = "Posterior", xlab = expression(theta), 
     ylab = expression(paste("p(",theta,"|D)")))
polygon(c(0.5,x[x>0.5],1),c(0,post[x>0.5],0), col = "blueviolet")
posthalf = sum(post[x>0.5])
text(.8, .002,expression(sum(paste("p(",theta,")=0.943"),paste(theta,">0.5"))))

#6.4C
x<-seq(0,1,by = 0.001)
prior = rep(1/length(x),length(x))
post <- BernGrid(x,prior,c(rep(0,42),rep(1,58)))
post1 <- BernGrid(x,post,c(rep(0,43),rep(1,57)))

#6.7
dx  = 0.001
par(mfrow = c(1,2))
#Midpoint approx
thetagrid = seq( from=dx/2 , to=1-dx/2 , by=dx )
weight1 = thetagrid^2
prior1 = weight1/sum(weight1)
plot(thetagrid,prior1, xlab = expression(theta), 
     ylab = expression(paste("p(",theta,")")), main = "Prior Model 1")
BernGrid(thetagrid,prior1,c(rep(0,2),rep(1,6)))

weight2 = (1-thetagrid)^2
prior2 = weight2/sum(weight2)
plot(thetagrid,prior2, xlab = expression(theta), 
     ylab = expression(paste("p(",theta,")")), main = "Prior Model 2")
BernGrid(thetagrid,prior2,c(rep(0,2),rep(1,6)))

#Right endpoint approx
#x = seq(0,1,by = dx)
#weight = x^2
#prior = weight/sum(weight)
#plot(x,prior)
