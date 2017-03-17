x <- seq(0,1,by = 0.01)
#prior
curve(dbeta(x,1,1), xlab = expression(theta), 
      ylab = paste("p(",expression(theta),")"), main = "Prior Belief about Preference for Candidate A")

#Posterior
curve(dbeta(x,59,43), ylab = paste("p(",expression(theta),"| D)"), main = "Posterior Belief about Preference for Candidate A", xlab = expression(theta))
)

#95% HDI
qbeta(c(0.475,0.525),.5,.5)

#Q5.6
#Fair coin model
curve(dbeta(x,100,100), ylab = expression(paste("p(",theta,")")), xlab = expression(theta), main = "Fair Coin Prior (M1)")
#Trick coin model
curve(dbeta(x,0.5,0.5), ylab = expression(paste("p(",theta,")")), xlab = expression(theta), main = "Trick Coin Prior (M2)")