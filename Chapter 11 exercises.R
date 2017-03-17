#Ch 11 Kruschke 
###############
#   11.2      #
###############
#A
N <- 26
z <- 8
theta <- 0.144

x<-c()
y<-c()
for( theta in seq( .140 , .150 , .001 ) ) {
   x <- c(x, theta )
   y <- c(y, sum( dbinom( 8:26 , 26 , theta ) ) )
}
windows()
plot(x,y, type = "l", xlab = expression(theta), 
     main = "P(z >= 8) in Binominal Distribution \nwith N = 26", ylab = expression(paste("P(z >= 8|", theta,")")), 
     xlim = c(0.140, 0.145))
abline(v = 0.144, col = "blue", lty = 2)
abline(h = 0.025, col = "red", lty = 2)
text(0.141,0.0253,"P(z >= 8) = 0.025", col = "red")

#B

theta <- 0.517
x<-c()
y<-c()
for( theta in seq( .515 , .520 , .001 ) ) {
  x <- c(x, theta )
  y <- c(y, sum( dbinom( 1:8 , 26 , theta ) ) )
}
windows()
plot(x,y, type = "l", xlab = expression(theta), 
     main = "P(z <= 8) in Binominal Distribution \nwith N = 26", ylab = expression(paste("P(z <= 8|", theta,")")), 
     xlim = c(0.515, 0.520))
abline(v = 0.517, col = "blue", lty = 2)
abline(h = 0.025, col = "red", lty = 2)
text(0.141,0.0253,"P(z >= 8) = 0.025", col = "red")

for( theta in seq( .515 , .520 , .001 ) ) {
show( c( theta , sum( dbinom( 1:8 , 26 , theta ) ) ) )
}

for( theta in seq( .140 , .145 , .001 ) ) {
  show( c( theta , sum( dbinom( 1:8 , 26 , theta ) ) ) )
}


#11.2D
z <-8
x <- seq(.140, .145, by = 0.001)
for (theta in x ){
  prob <- 0
  for (i in 8:26) {
    prob = prob + choose(i-1, z-1)*theta^z*(1-theta)^(i-z)
  }
  show(c(round(theta,3), prob))
}

x <- seq(.490, .495, by = 0.001)
for (theta in x ){
  prob <- 0
  for (i in 8:25) {
    prob = prob + choose(i-1, z-1)*theta^z*(1-theta)^(i-z)
  }
  show(c(round(theta,3), 1-prob))
}

###############
#   11.3      #
###############
#A
sum( dbinom( 26:39 , 39 , .5 ) )

#B
z_obs = 26 ; N_obs = 39
nulltheta = .5
tail_prob = 0 # Zero initial value for accumulation over possible N.
for ( N in 1 : (3*N_obs) ) { # Start at 1 to avoid /0. 3*N_obs is arbitrary.
  # Create vector of z values such that z/N >= z_obs/N_obs
  zvec = (0:N)[ (0:N)/N >= z_obs/N_obs ]
  tail_prob = tail_prob + (
    dpois( N , N_obs ) * sum( dbinom( zvec , N , nulltheta ) ) )
}
show( tail_prob )