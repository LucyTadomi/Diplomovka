x1 <- c(170,125,148,140,106,108,124,134,116,114,118,138,134,124,114)
x2 <- c(76,75,120,78,72,62,70,64,76,74,68,78,86,64,66)

n <-length(x1)

#odcitame stredne hodnoty
x <- x1-120
y <- x2-80


#spocitame pomer y/x a vyjadrime arctg
m <- y/x
theta <- atan(m)

rank <- rank(theta)
#najdime poradie v ktorom budeme data aplikovat
theta_shift <- theta
theta_shift[theta<0] <- theta[theta<0]+pi
poradie <- rank(theta_shift)

data<- data.frame(x,y,theta,rank,poradie)


S1 <- 0
S2 <- 0
a <- 0

for(i in 1:(n)){
  
  if(data$y[i]>0){
    a <- 1
  }
  else{
    a <- -1
  }
  S1 = S1 + a*cos(pi*(data$poradie[i]/n))*data$rank[i]
  S2 = S2 + a*sin(pi*(data$poradie[i]/n))*data$rank[i]
}

S1 <- S1/n
S2 <- S2/n

