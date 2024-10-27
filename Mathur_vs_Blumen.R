#########
#Rank sa urcuje od -pi/2,pi/2, usporiadanie priamok je medzi 0,pi

x <- c(-0.2,-0.3,-0.4,-0.25,-0.45,-0.3,0.3,-0.1,-0.56131581, -0.08954324, -0.82218483,-0.5)
y <- c(0.7,0.9,0.2,0.3,0.75,0.6,0.4,-1,0.37208361, 0.08238545, 0.47722007,-0.5)
n <-length(x)
#spocitame pomer y/x a vyjadrime arctg
m <- y/x
theta <- atan(m)

plot(x, y, main="Dátové body", 
     xlab="x", ylab="x", xlim=c(-1,1),ylim=c(-1,1), pch=19, col="blue")
abline(h=0, v=0, col="black", lwd=2)
for(i in 1:n){abline(a=0,b=m[i],col='red',lwd=1)}


##Mathur
rank <- rank(theta)
#najdime poradie v ktorom budeme data aplikovat
theta_shift <- theta
theta_shift[theta<0] <- theta[theta<0]+pi
poradie <- rank(theta_shift)

data<- data.frame(x,y,theta,rank,poradie)

vars1 <-0
vars2 <- 0

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
  
  vars1 <- vars1 + cos(2*pi*(data$poradie[i]/n))*(data$rank[i]^2)
  vars2 <- vars2 + (-cos(2*pi*(data$poradie[i]/n)))*(data$rank[i]^2)
}

S1 <- S1/n
S2 <- S2/n

vars1 <- vars1*(1/(2*n^2)) +(n/6)*(1+1/n)*(1+1/(2*n))
vars2 <- vars2*(1/(2*n^2)) +(n/6)*(1+1/n)*(1+1/(2*n))

Z <- S1^2/vars1 + S2^2/vars2
# Z = 4.91, c.v. chi^2 df2 = 5.99, H0 nezamietame

#Blumen

data <- data.frame(x,y,m)
#usortime zaznamy podla rastuceho uhla vzhladom na os x
data_pos <- data[m>0,]
data_pos<- data_pos[order(data_pos$m),]
data_neg <- data[m<0,]
data_neg <-data_neg[order(data_neg$m),]

data_sort <- rbind(data_pos,data_neg)

v1 <- 0
v2 <- 0
a<-0

for(i in 0:(n-1)){
  
  if(data_sort$y[i+1]>0){a <- 1}
  else{a <- (-1)}
  v1 <- v1 + a*cos(pi*(i/n))
  v2 <- v2 + a*sin(pi*(i/n))
}

v_2 <- 2*(v1^2 + v2^2)/n
#V2 = 8.5, H0 zamietame
