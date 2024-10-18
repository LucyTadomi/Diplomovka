library(MASS)

rescale_to_unit_circle <- function(df) {
  # Calculate the distance from the origin for each point
  distances <- sqrt(df[,1]^2 + df[,2]^2)
  
  # Rescale each point to the unit circle
  df_rescaled <- df
  df_rescaled[,1] <- df[,1] / distances
  df_rescaled[,2] <- df[,2] / distances
  
  return(df_rescaled)
}

#kolko testov chceme urobit?

N <- 10000

#ako velke subory budeme chciet testovat? 15 pouzil Mathur aj Blumen

n_min <- 30
n_max <- 60

n <- sample(n_min:n_max, N, replace = TRUE)

stats <- numeric(0)

pokus<-1
for(pokus in 1:N){
  
  #vyrobme data pre dany pokus
  mu <- c(0, 0)  # vycentrovane data
  Sigma <- matrix(c(1, 0.3, 0.3, 1), 2, 2)  # nech su nase dve premenne zavisle
  data_points <- mvrnorm(n[pokus], mu = mu, Sigma = Sigma)
  
  #najdime ranky
  #m <- data_points[,1]/data_points[,2]
  theta <- atan(data_points[,2]/data_points[,1])
  rank <- rank(theta)
  #najdime poradie v ktorom budeme data aplikovat
  theta_shift <- theta
  theta_shift[theta<0] <- theta[theta<0]+pi
  poradie <- rank(theta_shift)
  
  #pre kontrolu najdime priemet bodov na jednotkovu kruznicu
  rescaled_points <- rescale_to_unit_circle(data_points)
  
  #testove statistiky vynulujeme
  S1 <- 0
  S2 <- 0
  a <- 0
  vars1_part <- 0
  vars2_part <- 0
  vars1 <-0
  vars2 <-0
  
  for(bod in 1:(n[pokus])){
    i <- poradie[bod]
  
    a <- sign(data_points[bod,2])
    S1 = S1 + a*cos(pi*(i/n[pokus]))*rank[bod]
    S2 = S2 + a*sin(pi*(i/n[pokus]))*rank[bod]
    
    #vars1_part <- vars1_part + cos(2*pi*(i/n[pokus]))*(rank[bod]^2)
    #vars2_part <- vars2_part + sin(2*pi*(i/n[pokus]))*(rank[bod]^2)
    #print(vars1_part)
    #vars2_part
    
    vars1_part <- vars1_part + cos(pi*(i/n[pokus]))^2 * rank[bod]^2
    vars2_part <- vars2_part + sin(pi*(i/n[pokus]))^2 * rank[bod]^2
  }
  S1 <- S1/n[pokus]
  S2 <- S2/n[pokus]
  
  #vars1 <- (1/2*(n[pokus]^2))*vars1_part + (n[pokus]/6)*(1+1/n[pokus])*(1+1/2*n[pokus]) #HAHA, toto vychadza zaporne
  #vars2 <- (1/2*(n[pokus]^2))*vars2_part + (n[pokus]/6)*(1+1/n[pokus])*(1+1/2*n[pokus])
  
  #Z1 <- S1/sqrt(vars1)
  #Z2 <- S2/sqrt(vars2)
  #Z <- Z1^2 + Z2^2
  
  vars1 <- vars1_part / (n[pokus]^2)
  vars2 <- vars2_part/(n[pokus]^2)
  Z1 <- S1/sqrt(vars1)
  Z2 <- S2/sqrt(vars2)
  Z <- Z1^2 + Z2^2
  
  #S1_ex <- S1 * (1/sqrt(var(cos(rescaled_points[,1]))))
  #S2_ex <- S2 * (1/sqrt(var(sin(rescaled_points[,2]))))
  #S_ex <- (S1^2)+(S2^2)
  stats <- c(stats,Z)
}

ks_test <- ks.test(stats, "pchisq", df = 2)
ks_test$p.value

hist(stats[stats<30], probability = TRUE, breaks = 40, main = "Histogram s chí-kvadrát hustotou", xlab = "Value", 
     col = "lightblue", border = "black")

# Create a sequence of x-values for the CDF
x_vals <- seq(0, 40, length = 20000)

# Add the CDF of the Chi-squared distribution (df = 2)
lines(x_vals, dchisq(x_vals, df=2), col = "red", lwd = 2)

# Add a legend
legend("right", legend = c("Chi-Squared PDF (df=2)"), col = "red", lwd = 2)
