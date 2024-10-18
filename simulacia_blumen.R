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
  m <- data_points[,2]/data_points[,1]
  
  data <- data.frame(cbind(data_points,m))
  
  data_pos <- data[m>0,]
  data_pos<- data_pos[order(data_pos$m),]
  data_neg <- data[m<0,]
  data_neg <-data_neg[order(data_neg$m),]

  data_sort <- rbind(data_pos,data_neg)
  
  
  #testove statistiky vynulujeme
  S1 <- 0
  S2 <- 0
  a <- 0
  
  for(bod in 1:(n[pokus])){
    
    a <- sign(data_sort[bod,2])
    #print(a)
    S1 = S1 + a*cos(pi*(bod/n[pokus]))
    S2 = S2 + a*sin(pi*(bod/n[pokus]))

  }
  #S2 <- S2/n[pokus]
  #S1 <- S1/n[pokus]

  Z <- (S1^2 + S2^2)*(2/n[pokus])
  stats <- c(stats,Z)
}

ks_test <- ks.test(stats, "pchisq", df = 2)
ks_test$p.value

hist(stats, probability = TRUE, breaks=60, main = "Histogram s chí-kvadrát hustotou", xlab = "Value", 
     col = "lightblue", border = "black")

# Create a sequence of x-values for the CDF
x_vals <- seq(0, 40, length = 20000)

# Add the CDF of the Chi-squared distribution (df = 2)
lines(x_vals, dchisq(x_vals, df=2), col = "red", lwd = 2)

# Add a legend
legend("right", legend = c("Chi-Squared PDF (df=2)"), col = "red", lwd = 2)
