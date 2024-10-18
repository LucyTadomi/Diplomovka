library(MASS)

#kolko testov chceme urobit?
N <- 10000

#kolko permutacii budeme chciet pocitat?
N_perm <- 1000

#ako velke subory budeme chciet testovat? 15 pouzil Mathur aj Blumen
n <- 15

cvMathur <-18.08 #kriticka hodnota pre n = 15 z Mathurovho clanku

zamMathur <- vector(length=N)
zamPerm <- vector(length=N)


stats <- numeric(0)

pokus<-1
for(pokus in 1:N){
  
  #vyrobme data pre dany pokus
  mu <- c(0, 0)  # vycentrovane data
  Sigma <- matrix(c(1, 0.3, 0.3, 1), 2, 2)  # nech su nase dve premenne zavisle
  data_points <- mvrnorm(n, mu = mu, Sigma = Sigma)
  
  #najdime ranky
  theta <- atan(data_points[,2]/data_points[,1])
  rank <- rank(theta)
  #najdime poradie v ktorom budeme data aplikovat
  theta_shift <- theta
  theta_shift[theta<0] <- theta[theta<0]+pi
  poradie <- rank(theta_shift)
  
  #testove statistiky vynulujeme
  S1 <- 0
  S2 <- 0
  a <- 0
  
  for(bod in 1:(n)){
    i <- poradie[bod]
    
    a <- sign(data_points[bod,2])
    S1 = S1 + a*cos(pi*(i/n))*rank[bod]
    S2 = S2 + a*sin(pi*(i/n))*rank[bod]
  
  }
  S1 <- S1/n
  S2 <- S2/n

  S <- S1^2 + S2^2
  
  stats <- c(stats,S)
  
  zamMathur[pokus] <- sign(S-cvMathur) #ak zamietame, hodnota je 1
  
  #podme pridat permutacie
  stats_perm <- vector(length=N_perm)
  for(p in 1:N_perm){
    permutacia <- sample(c(1,-1),n,replace=TRUE)
    
    #poradie aj ranky zostavaju rovnake
    S1_perm <- 0
    S2_perm <- 0
    a <- 0
    
    for(bod in 1:(n)){
      i <- poradie[bod]
      
      a <- sign(data_points[bod,2])*permutacia[bod]
      S1_perm = S1_perm + a*cos(pi*(i/n))*rank[bod]
      S2_perm = S2_perm + a*sin(pi*(i/n))*rank[bod]
      
    }
    S1_perm <- S1_perm/n
    S2_perm <- S2_perm/n
    
    S_perm <- S1_perm^2 + S2_perm^2
    stats_perm[p] <- S_perm
    
  }
  zamPerm[pokus]<- sign(S-quantile(stats_perm,0.95))
  
}

