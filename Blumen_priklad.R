First_x <- c(20.4,20.3,19.5,18,30.3,12.7,23.8,32,3.2,21.7,24.8,24.6,21.8,23.8,27)
First_y <- c(15.7,29.1,30.3,28.9,17.6,21.8,16.2,16.8,13.8,22.3,20.9,8.7,25.8,7.7,14.4)
Second_x <- c(10.7,33.7,35.6,40,12.5,24.3,43.4,16.3,25.3,33.4,15.8, 21.1, 34.5, 33.4, 38.7)
Second_y <- c(35.5, 3.4, 20.5, 30.2, 26.4, 22.6, 33.2, 39.4,23.9, 42.1, 21.8, 35.1, 24.5, 36.1, 21.1)
dif_x <- Second_x-First_x
dif_y <- Second_y- First_y
rat <- dif_y/dif_x
data<-data.frame(First_x,First_y,Second_x,Second_y,dif_x,dif_y,rat)
n <- 15


plot(dif_x, dif_y, main="Dátové body", 
     xlab="x", ylab="x", pch=19, col="blue")
abline(h=0, v=0, col="black", lwd=2)
for(i in 1:n){abline(a=0,b=rat[i],col='red',lwd=1)}

# Create a sequence of angles from 0 to 2π
uhol <- seq(0, 2*pi, length.out=100)

# Parametric equations for the unit circle
alpha <- cos(uhol)  # X coordinates
beta <- sin(uhol)  # Y coordinates

rescale_to_unit_circle <- function(df) {
  # Calculate the distance from the origin for each point
  distances <- sqrt(df$dif_x^2 + df$dif_y^2)
  
  # Rescale each point to the unit circle
  df_rescaled <- df
  df_rescaled$dif_x <- df$dif_x / distances
  df_rescaled$dif_y <- df$dif_y / distances
  
  return(df_rescaled)
}

rescaled_points <- rescale_to_unit_circle(data.frame(data[,5:6]))

# Plot the unit circle
plot(alpha, beta, type="l", col="blue", asp=1, 
     xlab="x", ylab="y", main="Dátové body premietnuté na kružnicu")
abline(h=0, v=0, col="black", lwd=2)
for(i in 1:n){abline(a=0,b=rat[i],col='red',lwd=1)}
points(rescaled_points, col='blue',pch=19)


#usortime zaznamy podla rastuceho uhla vzhladom na os x
data_pos <- data[rat>0,]
data_pos<- data_pos[order(data_pos$rat),]
data_neg <- data[rat<0,]
data_neg <-data_neg[order(data_neg$rat),]

data_sort <- rbind(data_pos,data_neg)
for(i in 1:n){abline(a=0,b=rat[i],col='red',lwd=1)}

# Plot the unit circle
plot(alpha, beta, type="l", col="blue", asp=1, 
     xlab="x", ylab="y", main="Dátové body premietnuté na kružnicu,\n rovnomerné uhly")
abline(h=0, v=0, col="black", lwd=2)
angles <- seq(0, pi, length.out = n + 1)
i<- 1
for (angle in angles) {
  x_end <- 1 * cos(angle)
  y_end <- 1 * sin(angle)
  a <- sign(data_sort$dif_y[i])
  i<- i+1

  segments(-x_end, -y_end, x_end, y_end, col="red", lwd=1)
  points(a*x_end,a*y_end, col='blue',pch=19)
}


#clanok ma numericke chyby
data_clanok <- data
data_clanok[5,7]<--0.78
data_clanok[15,7]<--0.83

data_pos_cl <- data_clanok[rat>0,]
data_pos_cl<- data_pos_cl[order(data_pos_cl$rat),]
data_neg_cl <- data_clanok[rat<0,]
data_neg_cl <-data_neg_cl[order(data_neg_cl$rat),]

data_sort_cl <- rbind(data_pos_cl,data_neg_cl)

#rat_sort <- merge(sort(rat[rat>0]),sort(rat[rat<0]))

v1 <- 0
v2 <- 0
a<-0

for(i in 0:(n-1)){

  if(data_sort$dif_y[i+1]>0){a <- 1}
  else{a <- (-1)}
  v1 <- v1 + a*cos(pi*(i/n))
  v2 <- v2 + a*sin(pi*(i/n))
}

v_2 <- 2*(v1^2 + v2^2)/n

v1_cl <- 0
v2_cl <- 0

for(i in 0:(n-1)){
  
  if(data_sort_cl$dif_y[i+1]>0){a <- 1}
  else{a <- (-1)}
  v1_cl <- v1_cl + a*cos(pi*(i/n))
  v2_cl <- v2_cl + a*sin(pi*(i/n))
}

v_2_cl <- 2*(v1_cl^2 + v2_cl^2)/n
