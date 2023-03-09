# --- FA 5 -- #

probabilities <- c

dGeom <- function(x, prob) {
  psuccess <- prob
  var <- x  + 1
  
  pfailed <- (1-psuccess)**(var-1)
  result <- psuccess * pfailed
  
  return(result)
}

dGeom(4, 0.03)

rGeom <- function(x, prob) {
  q <- 1-prob
  prob_set <- c()
  sample_set <- c()
  
  #Cumulative probabilities & Sample Set
  if(length(x) > 1) {
    for (i in (x+1)) {
      prob_set[i] <- (1 - q**i)
    }
    sample_set <- sample(c(x), length(x), replace = T, prob = prob_set)
    
  } else {
    for (i in 1:(x+1)) {
      prob_set[i] <- (1 - q**i)
    }
    sample_set <- sample(c(1:(x+1)), x, replace = T, prob = prob_set)
  }
  probabilities <- sample_set
  return(sample_set)
}

n <- 1000
prob <- 0.2
result <- rGeom(n, prob)

# Mean and Median
rGeom_mean <- mean(result)
rGeom_mean

rGeom_var <- var(result)
rGeom_var

rGeom_sd <- sd(result)
rGeom_sd

par(mfrow = c(1,2), bg="white")
plot(table(result + 1)/length(result + 1), main="1000 Random Distribution Plot", xlab = "Random Variable = 1000", ylab = "Relative Freq", type = "h")
hist(result, main="1000 Random Distribution Hist", xlab = "Random Variable = 1000", ylab = "Relative Freq")