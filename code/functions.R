
# Equation 1 --------------------------------------------------------------
# Environmental fluctuation
# E: environmental factor
# t: time
# A: amplitude
# T: period
# D: magnitude of the stochastic component
# xi: variation of the stochastic component

generate.E <- function(mu, A, t, T, D) {
  xi <- rnorm(length(t), 0, 1)
  mu + A * sin(2 * pi * t / T) + D * xi
}




# Equation 3 --------------------------------------------------------------
# Fecundity
# Beverton-Holt type
# lambda: maximum growth rate
# a: density dependency
# N: previous population size
M <- function(lambda, a, N) {
  lambda / (1 + a * N)
}




# Equation 4 --------------------------------------------------------------
# Immature's environmental response (sigmoidal/Hill function)
# sigma: maximal fraction of the diapausers
# c: slope in the transitional phase (larger c = more abrupt response)
# P: threshold/inflection point of environment
fE <- function(sigma, c, P, E) {
  sigma * (1 - (1 / (1 + exp(c * (P - E)))))
}




# Equation 2 --------------------------------------------------------------
# Population dynamics 
# seems to be annual species
fI <- function(sigma, c, P, E, I, lambda, a, N) {
  fE(sigma, c, P, E) * I + M(lambda, a, N) * N
}

fN <- function(s, sigma, c, P, E, I) {
  s * (1 - fE(sigma, c, P, E)) * I
}





# Fig. 3 ------------------------------------------------------------------
fig3.func <- function(D, warmup, post.warmup) {
  times <- seq(0, warmup + post.warmup, 1)
  E <- generate.E(mu = 2, A = 0.05, t = times, T = 11, D = D)
  # plot(times, E, type = "l")
  
  # couldn't find what their initial I or N was
  I0 <- 1000
  N0 <- 0
  
  I <- numeric(length(times))
  N <- numeric(length(times))
  I[1] <- I0
  N[1] <- N0
  
  for (t in 1:(length(times)-1)) {
    I[t + 1] <- 
      fI(
        sigma = 0.99,
        c = 30,
        P = 2.15,
        E = E[t],
        I = I[t],
        lambda = 2,
        a = 0.001,
        N = N[t]
      )
    
    N[t + 1] <- 
      fN(
        s = 0.55,
        sigma = 0.99,
        c = 30,
        P = 2.15,
        E = E[t],
        I = I[t]
      )
  }
  
  return(list(times = times, I = I, N = N, E = E))
}





# Fig. 4 ------------------------------------------------------------------
extract.outbreak <- function(D, theta = 0.05, warmup, post.warmup) {
  sim <- fig3.func(D = D, warmup = warmup, post.warmup = post.warmup)$N
  outbreak <- numeric(length(sim))
  outbreak[1] <- NA
  for (t in 2:length(sim)) {
    outbreak[t] <- 
      ifelse(sim[t-1] / sim[t] < theta,
             sim[t],
             0) 
  }
  
  return(outbreak)
}
