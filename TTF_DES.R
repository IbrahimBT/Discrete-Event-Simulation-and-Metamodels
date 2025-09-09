# Simulate time to first system failure (unlimited lifetimes + repair)
set.seed(42)
simulate_ttf <- function() {
  lifetime_values <- 1:6
  repair_time <- 2.5  # repair time
  clock <- 0
  state <- 2  # two components working
  next_repair <- Inf
  
  # Initial lifetimes for two components
  failure_times <- c(clock + sample(lifetime_values, 1),
                     clock + sample(lifetime_values, 1))
  
  while (TRUE) {
    next_failure <- min(failure_times)
    
    if (next_failure < next_repair) {
      # A component fails before repair completes
      clock <- next_failure
      failure_times <- failure_times[failure_times != next_failure]
      
      if (state == 2) {
        # First failure: enter repair mode
        state <- 1
        next_repair <- clock + repair_time
        failure_times <- c(failure_times, clock + sample(lifetime_values, 1))  # other component keeps running
        
      } else if (state == 1) {
        # Second failure --> system fails
        return(clock)
      }
      
    } else {
      # Repair completes
      clock <- next_repair
      next_repair <- Inf
      state <- 2
      # Repaired component is back --> both components need new lifetimes
      failure_times <- c(clock + sample(lifetime_values, 1),
                         clock + sample(lifetime_values, 1))
    }
  }
}

begin = Sys.time()
n_simulations <- 1000
ttf_results <- replicate(n_simulations, simulate_ttf())

end = Sys.time()

print (end - begin)

# Plot
hist(ttf_results,
     breaks = 15,
     col = "lightblue",
     main = "Time to First System Failure ",
     xlab = "Time to Failure (days)",
     border = "white")

lines(density(ttf_results), col = "blue", lwd = 2)
abline(v = mean(ttf_results), col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c("Density", "Mean TTF"),
       col = c("blue", "red"),
       lwd = 2,
       lty = c(1, 2))


cat("Mean Time to Failure:", mean(ttf_results), "\n")

