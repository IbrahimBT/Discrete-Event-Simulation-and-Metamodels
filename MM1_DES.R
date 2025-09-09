# Function to simulate an M/M/1 queue

simulate_queue <- function(arrival_rate, service_rate, n_customers) {
  interarrival_times <- rexp(n_customers, rate = arrival_rate)
  arrival_times <- cumsum(interarrival_times)
  
  service_times <- rexp(n_customers, rate = service_rate)
  start_service_times <- numeric(n_customers)
  end_service_times <- numeric(n_customers)
  wait_times <- numeric(n_customers)
  
  start_service_times[1] <- arrival_times[1]
  end_service_times[1] <- start_service_times[1] + service_times[1]
  wait_times[1] <- 0
  
  for (i in 2:n_customers) {
    start_service_times[i] <- max(arrival_times[i], end_service_times[i-1])
    wait_times[i] <- start_service_times[i] - arrival_times[i]
    end_service_times[i] <- start_service_times[i] + service_times[i]
  }
  
  return(mean(wait_times))  # Return average waiting time
}


# Input Parameters
arrival_rate <- 1                 # Fixed arrival rate (λ)
service_means <- seq(0.7, 0.95, by = 0.05)  # Varying mean service times
n_customers <- 5000              # Number of customers per simulation
n_replications <- 3              # Replications per setting


# Run Simulations
set.seed(42)  
results <- data.frame()

for (mu in service_means) {
  for (rep in 1:n_replications) {
    avg_wait <- simulate_queue(
      arrival_rate = arrival_rate,
      service_rate = 1 / mu,  # rate = 1 / mean
      n_customers = n_customers
    )
    results <- rbind(results, data.frame(
      service_time = mu,
      avg_wait_time = avg_wait
    ))
  }
}

