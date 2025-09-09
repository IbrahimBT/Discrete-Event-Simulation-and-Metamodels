# 1. Libraries, mlegp for gp
library(mlegp) 
library(ggplot2)


# 2. Define the M/M/1 Queue Simulator
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
  
  return(mean(wait_times))
}


# 3. Run Simulation Experiments
arrival_rate <- 1
service_means <- seq(0.7, 0.95, by = 0.05)
n_customers <- 5000
n_replications <- 3

set.seed(42)
results <- data.frame()

for (mu in service_means) {
  for (rep in 1:n_replications) {
    avg_wait <- simulate_queue(
      arrival_rate = arrival_rate,
      service_rate = 1 / mu,
      n_customers = n_customers
    )
    results <- rbind(results, data.frame(
      service_time = mu,
      avg_wait_time = avg_wait
    ))
  }
}


#4. Fit Gaussian Process Model 


x_gp <- matrix(results$service_time, ncol = 1)
y_gp <- results$avg_wait_time

gp_model <- mlegp(x_gp, y_gp)


# 5. Predict Over Grid


x_pred <- matrix(seq(0.7, 0.95, length.out = 100), ncol = 1)
gp_pred <- predict(gp_model, x_pred, se.fit = TRUE)

gp_results <- data.frame(
  service_time = x_pred[, 1],
  mean_wait = gp_pred$fit,
  upper = gp_pred$fit + 2 * gp_pred$se.fit,
  lower = gp_pred$fit - 2 * gp_pred$se.fit
)
gp_fitted <- predict(gp_model, x_gp)
mse <- mean((y_gp - gp_fitted)^2)
cat("Mean Squared Error (MSE):", round(mse, 4), "\n")


# 6. Plot the GP Fit
ggplot() +
  geom_point(data = results, aes(x = service_time, y = avg_wait_time), color = "black") +
  geom_line(data = gp_results, aes(x = service_time, y = mean_wait), color = "blue", size = 1) +
  geom_ribbon(data = gp_results, aes(x = service_time, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    title = "Gaussian Process Metamodel",
    x = "Mean Service Time",
    y = "Average Wait Time"
  )

