library(fitdistrplus)
library(goftest)

queue_data <- read.csv("C:\\Users\\riya8\\Desktop\\Resume Projects\\Queuing Theory Project\\archive\\simulated_call_centre.csv")
print(queue_data)
typeof(queue_data$call_started)


############### Convert to 24-hour format ##################

queue_data$call_started <- format(
  as.POSIXct(paste("1970-01-01", queue_data$call_started), 
             format = "%Y-%m-%d %I:%M:%S %p"
  ), "%H:%M:%S")


queue_data$call_answered <- format(
  as.POSIXct(paste("1970-01-01", queue_data$call_answered), 
             format = "%Y-%m-%d %I:%M:%S %p"
  ), "%H:%M:%S")


queue_data$call_ended <- format(
  as.POSIXct(paste("1970-01-01", queue_data$call_ended), 
             format = "%Y-%m-%d %I:%M:%S %p"
  ), "%H:%M:%S")

############# Changing the format of the date-time columns to make the subtraction of times easier ##############

queue_data$call_start <- as.POSIXct(
  paste("1970-01-01", queue_data$call_started),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

queue_data$call_answer <- as.POSIXct(
  paste("1970-01-01", queue_data$call_answered),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

queue_data$call_end <- as.POSIXct(
  paste("1970-01-01", queue_data$call_ended),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

print(tail(queue_data))

queue_data$inter_arrival_time <- numeric(length(queue_data$call_id))
for(i in 2:length(queue_data$call_id)){
  if(queue_data$daily_caller[i] > 1){
    queue_data$inter_arrival_time[i] <- queue_data$call_start[i] - queue_data$call_start[i-1]
  }else{
    queue_data$inter_arrival_time[i] <- 0
  }
}



############## Perfected dataset ##############

head(queue_data)
queue_data <- subset(queue_data, 
                     select = c(call_id,          
                                date,
                                daily_caller,
                                call_started,
                                call_answered,
                                call_ended,
                                wait_length,
                                inter_arrival_time,
                                service_length,
                                meets_standard))

print(head(queue_data))
print(queue_data)


##################  Waiting Customers ####################

sum(queue_data$wait_length != 0) / length(queue_data$wait_length) # roughly 12% callers had to wait for their call to be picked up

n <- length(queue_data$wait_length)
k <- length(queue_data$wait_length != 0)
waiting_customers_time <- c()

for(i in 1:n){
  if(queue_data$wait_length[i] != 0){
    waiting_customers_time <- c(waiting_customers_time,queue_data$wait_length[i])
  }
}

length(waiting_customers_time) / length(queue_data$wait_length)
max(waiting_customers_time)

mean((waiting_customers_time)) # on an average each customer had to wait more than 2 minutes !
summary(waiting_customers_time)


wait_time <- mean(queue_data$wait_length)
cat("Mean waiting time" , wait_time,"secs\n",
    "Max wait time" , max(queue_data$wait_length) ,"secs\n",
    "Min wait time", min(queue_data$wait_length) , "secs\n")

summary(queue_data$wait_length)

############ Visual Inspection of the distribution of inter-arrival and service times ################

par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1) # Better margins

# 1. Inter-Arrival Time
hist(queue_data$inter_arrival_time, breaks = 50, prob = TRUE,
     main = "Inter-Arrival Time Distribution",
     xlab = "Time (seconds)", ylab = "Density",
     xlim = c(0, quantile(queue_data$inter_arrival_time, 0.99, na.rm = TRUE)),
     col = "lightblue")
curve(dexp(x, rate = 1/mean(queue_data$inter_arrival_time, na.rm = TRUE)),
      add = TRUE, col = "red", lwd = 2)
legend("topright", legend = "Exponential Fit", col = "red", lwd = 2)

# 2. Service Time
hist(queue_data$service_length, breaks = 50, prob = TRUE,
     main = "Service Time Distribution",
     xlab = "Time (seconds)", ylab = "Density",
     xlim = c(0, quantile(queue_data$service_length, 0.99, na.rm = TRUE)),
     col = "lightgreen")
curve(dexp(x, rate = 1/mean(queue_data$service_length, na.rm = TRUE)),
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = "Exponential Fit", col = "blue", lwd = 2)



############### AD test   ##################

# For inter-arrival times
ad.test(queue_data$inter_arrival_time, "pexp", 
                  rate = 1/mean(queue_data$inter_arrival_time, na.rm = TRUE))

# For service times
ad.test(queue_data$service_length, "pexp",
                   rate = 1/mean(queue_data$service_length, na.rm = TRUE))


# not appropriate to use this test because our data contains "0" values too

################   Kolmogorov-Smirnov Test ###############

# CHECKING FOR EXPONENTIAL DISTRIBUTION

data1 <- queue_data$inter_arrival_time         
data2 <- queue_data$service_length

ks_test1 <- ks.test(data1 , "pexp",
                    rate = 1/mean(data1))

# both the datas have ties.

sum(duplicated(data1)) / length(data1)
sum(duplicated(data2)) / length(data2)

# we cannot use KS test in this case because of ties.

############### Mann-Whitney Test ################

mann_whit_exp <- function(data , rate_est , n_sim = length(data1)){
  # generate synthetic data from exponential distribution
  
  sim_data <- rexp(n_sim , rate = rate_est)
  
  # perform Mann-Whitney Test
  wilcox.test(data, sim_data , exact = FALSE)
  
}

# apply to inter-arrival times

rate_iat <- 1/mean(data1)
test_result_iat <- mann_whit_exp(data1 , rate_iat)
print(test_result_iat)

# apply to service times

rate_st <- 1/mean(data2)
test_result_st <- mann_whit_exp(data2 , rate_st)
print(test_result_st)


## service times are distributed as Exponential Distribution.

## Gama Distribution

gamma_fit <- fitdist(data1, "gamma", method = "mle")
shape_est <- gamma_fit$estimate["shape"]
rate_est <- gamma_fit$estimate["rate"]

mann_whit_gamma <- function(data , shape,rate, n_sim = length(data1)){
  # generate synthetic data from exponential distribution
  
  sim_data <- rgamma(n_sim ,shape=shape, rate = rate)
  
  # perform Mann-Whitney Test
  wilcox.test(data, sim_data , exact = FALSE)
  
}
# apply to inter-arrival times

test_gamma <- mann_whit_gamma(data1,
                             shape = shape_est,
                             rate = rate_est)
print(test_gamma)

# not gamma.




########### KEY PERFORMANCE INDICATORS ############

#A)- Arrival and Service Rates

lambda <- 1/mean(data1)  #Arrival rate
mu <- 1/mean(data2)  # service rate


cat("System Parameters :\n",
    "Arrival Rate(λ):", lambda, "calls/time unit\n",
    "Service Rate(μ):" , mu , "calls/time unit\n")

# B)- Server Calculation Methods

# >>>>  Based on Target Utilization (ρ) <<<<<

target_utilization <- 0.8  # Typical target (80% utilization)
c_util <- ceiling(lambda / (mu * target_utilization))
cat("Servers needed for", target_utilization*100, "% utilization:", c_util, "\n")

# >>>> Based on Maximum Acceptable Wait Time (Wq) <<<<<

target_Wq <- 60 #(seconds)
find_servers <- function(lambda, mu, target_Wq){
  c <- 1
  while(TRUE) {
    rho <- lambda/(c*mu)
    if (rho >= 1) {
      c <- c + 1
      next
    }
    # Erlang C formula
    erlang_c <- ( (c*rho)^c / factorial(c) ) / 
      ( (1-rho)*sum( (c*rho)^(0:(c-1))/factorial(0:(c-1)) ) + 
          ( (c*rho)^c / factorial(c) ) )
    Wq <- erlang_c / (c*mu - lambda)
    if (Wq <= target_Wq) break
    c <- c + 1
  }
  return(c)
}

c_wait <- find_servers(lambda, mu, target_Wq)
cat("Servers needed for Wq <", target_Wq/60, "minutes:", c_wait, "\n")

# C)- Wait Time Metrics

w <- queue_data$wait_length
wait_kpis <- list(
  avg_wait = mean(w),
  median_wait = median(w),
  p95_wait = quantile(w, 0.95),
  max_wait = max(w),
  wait_variance = var(w)
)
print(wait_kpis)


# D)- Service Level Metrics
service_kpis <- list(
  service_level_20 = mean(queue_data$wait_length <= 20) * 100,  # % under 20 sec
  service_level_60 = mean(queue_data$wait_length <= 60) * 100,  # % under 1 min
  abandonment_rate = mean(is.na(queue_data$call_answered)) # % abandoned calls
)
print(service_kpis)

# E)- Queue Dynamics
queue_kpis <- list(
  avg_queue_length = lambda * mean(w),  # Little's Law
  avg_agents_available = c * (1 - (lambda / (c * mu))
                              ))
print(queue_kpis)
