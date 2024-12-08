# Snigdha Das(ID_22024012)
# Changes should be made to the working directory for using it in other PC.
rm(list = ls())# to clear the environment.

library(readxl)


setwd("F:/Rstudio codes/MAT_4509")
#variable with my ID
id<-12

# load dataset
xl_data <- read_excel("United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range = "b2:w158")

# creating function to salarys and wages data by row
#took the same number of data input as my id's last two digits.
get_salary_wages <- function(row_num, data = xl_data){
  return((as.numeric(data[row_num, 1:id+1])))
}



# Extract salary and wages data for different fleets
salary_wages_snbodies <- get_salary_wages(6)  
salary_wages_lnbodies <- get_salary_wages(45) 
salary_wages_wbodies <- get_salary_wages(84)
salary_wages_tfleet <- get_salary_wages(123)


get_modes <- function(data) {
  freq_table <- table(data)
  max_freq <- max(freq_table)
  modes <- as.numeric(freq_table[freq_table == max_freq])
  if (length(modes) == length(data)) {
    return(NULL)
  }
  return(modes)
}
make_Freq_Dis <- function(
    wage_data) {
  # number of observations
  n <- length(wage_data)
  
  # calculating the value of k (smallest k such that 2^k > n)
  k <- 0
  for (i in 1:(n / 2)) {
    if (2^i > n) {
      k <- i
      break
    }
  }
  
  # calculating class interval ( interval >= (max - min)/k)
  min_salary <- min(wage_data)
  max_salary <- max(wage_data)
  class_interval <- (max_salary - min_salary) / k
  class_interval <- ceiling(class_interval)
  
  
  
  # Creating breakpoints
  break_points <- seq(
    min_salary - (class_interval / 2),
    max_salary + (class_interval / 2),
    by = class_interval
  )
  
  # Creating frequency distribution
  salary_bins <- cut(wage_data, breaks = break_points, right = TRUE)
  frequency_distribution <- table(salary_bins)
  #cut() function is used to divide a numeric vector into different ranges.
  return(frequency_distribution)
}
print_analysis <- function(wage_data, title) {
  mean <- mean(wage_data)
  median <- median(wage_data)
  modes <- get_modes(wage_data)
  sample_sd <- sd(wage_data) # sample
  sample_var <- var(wage_data) # sample
  quartiles <- quantile(wage_data, probs = c(0.25, 0.5, 0.75))
  tenth_percentile <- quantile(wage_data, probs = 0.10)
  ninth_decile <- quantile(wage_data, probs = 0.90)
  range <- max(wage_data) - min(wage_data)
  
  # print results
      # Here,I used cat() to concatenate and print strings and values from the variable together.
  cat("Analysis of ", title, ":\n")
  cat("Mean:", mean, "\n")
  cat("Median:", median, "\n")
  if (is.null(modes) || length(modes) == 0) {
    cat("Modes: None\n")
  } else {
    cat("Modes:", paste(modes, collapse = ", "), "\n")
  }
  
  cat("Sample Standard Deviation:", sample_sd, "\n")
  cat("Sample Variance:", sample_var, "\n")
  cat("Quartiles (Q1, Q2, Q3):", quartiles, "\n")
  cat("10th Percentile:", tenth_percentile, "\n")
  cat("9th Decile:", ninth_decile, "\n")
  cat("Range:", range, "\n")
  cat("\n\n")
}
set_window_size <- function() {
  windows(width = 1800 / 100, height = 1080 / 100)
}

plot_histogram <- function(frequency_distribution, title,xlim,ylim) {
  set_window_size()
  barplot(frequency_distribution,
          main = title,
          xlab = "Salary Ranges",
          ylab = "Frequency",
          col = "steelblue",
          border = "black",
          space = 0.5, # No space between bars
          width = 1 # Adjust width to fill the space better
  )
  
}

# get frequency distribution (i)
SN_freq_dis <- make_Freq_Dis(salary_wages_snbodies)
LN_freq_dis <- make_Freq_Dis(salary_wages_lnbodies)
WB_freq_dis <- make_Freq_Dis(salary_wages_wbodies)
TO_freq_dis <- make_Freq_Dis(salary_wages_tfleet)

# print Frequency Distribution
cat("Frequency Distribution for Small Narrowbodies:\n")
print(SN_freq_dis)
cat("\nFrequency Distribution for Large Narrowbodies:\n")
print(LN_freq_dis)
cat("\nFrequency Distribution for Widebodies:\n")
print(WB_freq_dis)
cat("\nFrequency Distribution for Total Fleet:\n")
print(TO_freq_dis)

# print analysis (ii)
print_analysis(salary_wages_snbodies, "salary wages of small narrowbodies")
print_analysis(salary_wages_lnbodies, "salary wages of large narrowbodies")
print_analysis(salary_wages_wbodies, "salary wages of widebodies")
print_analysis(salary_wages_tfleet, "salary wages of total fleet")

# histogram using i. (iii)
plot_histogram(SN_freq_dis, "salary wages of small narrowbodies")
plot_histogram(LN_freq_dis, "salary wages of large narrowbodies")
plot_histogram(WB_freq_dis, "salary wages of widebodies")
plot_histogram(TO_freq_dis, "salary wages of total fleet")