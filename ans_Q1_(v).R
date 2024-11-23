# Snigdha Das(ID_22024012)
# Changes should be made to the working directory for using it in other PC. 
rm(list = ls()) # Clear the environment
graphics.off()  # Close all open graphics windows

library(readxl)
library(graphics)

# Load dataset
xl_data <- read_excel("United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range = "b2:w158")

# Variable with my ID
id <- 12  # This should be set based on your specific requirements

# Define categories
daily_utilization_categories <- c("Block hours", "Airborne hours", "Departures")
ownership_categories <- c("Rental", "Depreciation and Amortization")
purchased_goods_categories <- c("Fuel/Oil", "Insurance", "Other (inc. Tax)")
fleet_category <- c(
  "small narrowbodies",
  "large narrowbodies",
  "widebodies",
  "total fleet"
)

# Row numbers for each category
purchased_goods_rows <- c(16, 55, 94, 133) - 5
ownership_rows <- purchased_goods_rows + 12
daily_utilization_rows <- ownership_rows + 13

get_data_by_row <- function(row_num) {
  return((as.numeric(xl_data[row_num, 1:(id) + 1])))  # Using 'id' to select number of values
}

get_category_data <- function(row_num, categories) {
  rows_data <- lapply(
    seq_along(categories),
    function(i) get_data_by_row(row_num + i)
  )
  costs <- unlist(rows_data)
  category <- factor(rep(categories, sapply(rows_data, length)))
  return(data.frame(costs = costs, category = category))
}

# Function to create box plots
box_plot <- function(data, title, ylab) {
  boxplot(costs ~ category,
          data = data,
          col = "lightblue",
          ylab = ylab,
          main = title,
          border = "black"
  )
}

# Plot all box plots for each fleet category in separate windows
for (i in 1:4) {
  windows(width = 15, height = 10)  # Open a new window with increased dimensions
  
  # Set up layout for 3 rows (1 for each category)
  par(mfrow = c(3, 1))  # 3 rows, 1 column
  
  # Plot Purchased Goods
  box_plot(
    get_category_data(purchased_goods_rows[i], purchased_goods_categories),
    paste("Purchased Goods for", fleet_category[i]),
    "Cost ($)"
  )
  
  # Plot Aircraft Ownership
  box_plot(
    get_category_data(ownership_rows[i], ownership_categories),
    paste("Aircraft Ownership for", fleet_category[i]),
    "Cost ($)"
  )
  
  # Plot Daily Utilization
  box_plot(
    get_category_data(daily_utilization_rows[i], daily_utilization_categories),
    paste("Daily Utilization for", fleet_category[i]),
    "Hours"
  )
}
