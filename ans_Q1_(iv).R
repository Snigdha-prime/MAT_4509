# Snigdha Das(ID_22024012)
# Changes should be made to the working directory for using it in other PC. 
rm(list = ls()) # Clearing  the environment
graphics.off()  # Closeing all open graphics windows

library(readxl)
library(graphics)

setwd("F:/Rstudio codes/MAT_4509")  # setting the directory name
# Variable with my ID
id <- 12

# Load dataset
xl_data <- read_excel("United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range = "b2:w158")

# Took the same number of data input as my ID's last two digits.
maintenance_categories <- c("labor", "materials", "third party", "burden")
maintenance_rows <- c(16, 55, 94, 133)

years <- 1995:(1995 + (id - 1))
load_factor_rows <- maintenance_rows + 18

# Function to extract data for a given row number (Maintenance/Load Factor)
get_data_by_row <- function(row_num) {
  return((as.numeric(xl_data[row_num, 1:(id) + 1])))
}

get_maintenance_category <- function(row_num) {
  labor <- get_data_by_row(row_num + 1)
  materials <- get_data_by_row(row_num + 2)
  third_party <- get_data_by_row(row_num + 3)
  burden <- get_data_by_row(row_num + 5)
  return(setNames(
    c(sum(labor), sum(materials), sum(third_party), sum(burden)),
    maintenance_categories
  ))
}

# For plotting Load Factor bar plot
plot_bar <- function(data, title) {
  barplot(data,
          xlab = "Years", 
          ylab = "Load Factor (%)",
          col = "lightblue",
          border = "black",
          main = title,
          cex.main = 1.5,  # Increase main title size
          cex.lab = 1.3,   # Increase axis label size
          font.main = 2,   # Bold main title
          font.lab = 2     # Bold axis labels
  )
}

fleet_category <- c("small narrowbodies", "large narrowbodies",
                    "widebodies", "total fleet")

# Function to create pie chart with percentages
plot_pie_with_percentages <- function(data, main_title) {
  percentages <- round(data / sum(data) * 100, 1)  # Calculate percentages
  labels <- paste0(names(data), ": ", percentages, "%")  # Create labels with percentages
  pie(data, labels = labels, main = main_title, 
      cex.main = 1.5,       # Increase main title size
      cex.lab = 1.3,        # Increase label size
      font.main = 2,        # Bold main title
      font.axis = 2         # Bold axis font (though not typically used in pie charts)
  )  
}

# Plot pie charts for maintenance in separate figures
for (i in 1:4) {
  data_1 <- get_maintenance_category(maintenance_rows[i])
  windows()  # Open a new window for Windows OS
  # quartz()  # Uncomment this line if you're on macOS
  plot_pie_with_percentages(data_1, fleet_category[i])  # Call the new function
  Sys.sleep(2)  # Pause to view the pie chart
}

# Plot bar charts for load factor in separate figures
for (i in 1:4) {
  data_2 <- setNames(get_data_by_row(maintenance_rows[i] + 18), years)
  windows()  # Open a new window for Windows OS
  # quartz()  # Uncomment this line if you're on macOS
  plot_bar(data_2, fleet_category[i])
  Sys.sleep(2)  # Pause to view the bar chart
}
