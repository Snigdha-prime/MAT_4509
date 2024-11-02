# Snigdha Das(ID_22024012)
# Changes should be made to the working directory for using it in other PC. 
rm(list = ls())# to clear the environment.
library(readxl)
library(graphics)

setwd("F:/Rstudio codes/MAT_4509")
#variable with my ID
id<-12

# load dataset
xl_data <- read_excel("United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range = "b2:w158")

#took the same number of data input as my id's last two digits.

maintenance_categories <- c("labor", "materials", "third party", "burden")
maintenance_rows <- c(16, 55, 94, 133)

years <- 1995:(1995+(id-1))
load_factor_rows <- maintenance_rows + 18

# Function to extract data for a given row number (Maintenance/Load Factor)
get_data_by_row <- function(row_num) {
  return((as.numeric(xl_data[row_num, 1:(id)+1])))
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

# For ploting Load Factor bar plot
plot_bar <- function(data, title) {
  barplot(data,
          xlab = "Years",
          ylab = "Load Factor (%)",
          col = "lightblue",
          border = "black",
          main = title
  )
}

fleet_category <- c("small narrowbodies","large narrowbodies",
                    "widebodies","total fleet")

set_window_size <- function() {
  windows(width = 1800 / 100, height = 1080 / 100)
}

# Plot pie charts for maintenance
for (i in 1:4) {
  data_1 <- get_maintenance_category(maintenance_rows[i])
  pie(data_1,labels = years,  main = fleet_category[i])
}

# Plot bar charts for load factor
for (i in 1:4) {
  data_2 <- setNames(get_data_by_row(maintenance_rows[i] + 18), years)
  plot_bar(data_2, fleet_category[i])
}