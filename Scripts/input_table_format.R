
# Changes in the csv file 

library(readr)
farmer_input_table <- read_delim("~/Documents/Essay_5/farmer_input_table.csv", 
                                 delim = ";", escape_double = FALSE, 
                                 trim_ws = TRUE)

wd <- "../land_consolidation_impacts/"
write.csv(farmer_input_table,paste0(wd, "/Input_tables/farmer_input_table.csv"))

