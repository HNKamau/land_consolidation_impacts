
# Changes in the csv file 

library(readr)
land_consolidation_input <- read_delim("Input_tables/land_consolidation_input.csv", 
                                 delim = ";", escape_double = FALSE, 
                                 trim_ws = TRUE)

write.csv(land_consolidation_input, "Input_tables/land_consolidation_input.csv"))

