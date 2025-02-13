# Model testing ###

make_variables <- function(est, n=1)
{x <- random(rho=est, n=n)
for (i in colnames(x))assign(i, as.numeric(x[1,i]), envir = .GlobalEnv)}

make_variables(estimate_read_csv("Input_tables/farmer_input_table.csv"))

