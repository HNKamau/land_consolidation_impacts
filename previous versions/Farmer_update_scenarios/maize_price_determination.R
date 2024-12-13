# Load necessary library
library(dplyr)

# Set the directory containing the CSV files
folder_path <- "~/Downloads/maize_prices/"

# Get a list of all CSV files in the directory
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, function(file) {
  read.csv(file, sep = ";")  # Specify semicolon as the separator
})
# Optionally, add a column with the file name to track the source of each data frame
list_of_dfs <- lapply(seq_along(list_of_dfs), function(i) {
  df <- list_of_dfs[[i]]
  
  # Convert Supply.Volume to numeric
  df <- df %>%
    mutate(Supply.Volume = as.numeric(Supply.Volume))  # Convert to numeric (or as.character() if preferred)
  
  # Add file name as a new column to track the source
  df$source_file <- basename(csv_files[i])
  
  return(df)
})
# Combine all data frames into one
combined_df <- bind_rows(list_of_dfs)

# View the first few rows of the combined data frame
head(combined_df)


str(combined_df)
unique(combined_df$Wholesale)
unique(combined_df$Retail)

combined_df <- combined_df %>%
  mutate(
    Wholesale = gsub("/Kg", "", Wholesale),           # Remove "/Kg"
    Wholesale = gsub(",", "", Wholesale),             # Remove commas (if they exist)
    Wholesale = trimws(Wholesale),                    # Remove any leading/trailing whitespaces
    Wholesale = na_if(Wholesale, " -"),               # Convert " - " to NA
    Wholesale = as.numeric(Wholesale),                # Convert to numeric
    Retail = gsub("/Kg", "", Retail),                 # Same process for Retail
    Retail = gsub(",", "", Retail),
    Retail = trimws(Retail),
    Retail = na_if(Retail, " -"),
    Retail = as.numeric(Retail),
    Date = as.Date(Date)                             # Change the date to be date
  )

unique(combined_df$Wholesale)
unique(combined_df$Retail)
range(combined_df$Wholesale, na.rm = T)
min(combined_df$Wholesale, na.rm = T)


ggplot(combined_df, aes(x = Date)) +
  geom_line(aes(y = Wholesale, color = "Wholesale"), size = 1) +  # Wholesale line
  geom_line(aes(y = Retail, color = "Retail"), size = 1) +        # Retail line
  labs(x = "Date", y = "Price (KES)", color = "Legend") +          # Labels
  theme_minimal() +                                                # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +       # Rotate x-axis labels for better readability
  scale_color_manual(values = c("Wholesale" = "#1a80bb", "Retail" = "#e67e22"))  # Custom colors for the lines



ggplot(combined_df, aes(x = Date)) +
  geom_jitter(aes(y = Retail)) +
  coord_cartesian(xlim = as.Date(c("2020-01-01", "2024-08-31")))



combined_df %>%
  filter(Wholesale <= 500 | is.na(Wholesale)) %>% 
  ggplot(aes(x=Wholesale)) +
  geom_histogram(binwidth = 10)

combined_df %>% 
  select(Wholesale ) %>% 
  arrange(ascendi)
  ggplot(aes(x=Wholesale))+
  geom_histogram(binwidth = 10)+
  coord_cartesian(xlim = c(15, 250))
  
  
  
 combined_df %>%
    filter(!is.na(Wholesale)) %>%
    select(Wholesale) %>%
    arrange(Wholesale)
  