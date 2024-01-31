##################################
##      PIM Site Selection      ##
##         Scott Miller         ##
##################################


# load packages
library(data.table)
library(tidyverse)
library(dplyr)
library(rgdal)
library(ggplot2)
library(sf)
library(writexl)
library(collapse)
library(stringr)

# set seed
set.seed(1342)
# set.seed(54312)

# ------------------------------------------------------------------------------ 
# Data import
# ------------------------------------------------------------------------------

# set directory
setwd("")

# import data
dta <- read.csv("All Project Data.csv",na.strings=c(""," ", "N/A","NA"))

# set as a data.table
dta <- as.data.table(dta)


# ------------------------------------------------------------------------------ 
# Data cleaning
# ------------------------------------------------------------------------------ 

# rename common variables
dta <- rename(dta,
              grant_number = Grants.Grant.Number,
              project_number = Project.Data..Hybrid..Project.Number,
              admin_1 = Project.Data..Hybrid..Admin.Unit.1,
              admin_2 = Project.Data..Hybrid..Admin.Unit.2,
              admin_3 = Project.Data..Hybrid..Admin.Unit.3,
              admin_4 = Project.Data..Hybrid..Admin.Unit.4,
              location_type = Project.Data..Hybrid..Location.Type,
              new_rehab = Project.Data..Hybrid..New.or.Rehab,
              inventory_type = Project.Data..Hybrid..Water.Inventory.Type,
              community = Project.Data..Hybrid..Community.Name)

# dta <- rename(dta,
#               grant_number = Grant.Number,
#               project_number = Project.Number,
#               admin_1 = Admin.Unit.1,
#               admin_2 = Admin.Unit.2,
#               admin_3 = Admin.Unit.3,
#               admin_4 = Admin.Unit.4,
#               location_type = Location.Type,
#               new_rehab = New.or.Rehab,
#               inventory_type = Water.Inventory.Type,
#               community = Community.Name)


# drop health centers
dta <- dta[!(dta$location_type == "health center/clinic" | dta$location_type == "Health Center/Clinic"),]
# dta <- dta[!(dta$location_type == "School" | dta$location_type == "school"),]


# data entry errors
# dta$admin_1 <- ifelse(dta$admin_1 == "Southern", "South", dta$admin_1)


# collapse by community
# -------------------------------
dta1 <- data.frame(dta)

dta1$community <- str_trim(dta1$community, side = c("both"))

for (j in 1:ncol(dta1)) {
    dta1[,j] <- as.character(dta1[,j])
}

dta1 <- collap(dta1, ~ admin_3 + community, catFUN = flast,         # Aggregate only categorical data
               cols = is_categorical)

dta <- as.data.table(dta1[,-c(9,14)])
# -------------------------------


# sort by grant & project number
dta <- dta[order(dta$grant_number, dta$project_number),]

# create new object with grant numbers
grants <- sort(unique(dta$grant_number))



# ------------------------------------------------------------------------------ 
# count number of water points in each grant
# ------------------------------------------------------------------------------ 

# Total water points in PIM sample frame
wp_all <- nrow(dta)

# water points by grant
wp_bygrant <- dta[, .N, by = "grant_number"]
wp_bygrant$pct <- round(wp_bygrant$N / wp_all, 2)


# ------------------------------------------------------------------------------ 
# count number of water points in each region
# ------------------------------------------------------------------------------ 

# water points by administrative unit 1
wp_byadmin1 <- dta[, .N, by = "admin_1"]
wp_byadmin1$pct <- round(wp_byadmin1$N / wp_all, 2)


# water points by grant & administrative unit 1
wp_bygrant.admin1 <- dta[CJ(grant_number, admin_1, unique=TRUE), on=c("grant_number", "admin_1"), .N, by=.EACHI]
wp_bygrant.admin1$pct <- round(wp_bygrant.admin1$N / wp_all, 2)


# water points by administrative unit 2
wp_byadmin2 <- dta[, .N, by = .(admin_1, admin_2)]
wp_byadmin2$pct <- round(wp_byadmin2$N / wp_all, 2)


# water points by grant & administrative unit 1 & administrative unit 2
wp_bygrant.admin2 <- dta[CJ(grant_number, admin_1, admin_2, unique=TRUE), on=c("grant_number", "admin_1", "admin_2"), .N, by=.EACHI]
wp_bygrant.admin2$pct <- round(wp_bygrant.admin2$N / wp_all, 2)


# water points by location type
wp_bylocation <- dta[, .N, by = "location_type"]
wp_bylocation$pct <- round(wp_bylocation$N / wp_all, 2)


# water points by new/rehab
wp_bynew_rehab <- dta[, .N, by = "new_rehab"]
wp_bynew_rehab$pct <- round(wp_bynew_rehab$N / wp_all, 2)

# water points by inventory_type
wp_byinventory <- dta[, .N, by = "inventory_type"]
wp_byinventory$pct <- round(wp_byinventory$N / wp_all, 2)


# ------------------------------------------------------------------------------ 
# sample water points by grant, district, new/rehab
# ------------------------------------------------------------------------------ 


# define total sample size
sample_size <- 100

# Calculate the number of water points to be selected from each grant
dta <- dta %>%
    left_join(wp_bygrant, by = "grant_number") %>%
    mutate(sample_size_grant = round(pct * sample_size, 0))

# Sample within each group and bind rows
sample <- dta %>%
    group_by(grant_number) %>%
    mutate(rand = runif(n())) %>%
    arrange(rand) %>%
    mutate(rn = row_number()) %>%
    filter(rn <= sample_size_grant) %>%
    select(-rn, -rand) %>%
    ungroup()

# If you need to bind rows
sample <- bind_rows(sample)



# ------------------------------------------------------------------------------ 
# check validity of sample
# ------------------------------------------------------------------------------ 

# Define the required variables
required_vars <- c("grant_number", "admin_1", "admin_2", "location_type", "new_rehab", "inventory_type")

# Create a function to calculate the frequency of observations in each dimension
calculate_frequencies <- function(data, sampled_data, var_name) {
    original_frequencies <- data %>% group_by(!!sym(var_name)) %>% summarise(Frequency = n() / nrow(data) * 100)
    sampled_frequencies <- sampled_data %>% group_by(!!sym(var_name)) %>% summarise(Frequency_sampled = n() / nrow(sampled_data) * 100)
    frequency_table <- full_join(original_frequencies, sampled_frequencies, by = var_name)
    frequency_table <- frequency_table %>% replace_na(list(Frequency = 0, Frequency_sampled = 0))
    # Convert the category column to character to ensure consistency
    frequency_table[[var_name]] <- as.character(frequency_table[[var_name]])
    colnames(frequency_table) <- c("Category", "Frequency", "Frequency_sampled")
    # Calculate the frequency difference
    frequency_table$Sampling_Error <- frequency_table$Frequency_sampled - frequency_table$Frequency
    frequency_table$Dimension <- var_name
    return(frequency_table)
}

# Initialize an empty list for the comparison table
comparison_tables <- list()

# Calculate frequencies for each dimension and append to the comparison tables list
for (dim in required_vars) {
    dim_frequencies <- calculate_frequencies(dta, sample, dim)
    comparison_tables[[dim]] <- dim_frequencies
}

# Bind all the frequency tables together
comparison_table <- bind_rows(comparison_tables)

# Reorder the columns to include the newly added 'Frequency_Difference' column
comparison_table <- comparison_table[, c("Dimension", "Category", "Frequency", "Frequency_sampled", "Sampling_Error")]

# Print the comparison table
print(comparison_table, n=30)



# ------------------------------------------------------------------------------ 
# Save sample to project folder
# ------------------------------------------------------------------------------ 

write_xlsx(list(sample = sample), 
           path = "[CN.PTR]_PIM_sample.xlsx")



# ------------------------------------------------------------------------------ 
# code sandbox
# ------------------------------------------------------------------------------ 

# dta[, data.table(table(Grant.Number)), by = "admin_1"]

