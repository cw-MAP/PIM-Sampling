##################################
##      PIM Site Selection      ##
##         Scott Miller         ##
##################################


# load packages
library(data.table)
library(dplyr)
library(rgdal)
library(ggplot2)
library(sf)
library(writexl)
library(collapse)
library(stringr)

# set seed
#set.seed(1342)
set.seed(54312)

# ------------------------------------------------------------------------------ 
# Data import
# ------------------------------------------------------------------------------

# set directory
setwd("/Users/scott.miller/Library/CloudStorage/Box-Box/Water Programs/M&E/02_Partner Planning Folders/WV_Niger/Site lists and sampling/PIM 2022")

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

# drop health centers
dta <- dta[!(dta$location_type == "health center/clinic"),]


# data entry errors
dta$admin_1 <- ifelse(dta$admin_1 == "MARADI", "Maradi", dta$admin_1)
dta$admin_2 <- ifelse(dta$admin_2 == "GUIDAN ROUMDJI", "Guidan Roumdji", dta$admin_2)
dta$location_type <- ifelse(dta$location_type == "community", "Community", dta$location_type)
dta$location_type <- ifelse(dta$location_type == "school", "School", dta$location_type)
dta$inventory_type <- ifelse(dta$inventory_type == "Water Point And Sanitation", "Water Point and Sanitation", dta$inventory_type)
dta$community <- ifelse(dta$community == "Water Point And Sanitation", "Water Point and Sanitation", dta$community)


# collapse by community
# -------------------------------
dta1 <- data.frame(dta)

dta1$community <- str_trim(dta1$community, side = c("both"))

for (j in 1:ncol(dta1)) {
    dta1[,j] <- as.character(dta1[,j])
}

dta1 <- collap(dta1, ~ admin_3 + community, catFUN = flast,         # Aggregate only categorical data
            cols = is_categorical)

dta <- dta <- as.data.table(dta1)
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
sample_size <- 110


for (i in 1:length(grants)) {
    
    # create data object with only grant_i's water points
    assign(paste0("grant_",i), dta[dta$grant_number == grants[i],])
    
    # calculate the number of water points to be selected from this grant
    assign(paste0("grant_",i,"_n"), round(wp_bygrant[grant_number == grants[i], pct]*sample_size, 0))

}

#assign and random numbers within each grant
    #sort by random numbers within each grant
        # select corresponding number of water points within each grant
grant_1$rand <- runif(nrow(grant_1))
    grant_1 <- grant_1[order(grant_1$rand),]
        grant_1 <- grant_1[1:grant_1_n,]
grant_2$rand <- runif(nrow(grant_2))
    grant_2 <- grant_2[order(grant_2$rand),]
        grant_2 <- grant_2[1:grant_2_n,]
grant_3$rand <- runif(nrow(grant_3))
    grant_3 <- grant_3[order(grant_3$rand),]
        grant_3 <- grant_3[1:grant_3_n,]
grant_4$rand <- runif(nrow(grant_4))
    grant_4 <- grant_4[order(grant_4$rand),]
        grant_4 <- grant_4[1:grant_4_n,]
grant_5$rand <- runif(nrow(grant_5))
    grant_5 <- grant_5[order(grant_5$rand),]
        grant_5 <- grant_5[1:grant_5_n,]
grant_6$rand <- runif(nrow(grant_6))
    grant_6 <- grant_6[order(grant_6$rand),]
        grant_6 <- grant_6[1:grant_6_n,]
# grant_7$rand <- runif(nrow(grant_7))
#     grant_7 <- grant_5[order(grant_7$rand),]
#         grant_7 <- grant_7[1:grant_7_n,]
# grant_8$rand <- runif(nrow(grant_8))
#     grant_8 <- grant_5[order(grant_8$rand),]
#         grant_8 <- grant_8[1:grant_8_n,]
# grant_9$rand <- runif(nrow(grant_9))
#     grant_9 <- grant_5[order(grant_9$rand),]
#         grant_9 <- grant_9[1:grant_9_n,]
# grant_10$rand <- runif(nrow(grant_10))
#     grant_10 <- grant_10[order(grant_10$rand),]
#         grant_10 <- grant_10[1:grant_10_n,]


sample <- rbind(grant_1, 
                    grant_2,
                    grant_3,
                    grant_4,
                    grant_5,
                    grant_6
                    # grant_7,
                    # grant_8,
                    # grant_9, 
                    # grant_10
        )        


wp_bygrant$sample_pct <- round((sample[, .N, by = "grant_number"]$N / nrow(sample)), 2)
wp_bygrant
wp_byadmin1$sample_pct <- round((sample[, .N, by = "admin_1"]$N / nrow(sample)), 2)
wp_byadmin1
wp_byadmin2$sample_pct <- round((sample[, .N, by = "admin_2"]$N / nrow(sample)), 2)
wp_byadmin2
wp_bylocation$sample_pct <- round((sample[, .N, by = "location_type"]$N / nrow(sample)), 2)
wp_bylocation
wp_bynew_rehab$sample_pct <- round((sample[, .N, by = "new_rehab"]$N / nrow(sample)), 2)
wp_bynew_rehab
wp_byinventory$sample_pct <- round((sample[, .N, by = "inventory_type"]$N / nrow(sample)), 2)
wp_byinventory


write_xlsx(list(sample = sample), 
           path = "NE.WVI_PIM_sample.xlsx")



# ------------------------------------------------------------------------------ 
# code sandbox
# ------------------------------------------------------------------------------ 

# dta[, data.table(table(Grant.Number)), by = "admin_1"]

