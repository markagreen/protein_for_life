##############################################
######## Analysis of Sainsbury's Data ########
##############################################

# Libraries
library(data.table)
library(ggplot2)

### Data ###

# Load data
sample <- fread("C:/Data for Protein/CUSTOMERS_TOTALS_NO_IMD/CUSTOMERS_TOTALS_NO_IMD.tab")
 
# Convert missing data to 0s
sample[is.na(sample)] <- 0

# Switch process extreme values as 0 - always crashes
#sample[,2:31] <- sapply(sample[,2:31],function(x)ifelse(x < 0, 0, x))
#sample[,2:31] <- sapply(sample[,2:31],function(x)ifelse(x > (mean(x)+(2*sd(x))), 0, x))

# Calculate percentage protein within category (and remove extreme values)
# mean(sample$'CM Baby & Beauty_Protein', na.rm = T) + (2 * sd(sample$'CM Baby & Beauty_Protein', na.rm = T))
sample$'CM Baby & Beauty_Protein'[sample$'CM Baby & Beauty_Protein' < 0 | sample$'CM Baby & Beauty_Protein' > 528.9722] <- NA
sample$'CM Baby & Beauty_Energy'[sample$'CM Baby & Beauty_Energy' < 0 | sample$'CM Baby & Beauty_Energy' > 22993.39] <- NA
sample$baby_beauty_pc_prtn <- ((4*sample$'CM Baby & Beauty_Protein') / sample$'CM Baby & Beauty_Energy') * 100

sample$'CM Bakery_Protein'[sample$'CM Bakery_Protein' < 0 | sample$'CM Bakery_Protein' > 11915.21] <- NA
sample$'CM Bakery_Energy'[sample$'CM Bakery_Energy' < 0 | sample$'CM Bakery_Energy' > 359451.9] <- NA
sample$bakery_pc_prtn <- ((4*sample$'CM Bakery_Protein') / sample$'CM Bakery_Energy') * 100

sample$'CM Beers  Wines & Spirits_Energy'[sample$'CM Beers  Wines & Spirits_Energy' < 0 | sample$'CM Beers  Wines & Spirits_Energy' > 93623.48] <- NA
sample$'CM Beers  Wines & Spirits_Protei'[sample$'CM Beers  Wines & Spirits_Protei' < 0 | sample$'CM Beers  Wines & Spirits_Protei' > 630.3179] <- NA
sample$beer_wine_spirit_pc_prtn <- ((4*sample$'CM Beers  Wines & Spirits_Protei') / sample$'CM Beers  Wines & Spirits_Energy') * 100

sample$'CM Canned & Packaged_Energy'[sample$'CM Canned & Packaged_Energy' < 0 | sample$'CM Canned & Packaged_Energy' > 465579.2] <- NA
sample$'CM Canned & Packaged_Protein'[sample$'CM Canned & Packaged_Protein' < 0 | sample$'CM Canned & Packaged_Protein' > 9819.867] <- NA
sample$canned_packaged_pc_prtn <- ((4*sample$'CM Canned & Packaged_Protein') / sample$'CM Canned & Packaged_Energy') * 100

sample$'CM Dairy_Energy'[sample$'CM Dairy_Energy' < 0 | sample$'CM Dairy_Energy' > 408031.4] <- NA
sample$'CM Dairy_Protein'[sample$'CM Dairy_Protein' < 0 | sample$'CM Dairy_Protein' > 15205.41] <- NA
sample$dairy_pc_prtn <- ((4*sample$'CM Dairy_Protein') / sample$'CM Dairy_Energy') * 100

sample$'CM Food For Later_Energy'[sample$'CM Food For Later_Energy' < 0 | sample$'CM Food For Later_Energy' > 141798.3] <- NA
sample$'CM Food For Later_Protein'[sample$'CM Food For Later_Protein' < 0 | sample$'CM Food For Later_Protein' > 5544.996] <- NA
sample$food_for_later_pc_prtn <- ((4*sample$'CM Food For Later_Protein') / sample$'CM Food For Later_Energy') * 100

sample$'CM Food To Go_Energy'[sample$'CM Food To Go_Energy' < 0 | sample$'CM Food To Go_Energy' > 58533.45] <- NA
sample$'CM Food To Go_Protein'[sample$'CM Food To Go_Protein' < 0 | sample$'CM Food To Go_Protein' > 5278.604] <- NA
sample$food_to_go_pc_prtn <- ((4*sample$'CM Food To Go_Protein') / sample$'CM Food To Go_Energy') * 100

sample$'CM Frozen Food_Energy'[sample$'CM Frozen Food_Energy' < 0 | sample$'CM Frozen Food_Energy' > 180590.6] <- NA
sample$'CM Frozen Food_Protein'[sample$'CM Frozen Food_Protein' < 0 | sample$'CM Frozen Food_Protein' > 9146.638] <- NA
sample$frozen_food_pc_prtn <- ((4*sample$'CM Frozen Food_Protein') / sample$'CM Frozen Food_Energy') * 100

sample$'CM Impulse Food_Energy'[sample$'CM Impulse Food_Energy' < 0 | sample$'CM Impulse Food_Energy' > 284766.7] <- NA
sample$'CM Impulse Food_Protein'[sample$'CM Impulse Food_Protein' < 0 | sample$'CM Impulse Food_Protein' > 3934.811] <- NA
sample$impluse_pc_prtn <- ((4*sample$'CM Impulse Food_Protein') / sample$'CM Impulse Food_Energy') * 100

sample$'CM Meat  Fish & Poultry_Energy'[sample$'CM Meat  Fish & Poultry_Energy' < 0 | sample$'CM Meat  Fish & Poultry_Energy' > 251650.1] <- NA
sample$'CM Meat  Fish & Poultry_Protein'[sample$'CM Meat  Fish & Poultry_Protein' < 0 | sample$'CM Meat  Fish & Poultry_Protein' > 29684.84] <- NA
sample$meat_fish_poultry_pc_prtn <- ((4*sample$'CM Meat  Fish & Poultry_Protein') / sample$'CM Meat  Fish & Poultry_Energy') * 100

sample$'CM Produce_Energy'[sample$'CM Produce_Energy' < 0 | sample$'CM Produce_Energy' > 111422.4] <- NA
sample$'CM Produce_Protein'[sample$'CM Produce_Protein' < 0 | sample$'CM Produce_Protein' > 2792.311] <- NA
sample$produce_pc_prtn <- ((4*sample$'CM Produce_Protein') / sample$'CM Produce_Energy') * 100

sample$'CM World Foods_Energy'[sample$'CM World Foods_Energy' < 0 | sample$'CM World Foods_Energy' > 206851.9] <- NA
sample$'CM World Foods_Protein'[sample$'CM World Foods_Protein' < 0 | sample$'CM World Foods_Protein' > 3077.735] <- NA
sample$world_foods_pc_prtn <- ((4*sample$'CM World Foods_Protein') / sample$'CM World Foods_Energy') * 100

sample$'CM Customer Ordering_Energy'[sample$'CM Customer Ordering_Energy' < 0 | sample$'CM Customer Ordering_Energy' > 41258.84] <- NA
sample$'CM Customer Ordering_Protein'[sample$'CM Customer Ordering_Protein' < 0 | sample$'CM Customer Ordering_Protein' > 1768.525] <- NA
sample$customer_ordering_pc_prtn <- ((4*sample$'CM Customer Ordering_Protein') / sample$'CM Customer Ordering_Energy') * 100

sample$'CM Food Service_Energy'[sample$'CM Food Service_Energy' < 0 | sample$'CM Food Service_Energy' > 57488.7] <- NA
sample$'CM Food Service_Protein'[sample$'CM Food Service_Protein' < 0 | sample$'CM Food Service_Protein' > 730.346] <- NA
sample$food_service_pc_prtn <- ((4*sample$'CM Food Service_Protein') / sample$'CM Food Service_Energy') * 100

# Total energy intake
sample$total_engy <- sample$'CM Baby & Beauty_Energy' + sample$'CM Bakery_Energy' + sample$'CM Beers  Wines & Spirits_Energy' + sample$'CM Canned & Packaged_Energy' +
  sample$'CM Dairy_Energy' + sample$'CM Food For Later_Energy' + sample$'CM Food To Go_Energy' + sample$'CM Frozen Food_Energy' + sample$'CM Impulse Food_Energy' + 
  sample$'CM Meat  Fish & Poultry_Energy' + sample$'CM Produce_Energy' + sample$'CM World Foods_Energy' + sample$'CM Customer Ordering_Energy' + sample$'CM Food Service_Energy'

# Total protein intake
sample$total_prtn <- 4* (sample$'CM Baby & Beauty_Protein' + sample$'CM Bakery_Protein' + sample$'CM Beers  Wines & Spirits_Protei' + sample$'CM Canned & Packaged_Protein' +
  sample$'CM Dairy_Protein' + sample$'CM Food For Later_Protein' + sample$'CM Food To Go_Protein' + sample$'CM Frozen Food_Protein' + sample$'CM Impulse Food_Protein' + 
  sample$'CM Meat  Fish & Poultry_Protein' + sample$'CM Produce_Protein' + sample$'CM World Foods_Protein' + sample$'CM Customer Ordering_Protein' + sample$'CM Food Service_Protein')

# Total % Protein
sample$tot_prtn_pc <- (sample$total_prtn / sample$total_engy) * 100 # % energy that is protein

# Percentage of total protein due to each category
sample$baby_beauty_pc_totprtn <- ((4*sample$'CM Baby & Beauty_Protein') / sample$total_prtn) * 100
sample$bakery_pc_totprtn <- ((4*sample$'CM Bakery_Protein') / sample$total_prtn) * 100
sample$beer_wine_spirit_pc_totprtn <- ((4*sample$'CM Beers  Wines & Spirits_Protei') / sample$total_prtn) * 100
sample$canned_packaged_pc_totprtn <- ((4*sample$'CM Canned & Packaged_Protein') / sample$total_prtn) * 100
sample$dairy_pc_totprtn <- ((4*sample$'CM Dairy_Protein') / sample$total_prtn) * 100
sample$food_for_later_pc_totprtn <- ((4*sample$'CM Food For Later_Protein') / sample$total_prtn) * 100
sample$food_to_go_pc_totprtn <- ((4*sample$'CM Food To Go_Protein') / sample$total_prtn) * 100
sample$frozen_food_pc_totprtn <- ((4*sample$'CM Frozen Food_Protein') / sample$total_prtn) * 100
sample$impluse_pc_totprtn <- ((4*sample$'CM Impulse Food_Protein') / sample$total_prtn) * 100
sample$meat_fish_poultry_pc_totprtn <- ((4*sample$'CM Meat  Fish & Poultry_Protein') / sample$total_prtn) * 100
sample$produce_pc_totprtn <- ((4*sample$'CM Produce_Protein') / sample$total_prtn) * 100
sample$world_foods_pc_totprtn <- ((4*sample$'CM World Foods_Protein') / sample$total_prtn) * 100
sample$customer_ordering_pc_totprtn <- ((4*sample$'CM Customer Ordering_Protein') / sample$total_prtn) * 100
sample$food_service_pc_totprtn <- ((4*sample$'CM Food Service_Protein') / sample$total_prtn) * 100

# Calculate age bands (Age calcuate based on 13th March 2016)
sample$age_band <- NA
sample$age_band[sample$'Age_130316' >= 40 &  sample$'Age_130316' < 55] <- 1
sample$age_band[sample$'Age_130316' >= 55 &  sample$'Age_130316' < 70] <- 2
sample$age_band[sample$'Age_130316' >= 70] <- 3

# Recode unknown gender as missing
sample$GENDER[sample$GENDER == "U"] <- NA
sample$GENDER[sample$GENDER == ""] <- NA

# Join on IMD data
imd <- fread("C:/Data for Protein/CUSTOMER_IMD.tab")
sample <- merge(sample, imd, by = "IDNew", all.x =T)
rm(imd)

sample$quintile <- NA # Quintile from decile
sample$quintile[sample$`Index of Multiple Deprivation De` == 1 | sample$`Index of Multiple Deprivation De` == 2] <- 1
sample$quintile[sample$`Index of Multiple Deprivation De` == 3 | sample$`Index of Multiple Deprivation De` == 4] <- 2
sample$quintile[sample$`Index of Multiple Deprivation De` == 5 | sample$`Index of Multiple Deprivation De` == 6] <- 3
sample$quintile[sample$`Index of Multiple Deprivation De` == 7 | sample$`Index of Multiple Deprivation De` == 8] <- 4
sample$quintile[sample$`Index of Multiple Deprivation De` == 9 | sample$`Index of Multiple Deprivation De` == 10] <- 5

### Analysis ###

# calculate tables of data required

mean_prtn_dat <- sample[, list(tot_prtn_pc = mean(tot_prtn_pc, na.rm=T), baby_beauty_pc_totprtn = mean(baby_beauty_pc_totprtn, na.rm=T), 
                               bakery_pc_totprtn = mean(bakery_pc_totprtn, na.rm=T), beer_wine_spirit_pc_totprtn = mean(beer_wine_spirit_pc_totprtn, na.rm=T),
                               canned_packaged_pc_totprtn = mean(canned_packaged_pc_totprtn, na.rm=T), dairy_pc_totprtn = mean(dairy_pc_totprtn, na.rm=T),
                               food_for_later_pc_totprtn = mean(food_for_later_pc_totprtn, na.rm=T), food_to_go_pc_totprtn = mean(food_to_go_pc_totprtn, na.rm=T),
                               frozen_food_pc_totprtn = mean(frozen_food_pc_totprtn, na.rm=T), impluse_pc_totprtn = mean(impluse_pc_totprtn, na.rm=T),
                               meat_fish_poultry_pc_totprtn = mean(meat_fish_poultry_pc_totprtn, na.rm=T), produce_pc_totprtn = mean(produce_pc_totprtn, na.rm=T),
                               world_foods_pc_totprtn = mean(world_foods_pc_totprtn, na.rm=T), customer_ordering_pc_totprtn = mean(customer_ordering_pc_totprtn, na.rm=T),
                               food_service_pc_totprtn = mean(food_service_pc_totprtn, na.rm=T), count = .N), 
                        by = c("Age_130316", "GENDER")]

# Std errs
mean_prtn_dat$tot_prtn_pc_stderr <- sqrt((mean_prtn_dat$tot_prtn_pc*(100-mean_prtn_dat$tot_prtn_pc))/mean_prtn_dat$count)
mean_prtn_dat$baby_beauty_pc_totprtn_stderr <- sqrt((mean_prtn_dat$baby_beauty_pc_totprtn*(100-mean_prtn_dat$baby_beauty_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$bakery_pc_totprtn_stderr <- sqrt((mean_prtn_dat$bakery_pc_totprtn*(100-mean_prtn_dat$bakery_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$beer_wine_spirit_pc_totprtn_stderr <- sqrt((mean_prtn_dat$beer_wine_spirit_pc_totprtn*(100-mean_prtn_dat$beer_wine_spirit_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$canned_packaged_pc_totprtn_stderr <- sqrt((mean_prtn_dat$canned_packaged_pc_totprtn*(100-mean_prtn_dat$canned_packaged_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$dairy_pc_totprtn_stderr <- sqrt((mean_prtn_dat$dairy_pc_totprtn*(100-mean_prtn_dat$dairy_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$food_for_later_pc_totprtn_stderr <- sqrt((mean_prtn_dat$food_for_later_pc_totprtn*(100-mean_prtn_dat$food_for_later_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$food_to_go_pc_totprtn_stderr <- sqrt((mean_prtn_dat$food_to_go_pc_totprtn*(100-mean_prtn_dat$food_to_go_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$impluse_pc_totprtn_stderr <- sqrt((mean_prtn_dat$impluse_pc_totprtn*(100-mean_prtn_dat$impluse_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$frozen_food_pc_totprtn_stderr <- sqrt((mean_prtn_dat$frozen_food_pc_totprtn*(100-mean_prtn_dat$frozen_food_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$meat_fish_poultry_pc_totprtn_stderr <- sqrt((mean_prtn_dat$meat_fish_poultry_pc_totprtn*(100-mean_prtn_dat$meat_fish_poultry_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$world_foods_pc_stderr <- sqrt((mean_prtn_dat$world_foods_pc*(100-mean_prtn_dat$world_foods_pc))/mean_prtn_dat$count)
mean_prtn_dat$customer_ordering_pc_totprtn_stderr <- sqrt((mean_prtn_dat$customer_ordering_pc_totprtn*(100-mean_prtn_dat$customer_ordering_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$food_service_pc_totprtn_stderr <- sqrt((mean_prtn_dat$food_service_pc_totprtn*(100-mean_prtn_dat$food_service_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$produce_pc_totprtn_stderr <- sqrt((mean_prtn_dat$produce_pc_totprtn*(100-mean_prtn_dat$produce_pc_totprtn))/mean_prtn_dat$count)
mean_prtn_dat$count <- NULL

write.csv(mean_prtn_dat, "C:/Sainsburys/mean_protein_age.csv")

mean_prtn_dat2 <- sample[, list(tot_prtn_pc = mean(tot_prtn_pc, na.rm=T), baby_beauty_pc_totprtn = mean(baby_beauty_pc_totprtn, na.rm=T), 
                               bakery_pc_totprtn = mean(bakery_pc_totprtn, na.rm=T), beer_wine_spirit_pc_totprtn = mean(beer_wine_spirit_pc_totprtn, na.rm=T),
                               canned_packaged_pc_totprtn = mean(canned_packaged_pc_totprtn, na.rm=T), dairy_pc_totprtn = mean(dairy_pc_totprtn, na.rm=T),
                               food_for_later_pc_totprtn = mean(food_for_later_pc_totprtn, na.rm=T), food_to_go_pc_totprtn = mean(food_to_go_pc_totprtn, na.rm=T),
                               frozen_food_pc_totprtn = mean(frozen_food_pc_totprtn, na.rm=T), impluse_pc_totprtn = mean(impluse_pc_totprtn, na.rm=T),
                               meat_fish_poultry_pc_totprtn = mean(meat_fish_poultry_pc_totprtn, na.rm=T), produce_pc_totprtn = mean(produce_pc_totprtn, na.rm=T),
                               world_foods_pc_totprtn = mean(world_foods_pc_totprtn, na.rm=T), customer_ordering_pc_totprtn = mean(customer_ordering_pc_totprtn, na.rm=T),
                               food_service_pc_totprtn = mean(food_service_pc_totprtn, na.rm=T), count = .N), 
                         by = c("age_band", "GENDER")]

mean_prtn_dat2$tot_prtn_pc_stderr <- sqrt((mean_prtn_dat2$tot_prtn_pc*(100-mean_prtn_dat2$tot_prtn_pc))/mean_prtn_dat2$count)
mean_prtn_dat2$baby_beauty_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$baby_beauty_pc_totprtn*(100-mean_prtn_dat2$baby_beauty_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$bakery_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$bakery_pc_totprtn*(100-mean_prtn_dat2$bakery_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$beer_wine_spirit_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$beer_wine_spirit_pc_totprtn*(100-mean_prtn_dat2$beer_wine_spirit_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$canned_packaged_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$canned_packaged_pc_totprtn*(100-mean_prtn_dat2$canned_packaged_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$dairy_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$dairy_pc_totprtn*(100-mean_prtn_dat2$dairy_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$food_for_later_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$food_for_later_pc_totprtn*(100-mean_prtn_dat2$food_for_later_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$food_to_go_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$food_to_go_pc_totprtn*(100-mean_prtn_dat2$food_to_go_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$impluse_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$impluse_pc_totprtn*(100-mean_prtn_dat2$impluse_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$frozen_food_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$frozen_food_pc_totprtn*(100-mean_prtn_dat2$frozen_food_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$meat_fish_poultry_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$meat_fish_poultry_pc_totprtn*(100-mean_prtn_dat2$meat_fish_poultry_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$world_foods_pc_stderr <- sqrt((mean_prtn_dat2$world_foods_pc*(100-mean_prtn_dat2$world_foods_pc))/mean_prtn_dat2$count)
mean_prtn_dat2$customer_ordering_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$customer_ordering_pc_totprtn*(100-mean_prtn_dat2$customer_ordering_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$food_service_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$food_service_pc_totprtn*(100-mean_prtn_dat2$food_service_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$produce_pc_totprtn_stderr <- sqrt((mean_prtn_dat2$produce_pc_totprtn*(100-mean_prtn_dat2$produce_pc_totprtn))/mean_prtn_dat2$count)
mean_prtn_dat2$count <- NULL

write.csv(mean_prtn_dat2, "C:/Sainsburys/mean_protein_ageband.csv")

mean_prtn_imd_dec <- sample[, list(tot_prtn_pc = mean(tot_prtn_pc, na.rm=T), baby_beauty_pc_totprtn = mean(baby_beauty_pc_totprtn, na.rm=T), 
                               bakery_pc_totprtn = mean(bakery_pc_totprtn, na.rm=T), beer_wine_spirit_pc_totprtn = mean(beer_wine_spirit_pc_totprtn, na.rm=T),
                               canned_packaged_pc_totprtn = mean(canned_packaged_pc_totprtn, na.rm=T), dairy_pc_totprtn = mean(dairy_pc_totprtn, na.rm=T),
                               food_for_later_pc_totprtn = mean(food_for_later_pc_totprtn, na.rm=T), food_to_go_pc_totprtn = mean(food_to_go_pc_totprtn, na.rm=T),
                               frozen_food_pc_totprtn = mean(frozen_food_pc_totprtn, na.rm=T), impluse_pc_totprtn = mean(impluse_pc_totprtn, na.rm=T),
                               meat_fish_poultry_pc_totprtn = mean(meat_fish_poultry_pc_totprtn, na.rm=T), produce_pc_totprtn = mean(produce_pc_totprtn, na.rm=T),
                               world_foods_pc_totprtn = mean(world_foods_pc_totprtn, na.rm=T), customer_ordering_pc_totprtn = mean(customer_ordering_pc_totprtn, na.rm=T),
                               food_service_pc_totprtn = mean(food_service_pc_totprtn, na.rm=T), count = .N), 
                        by = c("Age_130316", "GENDER", "Index of Multiple Deprivation De")]

mean_prtn_imd_dec$tot_prtn_pc_stderr <- sqrt((mean_prtn_imd_dec$tot_prtn_pc*(100-mean_prtn_imd_dec$tot_prtn_pc))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$baby_beauty_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$baby_beauty_pc_totprtn*(100-mean_prtn_imd_dec$baby_beauty_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$bakery_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$bakery_pc_totprtn*(100-mean_prtn_imd_dec$bakery_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$beer_wine_spirit_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$beer_wine_spirit_pc_totprtn*(100-mean_prtn_imd_dec$beer_wine_spirit_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$canned_packaged_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$canned_packaged_pc_totprtn*(100-mean_prtn_imd_dec$canned_packaged_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$dairy_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$dairy_pc_totprtn*(100-mean_prtn_imd_dec$dairy_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$food_for_later_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$food_for_later_pc_totprtn*(100-mean_prtn_imd_dec$food_for_later_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$food_to_go_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$food_to_go_pc_totprtn*(100-mean_prtn_imd_dec$food_to_go_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$impluse_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$impluse_pc_totprtn*(100-mean_prtn_imd_dec$impluse_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$frozen_food_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$frozen_food_pc_totprtn*(100-mean_prtn_imd_dec$frozen_food_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$meat_fish_poultry_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$meat_fish_poultry_pc_totprtn*(100-mean_prtn_imd_dec$meat_fish_poultry_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$world_foods_pc_stderr <- sqrt((mean_prtn_imd_dec$world_foods_pc*(100-mean_prtn_imd_dec$world_foods_pc))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$customer_ordering_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$customer_ordering_pc_totprtn*(100-mean_prtn_imd_dec$customer_ordering_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$food_service_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$food_service_pc_totprtn*(100-mean_prtn_imd_dec$food_service_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$produce_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec$produce_pc_totprtn*(100-mean_prtn_imd_dec$produce_pc_totprtn))/mean_prtn_imd_dec$count)
mean_prtn_imd_dec$count <- NULL

write.csv(mean_prtn_imd_dec, "C:/Sainsburys/mean_protein_imd_dec.csv")

mean_prtn_imd_dec2 <- sample[, list(tot_prtn_pc = mean(tot_prtn_pc, na.rm=T), baby_beauty_pc_totprtn = mean(baby_beauty_pc_totprtn, na.rm=T), 
                                bakery_pc_totprtn = mean(bakery_pc_totprtn, na.rm=T), beer_wine_spirit_pc_totprtn = mean(beer_wine_spirit_pc_totprtn, na.rm=T),
                                canned_packaged_pc_totprtn = mean(canned_packaged_pc_totprtn, na.rm=T), dairy_pc_totprtn = mean(dairy_pc_totprtn, na.rm=T),
                                food_for_later_pc_totprtn = mean(food_for_later_pc_totprtn, na.rm=T), food_to_go_pc_totprtn = mean(food_to_go_pc_totprtn, na.rm=T),
                                frozen_food_pc_totprtn = mean(frozen_food_pc_totprtn, na.rm=T), impluse_pc_totprtn = mean(impluse_pc_totprtn, na.rm=T),
                                meat_fish_poultry_pc_totprtn = mean(meat_fish_poultry_pc_totprtn, na.rm=T), produce_pc_totprtn = mean(produce_pc_totprtn, na.rm=T),
                                world_foods_pc_totprtn = mean(world_foods_pc_totprtn, na.rm=T), customer_ordering_pc_totprtn = mean(customer_ordering_pc_totprtn, na.rm=T),
                                food_service_pc_totprtn = mean(food_service_pc_totprtn, na.rm=T), count = .N), 
                         by = c("age_band", "GENDER", "Index of Multiple Deprivation De")]

mean_prtn_imd_dec2$tot_prtn_pc_stderr <- sqrt((mean_prtn_imd_dec2$tot_prtn_pc*(100-mean_prtn_imd_dec2$tot_prtn_pc))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$baby_beauty_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$baby_beauty_pc_totprtn*(100-mean_prtn_imd_dec2$baby_beauty_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$bakery_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$bakery_pc_totprtn*(100-mean_prtn_imd_dec2$bakery_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$beer_wine_spirit_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$beer_wine_spirit_pc_totprtn*(100-mean_prtn_imd_dec2$beer_wine_spirit_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$canned_packaged_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$canned_packaged_pc_totprtn*(100-mean_prtn_imd_dec2$canned_packaged_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$dairy_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$dairy_pc_totprtn*(100-mean_prtn_imd_dec2$dairy_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$food_for_later_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$food_for_later_pc_totprtn*(100-mean_prtn_imd_dec2$food_for_later_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$food_to_go_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$food_to_go_pc_totprtn*(100-mean_prtn_imd_dec2$food_to_go_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$impluse_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$impluse_pc_totprtn*(100-mean_prtn_imd_dec2$impluse_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$frozen_food_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$frozen_food_pc_totprtn*(100-mean_prtn_imd_dec2$frozen_food_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$meat_fish_poultry_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$meat_fish_poultry_pc_totprtn*(100-mean_prtn_imd_dec2$meat_fish_poultry_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$world_foods_pc_stderr <- sqrt((mean_prtn_imd_dec2$world_foods_pc*(100-mean_prtn_imd_dec2$world_foods_pc))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$customer_ordering_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$customer_ordering_pc_totprtn*(100-mean_prtn_imd_dec2$customer_ordering_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$food_service_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$food_service_pc_totprtn*(100-mean_prtn_imd_dec2$food_service_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$produce_pc_totprtn_stderr <- sqrt((mean_prtn_imd_dec2$produce_pc_totprtn*(100-mean_prtn_imd_dec2$produce_pc_totprtn))/mean_prtn_imd_dec2$count)
mean_prtn_imd_dec2$count <- NULL

write.csv(mean_prtn_imd_dec2, "C:/Sainsburys/mean_prtn_imd_dec2.csv")

mean_prtn_imd_qnt <- sample[, list(tot_prtn_pc = mean(tot_prtn_pc, na.rm=T), baby_beauty_pc_totprtn = mean(baby_beauty_pc_totprtn, na.rm=T), 
                                   bakery_pc_totprtn = mean(bakery_pc_totprtn, na.rm=T), beer_wine_spirit_pc_totprtn = mean(beer_wine_spirit_pc_totprtn, na.rm=T),
                                   canned_packaged_pc_totprtn = mean(canned_packaged_pc_totprtn, na.rm=T), dairy_pc_totprtn = mean(dairy_pc_totprtn, na.rm=T),
                                   food_for_later_pc_totprtn = mean(food_for_later_pc_totprtn, na.rm=T), food_to_go_pc_totprtn = mean(food_to_go_pc_totprtn, na.rm=T),
                                   frozen_food_pc_totprtn = mean(frozen_food_pc_totprtn, na.rm=T), impluse_pc_totprtn = mean(impluse_pc_totprtn, na.rm=T),
                                   meat_fish_poultry_pc_totprtn = mean(meat_fish_poultry_pc_totprtn, na.rm=T), produce_pc_totprtn = mean(produce_pc_totprtn, na.rm=T),
                                   world_foods_pc_totprtn = mean(world_foods_pc_totprtn, na.rm=T), customer_ordering_pc_totprtn = mean(customer_ordering_pc_totprtn, na.rm=T),
                                   food_service_pc_totprtn = mean(food_service_pc_totprtn, na.rm=T), count = .N), 
                            by = c("Age_130316", "GENDER", "quintile")]

mean_prtn_imd_qnt$tot_prtn_pc_stderr <- sqrt((mean_prtn_imd_qnt$tot_prtn_pc*(100-mean_prtn_imd_qnt$tot_prtn_pc))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$baby_beauty_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$baby_beauty_pc_totprtn*(100-mean_prtn_imd_qnt$baby_beauty_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$bakery_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$bakery_pc_totprtn*(100-mean_prtn_imd_qnt$bakery_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$beer_wine_spirit_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$beer_wine_spirit_pc_totprtn*(100-mean_prtn_imd_qnt$beer_wine_spirit_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$canned_packaged_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$canned_packaged_pc_totprtn*(100-mean_prtn_imd_qnt$canned_packaged_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$dairy_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$dairy_pc_totprtn*(100-mean_prtn_imd_qnt$dairy_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$food_for_later_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$food_for_later_pc_totprtn*(100-mean_prtn_imd_qnt$food_for_later_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$food_to_go_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$food_to_go_pc_totprtn*(100-mean_prtn_imd_qnt$food_to_go_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$impluse_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$impluse_pc_totprtn*(100-mean_prtn_imd_qnt$impluse_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$frozen_food_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$frozen_food_pc_totprtn*(100-mean_prtn_imd_qnt$frozen_food_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$meat_fish_poultry_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$meat_fish_poultry_pc_totprtn*(100-mean_prtn_imd_qnt$meat_fish_poultry_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$world_foods_pc_stderr <- sqrt((mean_prtn_imd_qnt$world_foods_pc*(100-mean_prtn_imd_qnt$world_foods_pc))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$customer_ordering_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$customer_ordering_pc_totprtn*(100-mean_prtn_imd_qnt$customer_ordering_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$food_service_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$food_service_pc_totprtn*(100-mean_prtn_imd_qnt$food_service_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$produce_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt$produce_pc_totprtn*(100-mean_prtn_imd_qnt$produce_pc_totprtn))/mean_prtn_imd_qnt$count)
mean_prtn_imd_qnt$count <- NULL

write.csv(mean_prtn_imd_qnt, "C:/Sainsburys/mean_protein_imd_qnt.csv")

mean_prtn_imd_qnt2 <- sample[, list(tot_prtn_pc = mean(tot_prtn_pc, na.rm=T), baby_beauty_pc_totprtn = mean(baby_beauty_pc_totprtn, na.rm=T), 
                                    bakery_pc_totprtn = mean(bakery_pc_totprtn, na.rm=T), beer_wine_spirit_pc_totprtn = mean(beer_wine_spirit_pc_totprtn, na.rm=T),
                                    canned_packaged_pc_totprtn = mean(canned_packaged_pc_totprtn, na.rm=T), dairy_pc_totprtn = mean(dairy_pc_totprtn, na.rm=T),
                                    food_for_later_pc_totprtn = mean(food_for_later_pc_totprtn, na.rm=T), food_to_go_pc_totprtn = mean(food_to_go_pc_totprtn, na.rm=T),
                                    frozen_food_pc_totprtn = mean(frozen_food_pc_totprtn, na.rm=T), impluse_pc_totprtn = mean(impluse_pc_totprtn, na.rm=T),
                                    meat_fish_poultry_pc_totprtn = mean(meat_fish_poultry_pc_totprtn, na.rm=T), produce_pc_totprtn = mean(produce_pc_totprtn, na.rm=T),
                                    world_foods_pc_totprtn = mean(world_foods_pc_totprtn, na.rm=T), customer_ordering_pc_totprtn = mean(customer_ordering_pc_totprtn, na.rm=T),
                                    food_service_pc_totprtn = mean(food_service_pc_totprtn, na.rm=T), count = .N), 
                             by = c("age_band", "GENDER", "quintile")]

mean_prtn_imd_qnt2$tot_prtn_pc_stderr <- sqrt((mean_prtn_imd_qnt2$tot_prtn_pc*(100-mean_prtn_imd_qnt2$tot_prtn_pc))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$baby_beauty_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$baby_beauty_pc_totprtn*(100-mean_prtn_imd_qnt2$baby_beauty_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$bakery_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$bakery_pc_totprtn*(100-mean_prtn_imd_qnt2$bakery_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$beer_wine_spirit_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$beer_wine_spirit_pc_totprtn*(100-mean_prtn_imd_qnt2$beer_wine_spirit_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$canned_packaged_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$canned_packaged_pc_totprtn*(100-mean_prtn_imd_qnt2$canned_packaged_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$dairy_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$dairy_pc_totprtn*(100-mean_prtn_imd_qnt2$dairy_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$food_for_later_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$food_for_later_pc_totprtn*(100-mean_prtn_imd_qnt2$food_for_later_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$food_to_go_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$food_to_go_pc_totprtn*(100-mean_prtn_imd_qnt2$food_to_go_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$impluse_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$impluse_pc_totprtn*(100-mean_prtn_imd_qnt2$impluse_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$frozen_food_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$frozen_food_pc_totprtn*(100-mean_prtn_imd_qnt2$frozen_food_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$meat_fish_poultry_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$meat_fish_poultry_pc_totprtn*(100-mean_prtn_imd_qnt2$meat_fish_poultry_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$world_foods_pc_stderr <- sqrt((mean_prtn_imd_qnt2$world_foods_pc*(100-mean_prtn_imd_qnt2$world_foods_pc))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$customer_ordering_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$customer_ordering_pc_totprtn*(100-mean_prtn_imd_qnt2$customer_ordering_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$food_service_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$food_service_pc_totprtn*(100-mean_prtn_imd_qnt2$food_service_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$produce_pc_totprtn_stderr <- sqrt((mean_prtn_imd_qnt2$produce_pc_totprtn*(100-mean_prtn_imd_qnt2$produce_pc_totprtn))/mean_prtn_imd_qnt2$count)
mean_prtn_imd_qnt2$count <- NULL

write.csv(mean_prtn_imd_qnt2, "C:/Sainsburys/mean_prtn_imd_qnt2.csv")

# Total protein by age

mean_prtn_dat <- read.csv("C:/Sainsburys/mean_protein_age.csv")
mean_prtn_dat <- mean_prtn_dat[mean_prtn_dat$GENDER == "M" | mean_prtn_dat$GENDER == "F",] # Drop missing data
mean_prtn_dat <- mean_prtn_dat[mean_prtn_dat$Age_130316 >= 40 & mean_prtn_dat$Age_130316 <=90,] # Drop below age 90

mean_prtn_dat$tot_prtn_pc_low <- mean_prtn_dat$tot_prtn_pc - (mean_prtn_dat$tot_prtn_pc_stderr * 1.96) # 95% CIs
mean_prtn_dat$tot_prtn_pc_high <- mean_prtn_dat$tot_prtn_pc + (mean_prtn_dat$tot_prtn_pc_stderr * 1.96)

plot_totalprtn <- ggplot(mean_prtn_dat, aes(x=Age_130316, y=tot_prtn_pc, group=GENDER, color=GENDER)) +
                    geom_line(size = 1) + # Plot as line plot
                    geom_ribbon(data = mean_prtn_dat, aes(ymin=mean_prtn_dat$tot_prtn_pc_low, ymax=mean_prtn_dat$tot_prtn_pc_high), alpha=0.1, colour=NA) +
                    #scale_x_continuous(breaks = seq(40,100, by = 10)) +
                    #ylim(0,20) +
                    xlim(40,90) +
                    ylab("Percentage of total energy purchased which was Protein (%)") +
                    xlab("Age (years)") +
                    theme(legend.position="bottom") +
                    theme(legend.title=element_blank()) +
                    scale_color_manual(labels = c("Female", "Male"), values = c("dodgerblue2", "orangered2"))
print(plot_totalprtn) 

#ggsave(plot_totalprtn, "C:/Sainsburys/Plots/total_prtn_age.tiff", dpi=300)
#ggsave(plot_totalprtn, "C:/Sainsburys/Plots/total_prtn_age.eps", dpi=300)

# By deprivation

mean_prtn_imd_qnt <- read.csv("C:/Sainsburys/mean_protein_imd_qnt.csv")
mean_prtn_imd_qnt <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$GENDER == "M" | mean_prtn_imd_qnt$GENDER == "F",] # Drop missing data
mean_prtn_imd_qnt <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$Age_130316 >= 40 & mean_prtn_imd_qnt$Age_130316 <=90,] # Drop below age 90

mean_prtn_imd_qnt$tot_prtn_pc_low <- mean_prtn_imd_qnt$tot_prtn_pc - (mean_prtn_imd_qnt$tot_prtn_pc_stderr * 1.96) # 95% CIs
mean_prtn_imd_qnt$tot_prtn_pc_high <- mean_prtn_imd_qnt$tot_prtn_pc + (mean_prtn_imd_qnt$tot_prtn_pc_stderr * 1.96)

mean_prtn_imd_qnt$quintile <- as.factor(mean_prtn_imd_qnt$quintile)

males <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$GENDER == "M",]
females <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$GENDER == "F",]

# Plot males
ggplot(males, aes(x=Age_130316, y=tot_prtn_pc, group=quintile, color=quintile)) +
  geom_line() + # Plot as line plot
  geom_smooth(method = "loess", size = 1.5) +
  geom_ribbon(data = males, aes(ymin=tot_prtn_pc_low, ymax=tot_prtn_pc_high), alpha=0.1, colour=NA) +
  #scale_x_continuous(breaks = seq(40,100, by = 10)) +
  #ylim(7,22) +
  ylim(0,22) +
  xlim(40,90) +
  ylab("Percentage of total energy purchased which was Protein (%)") +
  xlab("Age (years)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
                     values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))


# Plot females
ggplot(females, aes(x=Age_130316, y=tot_prtn_pc, group=quintile, color=quintile)) +
  geom_line() + # Plot as line plot
  geom_smooth(method = "loess", size = 1.5) +
  geom_ribbon(data = females, aes(ymin=tot_prtn_pc_low, ymax=tot_prtn_pc_high), alpha=0.1, colour=NA) +
  #scale_x_continuous(breaks = seq(40,100, by = 10)) +
  #ylim(7,22) +
  #ylim(0,22) +
  xlim(40,90) +
  ylab("Percentage of total energy purchased which was Protein (%)") +
  xlab("Age (years)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
                     values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))
 

## Food Choices ##

# Meat and dairy check
ggplot(mean_prtn_dat, aes(x=Age_130316, y=meat_fish_poultry_pc_totprtn, group=GENDER, color=GENDER)) +
  geom_line() + 
ggplot(mean_prtn_dat, aes(x=Age_130316, y=dairy_pc_totprtn, group=GENDER, color=GENDER)) +
  geom_line() 


# Plot as line plot
  #geom_ribbon(data = mean_prtn_dat, aes(ymin=mean_prtn_dat$tot_prtn_pc_low, ymax=mean_prtn_dat$tot_prtn_pc_high), alpha=0.1, colour=NA) +
  #scale_x_continuous(breaks = seq(40,100, by = 10)) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total energy purchased which was Protein (%)") +
  xlab("Age (years)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
                     values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))

# All categories #

males <- mean_prtn_dat[mean_prtn_dat$GENDER == "M",]
females <- mean_prtn_dat[mean_prtn_dat$GENDER == "F",]

# Males
library(reshape2)
# Convert to long format - first % values
male_long <- melt(males, id = "Age_130316", 
                  measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                              "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                              "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                              "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                              "produce_pc_totprtn", "world_foods_pc_totprtn"))
# Now convert std errs
male_long2 <- melt(males, id = "Age_130316", 
                   measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                               "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                               "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                               "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                               "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
male_long2$std_err <- male_long2$value

male_long <- cbind(male_long, male_long2$std_err)

# Calcuate 95% CIs
male_long$low <- male_long$value - (male_long$`male_long2$std_err` * 1.96)
male_long$high <- male_long$value + (male_long$`male_long2$std_err` * 1.96)

ggplot(male_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_line(aes(y=value, group=variable, color=variable), size = 1.5) +
  geom_ribbon(data = male_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))


# Females
female_long <- melt(females, id = "Age_130316", 
                    measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                                "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                                "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                                "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                                "produce_pc_totprtn", "world_foods_pc_totprtn"))


female_long2 <- melt(females, id = "Age_130316", 
                     measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                                 "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                                 "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                                 "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                                "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))

female_long2$std_err <- female_long2$value
female_long <- cbind(female_long, female_long2$std_err)

# Calcuate 95% CIs
female_long$low <- female_long$value - (female_long$`female_long2$std_err` * 1.96)
female_long$high <- female_long$value + (female_long$`female_long2$std_err` * 1.96)

ggplot(female_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_line(aes(y=value, group=variable, color=variable), size = 1.5) +
  geom_ribbon(data = female_long, aes(ymin = lowci, ymax = highci), alpha=0.1, colour=NA) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))



# By Deprivation level

mean_prtn_imd_qnt <- read.csv("C:/Sainsburys/mean_protein_imd_qnt.csv")
mean_prtn_imd_qnt <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$GENDER == "M" | mean_prtn_imd_qnt$GENDER == "F",] # Drop missing data
mean_prtn_imd_qnt <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$Age_130316 >= 40 & mean_prtn_imd_qnt$Age_130316 <=90,] # Drop below age 90

males_imd <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$GENDER == "M",]
females_imd <- mean_prtn_imd_qnt[mean_prtn_imd_qnt$GENDER == "F",]

# # Meat
# ggplot(males_imd, aes(x=Age_130316, y=meat_fish_poultry_pc_totprtn, group=quintile, color=quintile)) +
#   geom_line() +
#   geom_smooth(size=1.5) +
#   ylim(18,28) +
#   xlim(40,90) +
#   ylab("Percentage of total energy purchased which was Protein (%)") +
#   xlab("Age (years)") +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
#                      values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))
# 
# ggplot(females_imd, aes(x=Age_130316, y=meat_fish_poultry_pc_totprtn, group=quintile, color=quintile)) +
#   geom_line() +
#   geom_smooth(size=1.5) +
#   ylim(18,28) +
#   xlim(40,90) +
#   ylab("Percentage of total energy purchased which was Protein (%)") +
#   xlab("Age (years)") +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
#                      values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))
# 
# # Dairy
# ggplot(males_imd, aes(x=Age_130316, y=dairy_pc_totprtn, group=quintile, color=quintile)) +
#   geom_line() +
#   geom_smooth(size=1.5) +
#   ylim(17,23) +
#   xlim(40,90) +
#   ylab("Percentage of total energy purchased which was Protein (%)") +
#   xlab("Age (years)") +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
#                      values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))
# 
# ggplot(females_imd, aes(x=Age_130316, y=dairy_pc_totprtn, group=quintile, color=quintile)) +
#   geom_line() +
#   geom_smooth(size=1.5) +
#   ylim(17,23) +
#   xlim(40,90) +
#   ylab("Percentage of total energy purchased which was Protein (%)") +
#   xlab("Age (years)") +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
#                      values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))
# 
# # Canned and packaged
# ggplot(males_imd, aes(x=Age_130316, y=canned_packaged_pc_totprtn, group=quintile, color=quintile)) +
#   geom_line() +
#   geom_smooth(size=1.5) +
#   ylim(9,18) +
#   xlim(40,90) +
#   ylab("Percentage of total energy purchased which was Protein (%)") +
#   xlab("Age (years)") +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
#                      values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))
# 
# ggplot(females_imd, aes(x=Age_130316, y=canned_packaged_pc_totprtn, group=quintile, color=quintile)) +
#   geom_line() +
#   geom_smooth(size=1.5) +
#   ylim(9,18) +
#   xlim(40,90) +
#   ylab("Percentage of total energy purchased which was Protein (%)") +
#   xlab("Age (years)") +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   scale_color_manual(labels = c("Quintile 1 (Most Deprived)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (Least Deprived)"),
#                      values = c("blueviolet", "dodgerblue4", "dodgerblue1", "deepskyblue", "lightskyblue1"))




# Males
library(reshape2)

# Split by quintile
mq1 <- males_imd[males_imd$quintile == 1,] # Most deprived
mq2 <- males_imd[males_imd$quintile == 2,] # 
mq3 <- males_imd[males_imd$quintile == 3,] # 
mq4 <- males_imd[males_imd$quintile == 4,] # 
mq5 <- males_imd[males_imd$quintile == 5,] # Least deprived

# Quintile 1

# Convert to long format first
temp <- melt(mq1, id = "Age_130316", 
                  measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                              "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                              "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                              "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                              "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(mq1, id = "Age_130316", 
                   measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                               "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                               "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                               "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                               "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
mq1_long <- cbind(temp, temp2$std_err)

mq1_long$low <- mq1_long$value - (mq1_long$`temp2$std_err` * 1.96)
mq1_long$high <- mq1_long$value + (mq1_long$`temp2$std_err` * 1.96)

# Plot
ggplot(mq1_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = mq1_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 2

# Convert to long format first
temp <- melt(mq2, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(mq2, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
mq2_long <- cbind(temp, temp2$std_err)

mq2_long$low <- mq2_long$value - (mq2_long$`temp2$std_err` * 1.96)
mq2_long$high <- mq2_long$value + (mq2_long$`temp2$std_err` * 1.96)

# Plot
ggplot(mq2_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = mq2_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 3

# Convert to long format first
temp <- melt(mq3, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(mq3, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
mq3_long <- cbind(temp, temp2$std_err)

mq3_long$low <- mq3_long$value - (mq3_long$`temp2$std_err` * 1.96)
mq3_long$high <- mq3_long$value + (mq3_long$`temp2$std_err` * 1.96)

# Plot
ggplot(mq3_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = mq3_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 4

# Convert to long format first
temp <- melt(mq4, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(mq4, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
mq4_long <- cbind(temp, temp2$std_err)

mq4_long$low <- mq4_long$value - (mq4_long$`temp2$std_err` * 1.96)
mq4_long$high <- mq4_long$value + (mq4_long$`temp2$std_err` * 1.96)

# Plot
ggplot(mq4_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = mq4_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 5

# Convert to long format first
temp <- melt(mq5, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(mq5, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
mq5_long <- cbind(temp, temp2$std_err)

mq5_long$low <- mq5_long$value - (mq5_long$`temp2$std_err` * 1.96)
mq5_long$high <- mq5_long$value + (mq5_long$`temp2$std_err` * 1.96)

# Plot
ggplot(mq5_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = mq5_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

### Females ###

# females
library(reshape2)

# Split by quintile
fq1 <- females_imd[females_imd$quintile == 1,] # Most deprived
fq2 <- females_imd[females_imd$quintile == 2,] # 
fq3 <- females_imd[females_imd$quintile == 3,] # 
fq4 <- females_imd[females_imd$quintile == 4,] # 
fq5 <- females_imd[females_imd$quintile == 5,] # Least deprived

# Quintile 1

# Convert to long format first
temp <- melt(fq1, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(fq1, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
fq1_long <- cbind(temp, temp2$std_err)

fq1_long$low <- fq1_long$value - (fq1_long$`temp2$std_err` * 1.96)
fq1_long$high <- fq1_long$value + (fq1_long$`temp2$std_err` * 1.96)

# Plot
ggplot(fq1_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = fq1_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 2

# Convert to long format first
temp <- melt(fq2, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(fq2, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
fq2_long <- cbind(temp, temp2$std_err)

fq2_long$low <- fq2_long$value - (fq2_long$`temp2$std_err` * 1.96)
fq2_long$high <- fq2_long$value + (fq2_long$`temp2$std_err` * 1.96)

# Plot
ggplot(fq2_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = fq2_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 3

# Convert to long format first
temp <- melt(fq3, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(fq3, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
fq3_long <- cbind(temp, temp2$std_err)

fq3_long$low <- fq3_long$value - (fq3_long$`temp2$std_err` * 1.96)
fq3_long$high <- fq3_long$value + (fq3_long$`temp2$std_err` * 1.96)

# Plot
ggplot(fq3_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = fq3_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 4

# Convert to long format first
temp <- melt(fq4, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(fq4, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
fq4_long <- cbind(temp, temp2$std_err)

fq4_long$low <- fq4_long$value - (fq4_long$`temp2$std_err` * 1.96)
fq4_long$high <- fq4_long$value + (fq4_long$`temp2$std_err` * 1.96)

# Plot
ggplot(fq4_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = fq4_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

# Quintile 5

# Convert to long format first
temp <- melt(fq5, id = "Age_130316", 
             measure = c("baby_beauty_pc_totprtn", "bakery_pc_totprtn", "beer_wine_spirit_pc_totprtn",
                         "canned_packaged_pc_totprtn", "customer_ordering_pc_totprtn", "dairy_pc_totprtn",
                         "food_for_later_pc_totprtn","food_to_go_pc_totprtn", "food_service_pc_totprtn",
                         "frozen_food_pc_totprtn", "impluse_pc_totprtn", "meat_fish_poultry_pc_totprtn",
                         "produce_pc_totprtn", "world_foods_pc_totprtn"))

temp2 <- melt(fq5, id = "Age_130316", 
              measure = c("baby_beauty_pc_totprtn_stderr", "bakery_pc_totprtn_stderr", "beer_wine_spirit_pc_totprtn_stderr",
                          "canned_packaged_pc_totprtn_stderr", "customer_ordering_pc_totprtn_stderr", "dairy_pc_totprtn_stderr",
                          "food_for_later_pc_totprtn_stderr","food_to_go_pc_totprtn_stderr", "food_service_pc_totprtn_stderr",
                          "frozen_food_pc_totprtn_stderr", "impluse_pc_totprtn_stderr", "meat_fish_poultry_pc_totprtn_stderr",
                          "produce_pc_totprtn_stderr", "world_foods_pc_stderr"))
temp2$std_err <- temp2$value
fq5_long <- cbind(temp, temp2$std_err)

fq5_long$low <- fq5_long$value - (fq5_long$`temp2$std_err` * 1.96)
fq5_long$high <- fq5_long$value + (fq5_long$`temp2$std_err` * 1.96)

# Plot
ggplot(fq5_long, aes(x=Age_130316, y=value, group=variable, color=variable)) +
  geom_point(aes(y=value, group=variable, color=variable), size = 1) +
  geom_ribbon(data = fq5_long, aes(ymin=low, ymax=high), alpha=0.1, colour=NA) +
  geom_smooth(method = "loess", size=1.2) +
  ylim(0,30) +
  xlim(40,90) +
  ylab("Percentage of total protein purchased by food category (%)") +
  xlab("Age (years)") +
  scale_color_manual(name = "Legend", 
                     labels = c("Baby & Beauty", "Bakery", "Beer, Wine & Spirits", "Canned & Packaged Foods", 
                                "Customer Ordering", "Dairy", "Food for Later",  "Food to Go", "Food Service", 
                                "Frozen Food", "Impulse", "Meat, Fish & Poutry", "Produce", "World Foods"),
                     values =c("hotpink", "chocolate1", "blue", "chartreuse3", "tan1", "yellow2", "dodgerblue2",
                               "purple", "plum1", "cadetblue1", "grey60", "orangered2", "palegreen1", "black"))

