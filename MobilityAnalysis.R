library(tidyverse)
library(dplyr)

#Auto Pull from Google Mobility Report Website
gm <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

#Frequency Per Week Estimation Table
fe <- read.csv("inputs/FrequencyEstimations.csv")


#ConvertColumn Names to more appropriate naming convention
colnames(gm)[colnames(gm) == 'country_region_code'] <- 'countrycode'
colnames(gm)[colnames(gm) == 'country_region'] <- 'countryname'
colnames(gm)[colnames(gm) == 'retail_and_recreation_percent_change_from_baseline'] <- 'retailrec'
colnames(gm)[colnames(gm) == 'grocery_and_pharmacy_percent_change_from_baseline'] <- 'grocerypharmacy'
colnames(gm)[colnames(gm) == 'parks_percent_change_from_baseline'] <- 'parks'
colnames(gm)[colnames(gm) == 'transit_stations_percent_change_from_baseline'] <- 'transitstations'
colnames(gm)[colnames(gm) == 'workplaces_percent_change_from_baseline'] <- 'workplaces'
colnames(gm)[colnames(gm) == 'residential_percent_change_from_baseline'] <- 'residential'

#Convert Frequency Table into more readable headings
colnames(fe)[colnames(fe) == 'Social.Distancing'] <- 'policy1'
colnames(fe)[colnames(fe) == 'Additional.Pop..Guidelines'] <- 'policy2'
colnames(fe)[colnames(fe) == 'Enforced.Pop.Restrictions'] <- 'policy3'

#Add Retail & Recreation, Grocery & Pharmacy, Parks, Transit Station & Workplace Frequency by Policy to the Google Mobility Reports
gm$rrp1 <-fe[1,2]
gm$rrp2 <-fe[1,3]
gm$rrp3 <= fe[1,4]

gm$gpp1 <-fe[2,2]
gm$gpp2 <-fe[2,3]
gm$gpp3 <-fe[2,4]

gm$pp1 <- fe[3,2]
gm$pp2 <- fe[3,3]
gm$pp3 <- fe[3,4]

gm$tsp1 <- fe[4,2]
gm$tsp2 <- fe[4,3]
gm$tsp3 <- fe[4,4]

gm$wp1 <- fe[5,2]
gm$wp2 <- fe[5,3]
gm$wp3 <- fe[5,4]

#Sum Key Categories to get denominators or weights for each policy
gm$p1denom <- rowSums(gm[, c("rrp1", "gpp1", "tsp1", "wp1")])
gm$p2denom <- rowSums(gm[, c("rrp2", "gpp2", "tsp2", "wp2")])
gm$p3denom <- rowSums(gm[, c("gpp3", "tsp3", "wp3")])

#Compute Reduction Rate Under Policy 1
gm$p1reducrate <- ((gm$rrp1 / gm$p1denom)*gm$retailrec)+((gm$gpp1 / gm$p1denom)*gm$grocerypharmacy)+((gm$tsp1 / gm$p1denom)*gm$transitstations)+((gm$wp1 / gm$p1denom)*gm$workplaces)
gm$p2reducrate <- ((gm$rrp2 / gm$p2denom)*gm$retailrec)+((gm$gpp2 / gm$p2denom)*gm$grocerypharmacy)+((gm$tsp2 / gm$p2denom)*gm$transitstations)+((gm$wp2 / gm$p2denom)*gm$workplaces)
gm$p3reducrate <- ((gm$gpp3 / gm$p3denom)*gm$grocerypharmacy)+((gm$tsp3 / gm$p3denom)*gm$transitstations)+((gm$wp3 / gm$p3denom)*gm$workplaces)

gm_refined <- gm %>%
  select(countrycode, countryname, sub_region_1, sub_region_2, date, p1reducrate, p2reducrate, p3reducrate)

gm_countrylevel <- gm %>%
  select(countrycode, countryname, sub_region_1, sub_region_2, date, p1reducrate, p2reducrate, p3reducrate) %>%
  filter(is.na(sub_region_1)==TRUE & is.na(sub_region_2)==TRUE)

gm_countrylevel_average <- gm_countrylevel %>%
  select(countrycode, countryname, date, p2reducrate)
  
gm_countrylevel_average$p2reducrate <- gm_countrylevel_average$p2reducrate/100

#Get the lists of countries
SSA_COVID_Inputs <- read_csv("inputs/SSA COVID Inputs.csv")
countrylist <- unique(SSA_COVID_Inputs$Lvl2)

#Filter for given countries
gm_countrylevel_average <- filter(gm_countrylevel_average, gm_countrylevel_average$countryname %in% countrylist)

#Get the start dates
start_dates <- read_csv("inputs/start date.csv")
start_dates$Date<- as.Date(start_dates$Date, "%m/%d/%y")

#Join the start dates
merged_df <- merge(gm_countrylevel_average, start_dates, by.x="countryname", by.y ="Country", all.x = TRUE)
merged_df$time <- merged_df$date - merged_df$Date
merged_df$time <- as.numeric(merged_df$time)

#Grab only the dates where time >0
merged_df <- filter(merged_df, merged_df$time >= 0)
merged_df$p2reducrate <- -1*merged_df$p2reducrate

#Make a df of all days
totaldays <- as.data.frame(seq(1, 365))

#Split merged_df
individual <- split(merged_df, f = merged_df$countryname)
totaldays <- as.data.frame(seq(1, 365))

#create a list to loop over +
new_dfs <- list()

#loop and create the lists
for (i in 1:length(individual)){
  new_df <- merge(totaldays, individual[[i]], by.x="seq(1, 365)", by.y="time", all.x = TRUE) %>%
    fill(p2reducrate, .direction = "down")
  names(new_df)[names(new_df)=="p2reducrate"] <- "reduc"
  write.csv(new_df$reduc, paste0("inputs/SSA/",names(individual[i]),".csv"))
}