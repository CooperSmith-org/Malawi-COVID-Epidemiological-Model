setwd("C:/Users/Noah/Documents/wsl/git/git/africa-covid-work/africa-covid-work/consolidation_files/files")

library(tidyverse)
cities = c("MW210", "MW107", "MW314", "MW315")

### import shell
shell <- read_csv('MW_Cleansed_Shell.csv') %>%
  select(Code, Population)


### pop density
pop_density <- read_csv('UN_Adjusted_PopDensity.csv')

pop_density <- pop_density %>%
  select(ADM3_PCODE, ADM2_PCODE, `_UNsum`, sqkm) %>%
  mutate(new_code = if_else(ADM2_PCODE %in% cities, ADM2_PCODE, ADM3_PCODE)) %>%
  group_by(new_code) %>%
  summarise(total_pop = sum(`_UNsum`), total_area = sum(sqkm)) %>%
  mutate(pop_density = total_pop / total_area) %>%
  select(Code = new_code, pop_density)

### elderly
elderly <- read_csv('TA_UN_Adjusted_Population_elderly.csv') %>%
  select(Code, pct_elderly = percentelderly)

### dry season
dry <- read_csv('DrySeason_PopMovemement_MNO_v3.csv') %>%
  select(Code, Rainy_Season_Movement)

### health facility coverage
health_facility <- read_csv('HFCoverage_v3 (1).csv')
health_facility$pct_not_covered <- as.numeric(health_facility$'%NotCovered')
health_facility <- health_facility %>%
  select(Code = code, pct_not_covered)

### pop_decay_model
decay <- read_csv('pop_decay_model.csv') %>%
  select(Code = region, interpolated_CI = final_score)

full_df <- shell
for (df in list(pop_density, elderly, dry, health_facility, decay)){
  full_df <- left_join(full_df, df, by="Code")
  
}



norm <- function(x) (x/sum(x, na.rm=TRUE) * 100)



out <- full_df %>%
  filter(Code != 'MW20903') %>%
  mutate_at(vars(-c(Code, Population)), norm) %>%
  mutate(final_rank = pop_density + pct_elderly + interpolated_CI)

outpath <- 'C:/Users/Noah/Documents/wsl/git/git/africa-covid-work/africa-covid-work/consolidation_files/output'
out %>% write_csv(paste0(outpath,'/calculated_rank.csv'), na="NA")
# 
#                