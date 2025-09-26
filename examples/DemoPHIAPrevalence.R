library(EMODAnalyzeR)
library(haven)
library(dplyr)

biomarker_path = "C:/Users/kaftad01/Documents/KENPHIA 2018 Household Interview and Biomarker Datasets v1.1 (DTA)/kenphia2018adultbio.dta"
household_path = "C:/Users/kaftad01/Documents/KENPHIA 2018 Household Interview and Biomarker Datasets v1.1 (DTA)/kenphia2018hh.dta"
# biomarker_path = "C:/Users/kaftad01/Documents/ZAMPHIA 2016 Household Interview and Biomarker Datasets v2.0 (DTA)/zamphia2016adultbio.dta"
# household_path = "C:/Users/kaftad01/Documents/ZAMPHIA 2016 Household Interview and Biomarker Datasets v2.0 (DTA)/zamphia2016hh.dta"
county_list = list(Homa_Bay=8, Kisii=16, Kisumu=17, Migori=27, Nyamira=34, Siaya=38) #list(Homa_Bay=1, Kisii=2, Kisumu=3, Migori=4, Nyamira=5, Siaya=6)
all_counties_list = list(`Baringo` = 1,
                    `Bomet` = 2,
                    `Bungoma` = 3,
                    `Busia` = 4,
                    `Elgeyo Marakwet` = 5,
                    `Embu` = 6,
                    `Garissa` = 7,
                    `Homa Bay` = 8,
                    `Isiolo` = 9,
                    `Kajiado` = 10,
                    `Kakamega` = 11,
                    `Kericho` = 12,
                    `Kiambu` = 13,
                    `Kilifi` = 14,
                    `Kirinyaga` = 15,
                    `Kisii` = 16,
                    `Kisumu` = 17,
                    `Kitui` = 18,
                    `Kwale` = 19,
                    `Laikipia` = 20,
                    `Lamu` = 21,
                    `Machakos` = 22,
                    `Makueni` = 23,
                    `Mandera` = 24,
                    `Marsabit` = 25,
                    `Meru` = 26,
                    `Migori` = 27,
                    `Mombasa` = 28,
                    `Muranga` = 29,
                    `Nairobi` = 30,
                    `Nakuru` = 31,
                    `Nandi` = 32,
                    `Narok` = 33,
                    `Nyamira` = 34,
                    `Nyandarua` = 35,
                    `Nyeri` = 36,
                    `Samburu` = 37,
                    `Siaya` = 38,
                    `Taita Tavet` = 39,
                    `Tana River` = 40,
                    `Tharaka` = 41,
                    `Trans-Nzoia` = 42,
                    `Turkana` = 43,
                    `Uasin Gishu` = 44,
                    `Vihiga` = 45,
                    `Wajir` = 46,
                    `West Pokot` = 47)

counties = tibble(county=t(data.frame(county_list)))
colnames(counties) <- "county"
counties = counties %>% mutate(province=county)

all_counties = tibble(county=t(data.frame(all_counties_list)))
colnames(all_counties) <- "county"
all_counties = all_counties %>% mutate(province=county)


df = read.phia.biomarker(biomarker_path)
df_hh = read_dta(household_path)
df_joined = inner_join(df, df_hh %>% select(county, centroidid, householdid), by=c("householdid","centroidid"))
#df_joined = df_joined %>% mutate(county = province)

just_nyanza = inner_join(df_joined, counties, by="county")
all_county_phia = inner_join(df_joined, all_counties, by="county")
calculate.phia_survey.prevalence(just_nyanza,age_min_inclusive = 15, age_max_inclusive = 49)
results = list()
i = 1
for (county. in county_list) {
  print(county.)
  results[[i]] = just_nyanza %>%
        filter(county.==county) %>%
        calculate.phia_survey.prevalence(age_min_inclusive = 15, age_max_inclusive = 49) %>%
        calculate.phia_survey.effective_count()
  results[[i]]$county = county.
  results[[i]]$age = "[15:50)"
  i = i + 1
}

i_all_counties = 1
results_all_counties = list()
for (county. in all_counties_list) {
  print(county.)
  results_all_counties[[i_all_counties]] = all_county_phia %>%
    filter(county.==county) %>%
    calculate.phia_survey.prevalence(age_min_inclusive = 15, age_max_inclusive = 49) %>%
    calculate.phia_survey.effective_count()
  results_all_counties[[i_all_counties]]$county = county.
  results_all_counties[[i_all_counties]]$age = "[15:50)"
  i_all_counties = i_all_counties + 1
}

for (start_age in seq(15,60,5)) {
  results[[i]] = just_nyanza %>%
    calculate.phia_survey.prevalence(age_min_inclusive = start_age, age_max_inclusive = start_age + 4) %>%
    calculate.phia_survey.effective_count()
  results[[i]]$county = 0
  results[[i]]$age = paste0("[",start_age,":",start_age + 5, ")")
  i = i + 1
}

results[[i]] = just_nyanza %>%
  calculate.phia_survey.prevalence(age_min_inclusive = 15, age_max_inclusive = 49) %>%
  calculate.phia_survey.effective_count()
results[[i]]$county = 0
results[[i]]$age = "[15:50)"


bind_rows(results)
bind_rows(results) %>%
  mutate(county = unstack(stack(county_list)[2:1])[paste(county),"res"]) %>%
  replace(is.na(.),"All") %>%
  replace(.=="female","Female") %>%
  replace(.=="male","Male") %>%
  replace(.=="all","All") %>%
  select(prevalence, gender,county, age, effective_count) %>% write.csv("out.csv")

bind_rows(results_all_counties) %>%
  mutate(county = unstack(stack(all_counties_list)[2:1])[paste(county),"res"]) %>%
  replace(is.na(.),"All") %>%
  replace(.=="female","Female") %>%
  replace(.=="male","Male") %>%
  replace(.=="all","All") %>%
  select(prevalence, gender,county, age, effective_count) %>% write.csv("out_yw.csv")
