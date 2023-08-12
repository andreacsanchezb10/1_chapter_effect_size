install.packages("plyr")
library(Rtools)
library(readr)
library(plyr)
library(dplyr)

####### FACTORS -------
## Access to credit (1= yes, 0= no) ----
PCC_access_credit<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_access_credit.csv")%>%
  filter(factor_metric_unit == "access to credit (1= yes, 0= no)")%>%
  mutate(variance_value=as.character(variance_value))


str(PCC_access_credit)
sort(unique(PCC_access_credit$factor_metric_unit))
length(sort(unique(PCC_access_credit$id))) # Number of articles 28

## Access to agricultural extension ----
PCC_agricultural_extension<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_agricultural_extension.csv")%>%
  filter(factor_metric_unit == "agricultural extension (1= yes, 0= no)")%>%
  mutate(variance_value=as.character(variance_value))


str(PCC_agricultural_extension)
sort(unique(PCC_agricultural_extension$factor_metric_unit))
length(sort(unique(PCC_agricultural_extension$id))) # Number of articles 29

## Agricultural training ----
PCC_agricultural_training<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_agricultural_training.csv")%>%
  filter(factor_metric_unit == "access to agricultural training (1= yes, 0= no)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_agricultural_training)
sort(unique(PCC_agricultural_training$factor_metric_unit))
length(sort(unique(PCC_agricultural_training$id))) # Number of articles 12
table(PCC_agricultural_training$factor_metric_unit)

## Distance from farm to house (Km, minutes) ----
PCC_distance_farm<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_farm.csv")%>%
  filter(factor_metric_unit == "distance farm-house (km)"|
           factor_metric_unit =="distance farm-house (minutes)")%>%
  mutate(factor_sub_class= as.character(factor_sub_class))%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_distance_farm)
sort(unique(PCC_distance_farm$factor_metric_unit))
length(sort(unique(PCC_distance_farm$id))) # Number of articles 14
table(PCC_distance_farm$factor_metric_unit)

## Distance to market AND Distance to input market (Km, minutes) ----
PCC_distance_market<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_market.csv")%>%
  filter(factor_metric_unit == "distance to market (km)" |
           factor_metric_unit == "distance to market (minutes)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_distance_market)
sort(unique(PCC_distance_market$factor_metric_unit))
length(sort(unique(PCC_distance_market$id))) # Number of articles 19
table(PCC_distance_market$factor_metric_unit)

## Distance to road (Km, minutes) ----
PCC_distance_road<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_road.csv")%>%
  filter(factor_metric_unit == "distance to road (km)"|
           factor_metric_unit =="distance to road (minutes)")%>%
  mutate(variance_value=as.character(variance_value))


str(PCC_distance_road)
sort(unique(PCC_distance_road$factor_metric_unit))
length(sort(unique(PCC_distance_road$id))) # Number of articles 10
table(PCC_distance_road$factor_metric_unit)

## Farm altitude (m.a.s.l.) ----
PCC_farm_altitude<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_altitude.csv")%>%
  filter(factor_metric_unit == "farm altitude (m.a.s.l.)")%>%
  mutate(factor_sub_class= as.character(factor_sub_class))%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_farm_altitude)
sort(unique(PCC_farm_altitude$factor_metric_unit))
length(sort(unique(PCC_farm_altitude$id))) # Number of articles 5

## Farm labour force (number of people) ----
PCC_farm_labour<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_labour.csv")%>%
  filter(factor_metric_unit == "farm labour force (number of people)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_farm_labour)
sort(unique(PCC_farm_labour$factor_metric_unit))
length(sort(unique(PCC_farm_labour$id))) # Number of articles 18

## Farm size (ha)----
PCC_farm_size<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_size.csv")%>%
  filter(factor_metric_unit == "farm size (ha)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_farm_size)
sort(unique(PCC_farm_size$factor_metric_unit))
length(sort(unique(PCC_farm_size$id))) # Number of articles 53

## Household asset ----
PCC_h_asset<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_h_asset.csv")%>%
  filter(factor_metric_unit == "h asset (USD)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_h_asset)
sort(unique(PCC_h_asset$factor_metric_unit))
length(sort(unique(PCC_h_asset$id))) # Number of articles 22

## Household size (number of people) ----
PCC_h_size<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_h_size.csv")%>%
  filter(factor_metric_unit == "h size (number of people)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_h_size)
sort(unique(PCC_h_size$factor_metric_unit))
length(sort(unique(PCC_h_size$id))) # Number of articles 39

## Household head Age (years)----
PCC_hh_age<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_age.csv")%>%
  filter(factor_metric_unit == "hh age (years)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_hh_age)
sort(unique(PCC_hh_age$factor_metric_unit))
length(sort(unique(PCC_hh_age$id))) # Number of articles 60

## Household head association member (1= yes, 0= no) ----
PCC_hh_association_member<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_association_member.csv")%>%
  filter(factor_metric_unit == "hh association member (1= yes, 0= no)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_hh_association_member)
sort(unique(PCC_hh_association_member$factor_metric_unit))
length(sort(unique(PCC_hh_association_member$id))) # Number of articles 37

## Household head Education (years) ---- 
PCC_hh_education<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_education.csv")%>%
  filter(factor_metric_unit == "hh education (years)")%>%
  mutate(variance_value=as.character(variance_value))


str(PCC_hh_education)
sort(unique(PCC_hh_education$factor_metric_unit))
length(sort(unique(PCC_hh_education$id))) # Number of articles 33

## Household head farming experience (years) ----
PCC_hh_farming_experience<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_farming_experience.csv")%>%
  filter(factor_metric_unit == "hh farming experience (years)")%>%
  mutate(variance_value=as.character(variance_value))


str(PCC_hh_farming_experience)
sort(unique(PCC_hh_farming_experience$factor_metric_unit))
length(sort(unique(PCC_hh_farming_experience$id))) # Number of articles 19

## Household head Gender (1= male, 0= female)----
PCC_hh_gender<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_gender.csv")%>%
  filter(factor_metric_unit == "hh gender (1= male, 0= female)")%>%
  mutate(variance_value=as.character(variance_value))


str(PCC_hh_gender)
sort(unique(PCC_hh_gender$factor_metric_unit))
length(sort(unique(PCC_hh_gender$id))) # Number of articles 52

## Household head is native (1= native, 0= otherwise) ---- 
PCC_hh_native<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_native.csv")%>%
  filter(factor_metric_unit == "hh is native (1= native, 0= otherwise)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_hh_native)
sort(unique(PCC_hh_native$factor_metric_unit))
length(sort(unique(PCC_hh_native$id))) # Number of articles 9

## Irrigation (1= secure, 0= otherwise) ----
PCC_irrigation<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_irrigation.csv")%>%
  filter(factor_metric_unit == "irrigation (1= yes, 0= no)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_irrigation)
sort(unique(PCC_irrigation$factor_metric_unit))
length(sort(unique(PCC_irrigation$id))) # Number of articles 8
table(PCC_irrigation$factor_metric_unit)


## Land tenure security (1= secure, 0= otherwise) ----
PCC_land_tenure_security<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_land_tenure_security.csv")%>%
  filter(factor_metric_unit == "secured land tenure (1= secure, 0= otherwise)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_land_tenure_security)
sort(unique(PCC_land_tenure_security$factor_metric_unit))
length(sort(unique(PCC_land_tenure_security$id))) # Number of articles 27


## Livestock ownership (TLU; (Units of animal owned))----
PCC_livestock_ownership<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_livestock_ownership.csv")%>%
  filter(factor_metric_unit == "livestock ownership (TLU)"| 
           factor_metric_unit == "livestock ownership (Units of animal owned)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_livestock_ownership)
sort(unique(PCC_livestock_ownership$factor_metric_unit))
length(sort(unique(PCC_livestock_ownership$id))) # Number of articles 19
table(PCC_livestock_ownership$factor_metric_unit)

## Household off-farm income (1= yes, 0= no) ----
PCC_off_farm_income<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_off_farm_income.csv")%>%
  filter(factor_metric_unit == "hh off-farm income (1= yes, 0= no)")%>%
  mutate(variance_value=as.character(variance_value))

str(PCC_off_farm_income)
sort(unique(PCC_off_farm_income$factor_metric_unit))
length(sort(unique(PCC_off_farm_income$id))) # Number of articles 30
table(PCC_off_farm_income$factor_metric_unit)

## Precipitation (mm) ----
PCC_precipitation<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_precipitation.csv")%>%
  filter(factor_metric_unit == "precipitation (mm)")%>%
  mutate(variance_value=as.character(variance_value),
         factor_sub_class= as.character(factor_sub_class))
  

str(PCC_precipitation)
sort(unique(PCC_precipitation$factor_metric_unit))
length(sort(unique(PCC_precipitation$id))) # Number of articles 6

## Soil erosion ----
PCC_soil_erosion<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_soil_erosion.csv")%>%
  filter(factor_metric_unit == "soil erosion (1 = major problem, 2 = minor problem, 3 = not a problem)")%>%
  mutate(variance_value=as.character(variance_value),
         factor_sub_class= as.character(factor_sub_class))

str(PCC_soil_erosion)
sort(unique(PCC_soil_erosion$factor_metric_unit))
length(sort(unique(PCC_soil_erosion$id))) # Number of articles 3
table(PCC_soil_erosion$factor_metric_unit)

## Soil fertility ----
PCC_soil_fertility<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_soil_fertility.csv")%>%
  filter(factor_metric_unit == "soil fertility (1= high fertility, 0= otherwise)")%>%
  mutate(variance_value=as.character(variance_value),
         factor_sub_class= as.character(factor_sub_class))
  

str(PCC_soil_fertility)
sort(unique(PCC_soil_fertility$factor_metric_unit))
length(sort(unique(PCC_soil_fertility$id))) # Number of articles 10
table(PCC_soil_fertility$factor_metric_unit)

## Soil slope  ----
PCC_soil_slope<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_soil_slope.csv")%>%
  filter(factor_metric_unit == "soil slope (1= steep slope, 0= otherwise)")%>%
  mutate(variance_value=as.character(variance_value),
         factor_sub_class= as.character(factor_sub_class))

str(PCC_soil_slope)
sort(unique(PCC_soil_slope$factor_metric_unit))
length(sort(unique(PCC_soil_slope$id))) # Number of articles 6
table(PCC_soil_slope$factor_metric_unit)

## Combine all PCC data for effect size calculation ----
PCC_access_credit
PCC_agricultural_extension
PCC_agricultural_training
PCC_distance_farm
PCC_distance_market
PCC_distance_road
PCC_farm_altitude
PCC_farm_labour
PCC_farm_size
PCC_h_asset
PCC_h_size
PCC_hh_age
PCC_hh_association_member
PCC_hh_education
PCC_hh_farming_experience
PCC_hh_gender
PCC_hh_native
PCC_irrigation
PCC_land_tenure_security
PCC_livestock_ownership
PCC_off_farm_income
PCC_precipitation
PCC_soil_erosion
PCC_soil_fertility
PCC_soil_slope

PCC_data<- bind_rows(PCC_access_credit,
                     PCC_agricultural_extension,
                     PCC_agricultural_training,
                     PCC_distance_farm,
                     PCC_distance_market,
                     PCC_distance_road,
                     PCC_farm_altitude,
                     PCC_farm_labour,
                     PCC_farm_size,
                     PCC_h_asset,
                     PCC_h_size,
                     PCC_hh_age, 
                     PCC_hh_association_member,
                     PCC_hh_education,
                     PCC_hh_farming_experience,
                     PCC_hh_gender,
                     PCC_hh_native,
                     PCC_irrigation,
                     PCC_land_tenure_security,
                     PCC_livestock_ownership,
                     PCC_off_farm_income,
                     PCC_precipitation,
                     PCC_soil_erosion,
                     PCC_soil_fertility,
                     PCC_soil_slope)

table(PCC_data$factor_metric_unit)
names(PCC_data)
sort(unique(PCC_data$factor_sub_class))
PCC_data$factor_context[PCC_data$factor_metric_unit %in% c("hh age (years)",
                                                           "hh gender (1= male, 0= female)",
                                                           "hh education (years)",
                                                           "h size (number of people)",
                                                           "access to credit (1= yes, 0= no)",
                                                           "off-farm income (1= yes, 0= no)",
                                                           "hh farming experience (years)",
                                                           "hh association member (1= yes, 0= no)")]<-"Farmer characteristics"

PCC_data$factor_context[PCC_data$factor_metric_unit %in% c("farm labour force (number of people)",
                                                           "secured land tenure (1= secure, 0= otherwise)",
                                                           "livestock ownership (TLU)",
                                                           "farm size (ha)")]<-"Farm characteristics"

PCC_data$factor_context[PCC_data$factor_metric_unit %in% c("farm labour force (number of people)",
                                                           "secured land tenure (1= secure, 0= otherwise)",
                                                           "livestock ownership (TLU)",
                                                           "farm size (ha)")]<-"Farm characteristics"
PCC_data$factor_context[PCC_data$factor_metric_unit %in% c("agricultural extension (1= yes, 0= no)",
                                                           "distance to market (km)",
                                                           "distance to road (km)")]<-"Context characteristics"                                                          
                                                           
length(sort(unique(PCC_data$id))) # Number of articles 78
table(PCC_data$factor)
sort(unique(PCC_data$effect_size_type))
sort(unique(PCC_data$y_metric_recla))
table(PCC_data$y_metric_recla)
sort(unique(PCC_data$model_method))
table(PCC_data$model_analysis_raw, PCC_data$model_method)


### Filter articles for meta-analysis ----
sort(unique(PCC_data$coefficient_type))

PCC_data$coefficient_type[PCC_data$coefficient_type %in% "coefficient value"] <- "B"
PCC_data$coefficient_type[PCC_data$coefficient_type %in% "marginal effect"] <- "ME"
PCC_data$coefficient_type[PCC_data$coefficient_type %in% c("odds ratio")] <- "OD"
sort(unique(PCC_data$coefficient_type))

sort(unique(PCC_data$variance_metric))
PCC_data$variance_metric[PCC_data$variance_metric %in% c("standard error", "robust standard error")] <- "SE"
PCC_data$variance_metric[PCC_data$variance_metric %in% c("t value", "t ratio")] <- "T"
PCC_data$variance_metric[PCC_data$variance_metric %in% c("z value")] <- "Z"
PCC_data$variance_metric[PCC_data$variance_metric %in% c("p value")] <- "P"
sort(unique(PCC_data$variance_metric))

sort(unique(PCC_data$model_method))

PCC_data<- PCC_data%>%
  mutate(coefficient_variance_type= paste(coefficient_type, variance_metric,sep = "_"),
         model_coefficient_variance_type= paste(model_method, coefficient_type, variance_metric,sep = "_"))

# Replace p == NA for 1
# Ruzzante et al (2019) replaced p== NA for 1
# Stanley and Doucouliagos recommend to use 0.1 OR 0.5
# Greenberg et al, (2003) use 0.3, as this is the midpoint between 0.10 and 0.5.
PCC_data$variance_value_num<- ifelse(PCC_data$variance_metric %in% "P" &
                                       is.na(PCC_data$variance_value_num) ,1,PCC_data$variance_value_num)

PCC_data$variance_value_num<- as.numeric(PCC_data$variance_value_num)

# Replace SE == 0 for 0.0001
PCC_data$variance_value_num<- ifelse(PCC_data$variance_metric %in% "SE" &
                                       PCC_data$variance_value_num %in% 0 ,0.0001,PCC_data$variance_value_num)
names(PCC_data)

### Calculate t value or z value ----
table(PCC_data$model_coefficient_variance_type)
sort(unique(PCC_data$model_coefficient_variance_type))
sort(unique(PCC_data$coefficient_variance_type))
sort(unique(PCC_data$model_method))
table(PCC_data$coefficient_variance_type,PCC_data$model_method )

# model_coefficient_variance_type == 
#"fixed_effects_B_SE"
#"instrumental_variable_B_SE"
#"logit_B_SE"
#"logit_ME_SE"
#"OLS_B_SE"
#"other_B_SE"
#"other_ME_SE"
#"probit_B_SE"
#"probit_ME_SE"
#"tobit_B_SE"
#"tobit_ME_SE"
#"truncated_B_SE"
# coefficient_variance_type == c("B_SE", "ME_SE")
# z= B/SE, t= B/SE
t_z_B_SE <- function (b, se) {  
  result<- (b/se)
  return(result)
}

PCC_data$z_t_value_recal[PCC_data$coefficient_variance_type %in% c("B_SE","ME_SE")] <-  
  t_z_B_SE(PCC_data$coefficient_num[PCC_data$coefficient_variance_type %in% c("B_SE","ME_SE")],
           PCC_data$variance_value_num[PCC_data$coefficient_variance_type %in% c("B_SE","ME_SE")])

# model_coefficient_variance_type == 
#"logit_B_P"
#"logit_ME_P"
#"probit_B_P"
# coefficient_variance_type == c("B_P", "ME_P")
# coefficient_num > 0 
# CHECK: Ref available: Kleinbaum, D. G., & Klein, M. (2010). Logistic regression: a self-learning text (3rd ed.). Springer Science & Business Media.
# SE = B/z; z=  Φ^−1(1-p/2) ∗ sign(B)
t_z_probit_logit_B_P <- function (p) {  
  result<- (qnorm(1-p/2))
  return(result)
}

PCC_data$z_t_value_recal[PCC_data$model_coefficient_variance_type %in%  c("probit_B_P", "logit_B_P", "logit_ME_P") & 
                           PCC_data$coefficient_num >= 0 ] <- 
  t_z_probit_logit_B_P(PCC_data$variance_value_num[PCC_data$model_coefficient_variance_type %in%  c("probit_B_P", "logit_B_P", "logit_ME_P") &
                                                     PCC_data$coefficient_num >= 0 ])

# model_coefficient_variance_type ==
#"logit_B_P"
#"logit_ME_P"
#"probit_B_P"
#"logit_ME_P"
# coefficient_variance_type == c("B_P", ME_P)
# coefficient_num < 0
# SE = B/z; z=  Φ^−1(p/2) ∗ sign(B)
t_z_probit_logit_B_P_2 <- function (p) {  
  result<- (qnorm(p/2))
  return(result)
}
PCC_data$z_t_value_recal[PCC_data$model_coefficient_variance_type %in%  c("probit_B_P", "logit_B_P", "logit_ME_P") & 
                           PCC_data$coefficient_num < 0 ] <- 
  t_z_probit_logit_B_P_2(PCC_data$variance_value_num[PCC_data$model_coefficient_variance_type %in%  c("probit_B_P", "logit_B_P", "logit_ME_P") &
                                                       PCC_data$coefficient_num < 0 ])

# model_coefficient_variance_type == 
#"OLS_B_P"
#"tobit_B_P"
# coefficient_variance_type == c("B_P")
#Formula from Ruzzante et al supp info
# t_z= t = Ft^−1 (p/2, df) ∗ sign(b)
# coefficient_num > 0
t_z_tobit_OLS_B_P <- function(p,n,k) {
  t <- qt(p/2, (n-k-1))
  return(t)
}

PCC_data$z_t_value_recal[PCC_data$model_coefficient_variance_type %in%  c("tobit_B_P")] <- 
  t_z_tobit_OLS_B_P(PCC_data$variance_value_num[PCC_data$model_coefficient_variance_type %in%  c("tobit_B_P")],
                PCC_data$n_samples_num[PCC_data$model_coefficient_variance_type %in%  c("tobit_B_P")],
                PCC_data$n_predictors_num[PCC_data$model_coefficient_variance_type %in%  c("tobit_B_P")])

# model_method == 
#"logit_B_T"
#"logit_B_Z"
#"other_B_T"
#"probit_B_T"
#"tobit_B_T"
# coefficient_variance_type == c("B_T", "B_Z")
# t_z= t OR z
t_z_ANY <- function (t_z) {  
  result<- t_z
  return(result)
}

PCC_data$z_t_value_recal[PCC_data$coefficient_variance_type %in%  c("B_T", "B_Z")] <- 
  t_z_ANY(PCC_data$variance_value_num[PCC_data$coefficient_variance_type %in%  c("B_T", "B_Z")])

#Things to check:
# Formula to get t value from GLM_B_P
# Formula to get t value from other_B_P
# Check if #737 should be included, it reports negative SE values.
# Check if z_t_value_recal has the same sign than coefficient_num

## Remove the rows with z_t_value_recal == NA
PPC_ES<- PCC_data%>%
  filter(!is.na(z_t_value_recal))

## Calculate the partial correlation effect size
#https://wviechtb.github.io/metadat/reference/dat.aloe2013.html
#install.packages("metafor")
library(metafor)

sort(unique(PPC_ES$intervention_recla))
table(PPC_ES$intervention_recla)

PPC_ES<-escalc(measure="PCOR", ti= z_t_value_recal, ni=n_samples_num, mi=n_predictors_num, data=PPC_ES)

### Meta-analysis function ----
table(PPC_ES$factor_metric_unit)

meta_regression_model <- function(data, metric_unit) {
  model_result <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                         data = data,
                         method = "REML", tdist = TRUE,
                         subset = (factor_metric_unit == metric_unit))
  return(summary(model_result))
}

# Vector of factor_metric_unit levels
factor_metric_units <- unique(PPC_ES$factor_metric_unit)

# List to store the results of all models
all_results <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- meta_regression_model(data = PPC_ES, metric_unit = unit)
  all_results[[unit]] <- result
}

library(tibble)
# Combine all results into one table
meta_regression_results <- do.call(rbind, all_results)
meta_regression_results <- as.data.frame(meta_regression_results)%>%
  rownames_to_column(., var = "factor_metric_unit")%>%
  mutate(ci.lb = sapply(ci.lb, as.numeric),
         ci.ub = sapply(ci.ub, as.numeric))


articles_count <- PPC_ES %>%
  group_by(factor_metric_unit) %>%
  summarise(n_articles = n_distinct(id))

install.packages("string")
library(stringi)

results<- meta_regression_results%>%
  mutate(beta = as.numeric(beta))%>%
  left_join(articles_count, by = "factor_metric_unit")%>%
  mutate(factor_metric_unit= as.factor(factor_metric_unit))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval<=0.1,"","")))))%>%
  select(factor_metric_unit, beta, ci.lb, ci.ub,significance,n_articles,k)
  

results$factor_context[results$factor_metric_unit %in% c("hh age (years)",
                                                           "hh gender (1= male, 0= female)",
                                                           "hh education (years)",
                                                           "h size (number of people)",
                                                           "access to credit (1= yes, 0= no)",
                                                           "off-farm income (1= yes, 0= no)",
                                                           "hh farming experience (years)",
                                                           "hh association member (1= yes, 0= no)")]<-"Farmer characteristics"

results$factor_context[results$factor_metric_unit %in% c("farm labour force (number of people)",
                                                           "secured land tenure (1= secure, 0= otherwise)",
                                                           "livestock ownership (TLU)",
                                                           "farm size (ha)")]<-"Farm characteristics"

results$factor_context[results$factor_metric_unit %in% c("farm labour force (number of people)",
                                                           "secured land tenure (1= secure, 0= otherwise)",
                                                           "livestock ownership (TLU)",
                                                           "farm size (ha)")]<-"Farm characteristics"
results$factor_context[results$factor_metric_unit %in% c("agricultural extension (1= yes, 0= no)",
                                                           "distance to market (km)",
                                                           "distance to road (km)")]<-"Context characteristics" 

results$factor_metric_unit<-str_to_sentence(results$factor_metric_unit)
#install.packages("tidyverse")
library(tidyverse)

results<- results%>%
  mutate(factor_metric_unit = paste(factor_metric_unit, " (", n_articles,", ",k, ")", sep = ""))

results$factor_metric_unit = with(results, reorder(factor_metric_unit, factor_context, median))
  
#install.packages("pals")
library(ggplot2)
library(pals)
library(RColorBrewer)

dark2_palette <- brewer.pal(n = 8, name = "Accent")
dark2_palette


ggplot(data=results, aes(y=factor_metric_unit,x=beta,xmin=ci.lb, xmax=ci.ub,
                                colour = factor(factor_context) ))+
  geom_vline(xintercept=0, colour = "grey20",linetype = 1, linewidth=0.7)+
  geom_errorbar(width=0.2,size=1, position = (position_dodge(width = -0.2)),show.legend = TRUE)+
  geom_point(size = 4, position = (position_dodge(width = -0.2)),show.legend = TRUE)+
  geom_text(aes(label=significance, x=ci.ub+0.01, group=factor_metric_unit), vjust=0.7, hjust=-0.005,
            color="black", size=7, family="sans",face="bold",position = (position_dodge(width = -0.5)))+
  scale_colour_brewer(palette = "Accent", name= "Factors class")+
  scale_y_discrete(limits=rev, labels= results$factor_metric_unit, breaks=results$factor_metric_unit,
                   position = "left")+
  xlab("PCC")+
  theme(axis.text.x = element_text(color="black",size=12,  family = "sans",
                                   margin = margin(t = 5, r = 0, b = 5, l = 0)),
        axis.text.y = element_text(color="black",size=11, family = "sans",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="black",size=12, family = "sans",face="bold",
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
        plot.background = element_rect(fill = "White", color = "White"),
        panel.background = element_rect(fill = "White", color = "White"),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major  = element_line(color = "grey90",size = 0.6),
        axis.line = element_line(colour = "black"),
        legend.box.background = element_rect(color="white", size=0.5),
        legend.text = element_text(color="black",size=11,family = "sans"),
        legend.title = element_text(color="black",size=11,family = "sans",face="bold" ),
        legend.title.align=0.5,
        legend.position = "bottom",
        legend.key = element_rect(fill = "white"))


### Figure: Number of articles by country
#Study locations
sort(unique(PPC_ES$country))
#install.packages("countrycode")
library(countrycode)
library(readxl)

UN_region <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_effect_size/UN_region.xlsx", sheet = "UN_subregion")%>%
  mutate(Country_Name= if_else(Country_Name == "United States of America (The)","USA",
                                  if_else(Country_Name =="United Republic of Tanzania (The)","Tanzania",
                                          if_else(Country_Name =="Bolivia (Plurinational State of)","Bolivia",
                                                  if_else(Country_Name == "Sudan (The)", "Sudan",
                                                     if_else(Country_Name=="Republic of Moldova (The)", "Moldova",
                                                             if_else(Country_Name=="Philippines (The)", "Philippines",
                                                  Country_Name)))))))

sort(unique(UN_region$Country_Name))

PPC_ES$country[PPC_ES$country %in% "Vietnam, Thailand"] <- "Thailand"
PPC_ES$country[PPC_ES$country %in% "Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia"] <- "Ethiopia"
PPC_ES$country[PPC_ES$country %in% "Vietnam"] <- "Viet Nam"

length(unique(PPC_ES$id)) #75
length(unique(PPC_ES$country)) #28
sort(unique(PPC_ES$country))
names(UN_region)

country<- PPC_ES%>%
  select("id", "country")%>%
  left_join(UN_region, by=c("country" ="Country_Name"))%>%
  group_by(country)%>%
  mutate(n_articles = n_distinct(id))%>%
  group_by(country,n_articles, UN_Regions,UN_sub_region,World_Bank_Income_Group_Combined_13)%>%
  tally()

sort(unique(country$UN_Regions))

length(sort(unique(country$country))) #total number of countries #28


world <- ggplot2::map_data("world")%>%filter(region != "Antarctica")

world_map <- ggplot2::map_data("world")%>%filter(region != "Antarctica")%>%
  left_join(country, by =  c("region" ="country"))%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate(n_articles_intervals= cut(n_articles,seq(0,8,2)))%>%
  mutate(n_articles_intervals= if_else(n_articles_intervals=="(0,2]", "(1,2]",
                                               n_articles_intervals))%>%
  mutate_all(~replace(., is.na(.), 0))
  
#mutate(region= if_else(continent=="0","",region))
#world_map$N_articles_frequency <- cut(world_map$n_articles,breaks = c(0,1,2,3,4,5,6,7))
sort(unique(world_map$N_articles_frequency))
View(world_map)

sort(unique(world_map$UN_subregion))
world_prueba <- table(cut(world_map$n_articles,seq(0,8,2)))
world_prueba


ggplot(data = world_map, aes(x = long, y = lat, group = group, fill = n_articles_intervals)) +
  geom_polygon(color = "grey60", size = 0.05, show.legend = TRUE) +
  coord_fixed() +
  scale_fill_manual(labels = c("No data", "1-2", "3-4", "5-6","7-8"),
                    breaks = c("0", "(1,2]", "(2,4]", "(4,6]","(6,8]"),
                    values = c("white", "#B2DF8A", "#33A02C","#A6CEE3","#1F78B4"),
                    guide = guide_legend(label.position = "top"))+
  labs(fill = "Number of articles")+
  theme(legend.position = "bottom",
        legend.title =element_text(color="black",size=10, family = "sans",face="bold",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.text = element_text(color="black",size=10, family = "sans",face="bold"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.text.align =1,
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.spacing.x = unit(0, 'cm'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color="black", size=9, family = "sans",face="bold",hjust = 0.01,vjust = -7))+
        #plot.margin = margin(0, 0, 0, 0, "cm"))+
  labs(x = NULL, y = NULL)

"#FFFF99"

# Spider diagram showing the number of articles per factor, per system
install.packages("grafify")
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(scales)
library(ggplot2)
library(stringr)
library("grafify")

sort(unique(PPC_ES$intervention_recla))


factors<- PPC_ES
factors$intervention_recla_2[factors$intervention_recla%in% c("agroforestry and fallow",
                                                              "crop rotation and intercropping",
                                                              "intercropping or crop rotation")] <- "Combined practices"

factors$intervention_recla_2[factors$intervention_recla%in% c("embedded seminatural infrastructures",
                                                              "soil bund with contour cultivation")]<- "Embedded seminatural infrastructures"
factors$intervention_recla_2[factors$intervention_recla%in% c("agroforestry")]<- "Agroforestry"
factors$intervention_recla_2[factors$intervention_recla%in% c("crop rotation")]<- "Crop rotation"
factors$intervention_recla_2[factors$intervention_recla%in% c("grazing cut and carry",
                                                              "integrated crop-livestock")]<- "Crop-silvopasture systems"
factors$intervention_recla_2[factors$intervention_recla%in% c("intercropping",
                                                              "pull-push")]<- "Intercropping"
factors$intervention_recla_2[factors$intervention_recla%in% c("land with temporary fallow")]<- "Fallow"


sort(unique(factors$intervention_recla))
sort(unique(factors$intervention_recla_2))

factors_2<- factors%>%
  mutate(factor_metric_unit= str_to_sentence(factor_metric_unit))%>%
  group_by(factor_metric_unit)%>%
  mutate(n_articles = n_distinct(id))%>%ungroup()%>%
  group_by(factor_metric_unit,intervention_recla_2)%>%
  summarise(n_articles = n_distinct(id))%>%
  select(factor_metric_unit,intervention_recla_2,n_articles)%>%
  pivot_wider(names_from = factor_metric_unit, values_from = "n_articles")%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
  select(!intervention_recla_2)


factors_2<- factors_2%>%
  add_row(.before = 1)%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 21))%>%
  add_row(.before = 2)%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
#install.packages("fmsb")
library(fmsb)

dark2_palette <- brewer.pal(n = 8, name = "Set1")
dark2_palette


# Create data: note in High school for several students
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# Color vector
colors_border=c( "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628" )

# plot with default options:
radarchart( factors_2  , axistype=1 , 
            #custom polygon
            pcol=colors_border  , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,25,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=1.2, y=1.3, legend = rownames(factors_2[-c(1,2),]), bty = "n", pch=20 , col=colors_border ,
       text.col = "grey20", cex=1.2, pt.cex=3)
