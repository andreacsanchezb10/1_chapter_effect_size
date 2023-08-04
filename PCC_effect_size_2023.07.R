install.packages("plyr")
library(Rtools)
library(readr)
library(plyr)
library(dplyr)

####### FARMER CHARACTERISTICS -------
##### Socio-demographic ------
#"hh age"
#"hh gender"
#"hh education"
#"h size"

## Household head Age (years)----
PCC_hh_age<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_age.csv")%>%
  filter(factor_metric_unit == "hh age (years)")

str(PCC_hh_age)
sort(unique(PCC_hh_age$factor_metric_unit))
length(sort(unique(PCC_hh_age$id))) # Number of articles 54

## Household head Gender (1= male, 0= female)----
PCC_hh_gender<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_gender.csv")%>%
  filter(factor_metric_unit == "hh gender (1= male, 0= female)")

str(PCC_hh_gender)
sort(unique(PCC_hh_gender$factor_metric_unit))
length(sort(unique(PCC_hh_gender$id))) # Number of articles 48

## Household head Education (years) ---- 
PCC_hh_education<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_education.csv")%>%
  filter(factor_metric_unit == "hh education (years)")

str(PCC_hh_education)
sort(unique(PCC_hh_education$factor_metric_unit))
length(sort(unique(PCC_hh_education$id))) # Number of articles 32

## Household size (number of people) ----
PCC_h_size<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_h_size.csv")%>%
  filter(factor_metric_unit == "h size (number of people)")

str(PCC_h_size)
sort(unique(PCC_h_size$factor_metric_unit))
length(sort(unique(PCC_h_size$id))) # Number of articles 37

##### Economic and financial capital ------
#"access to credit"
#"hh off-farm income" AND "hh engaged in off-farm activities"

## Access to credit ----
PCC_access_credit<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_access_credit.csv")%>%
  filter(factor_metric_unit == "access to credit (1= yes, 0= no)")

str(PCC_access_credit)
sort(unique(PCC_access_credit$factor_metric_unit))
length(sort(unique(PCC_access_credit$id))) # Number of articles 25

## Household off-farm income ----
PCC_off_farm_income<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_off_farm_income.csv")%>%
  filter(factor_metric_unit == "off-farm income (1= yes, 0= no)")

str(PCC_off_farm_income)
sort(unique(PCC_off_farm_income$factor_metric_unit))
length(sort(unique(PCC_off_farm_income$id))) # Number of articles 27

##### Information ------
#"hh farming experience"
#"hh association member"

## Household head farming experience (years) ----
PCC_hh_farming_experience<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_farming_experience.csv")%>%
  filter(factor_metric_unit == "hh farming experience (years)")

str(PCC_hh_farming_experience)
sort(unique(PCC_hh_farming_experience$factor_metric_unit))
length(sort(unique(PCC_hh_farming_experience$id))) # Number of articles 17

## Household head association member ----
PCC_hh_association_member<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_association_member.csv")%>%
  filter(factor_metric_unit == "hh association member (1= yes, 0= no)")

str(PCC_hh_association_member)
sort(unique(PCC_hh_association_member$factor_metric_unit))
length(sort(unique(PCC_hh_association_member$id))) # Number of articles 34

####### FARM CHARACTERISTICS -----
##### Social capital ------
#"farm labour force"

## Farm labour force----
PCC_farm_labour<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_labour.csv")%>%
  filter(factor_metric_unit == "farm labour force (number of people)")

str(PCC_farm_labour)
sort(unique(PCC_farm_labour$factor_metric_unit))
length(sort(unique(PCC_farm_labour$id))) # Number of articles 15

##### Physical capital ------
#"secured land tenure"
#"livestock ownership"

## Secure land tenure----
PCC_land_tenure_security<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_land_tenure_security.csv")%>%
  filter(factor_metric_unit == "secured land tenure (1= secure, 0= otherwise)")

str(PCC_land_tenure_security)
sort(unique(PCC_land_tenure_security$factor_metric_unit))
length(sort(unique(PCC_land_tenure_security$id))) # Number of articles 27

## Livestock ownership ----
PCC_livestock_ownership<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_livestock_ownership.csv")%>%
  filter(factor_metric_unit == "livestock ownership (TLU)")

str(PCC_livestock_ownership)
sort(unique(PCC_livestock_ownership$factor_metric_unit))
length(sort(unique(PCC_livestock_ownership$id))) # Number of articles 27

##### Biophysical ------
#"farm size"

## Farm size (ha)----
PCC_farm_size<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_size.csv")%>%
  filter(factor_metric_unit == "farm size (ha)")

str(PCC_farm_size)
sort(unique(PCC_farm_size$factor_metric_unit))
length(sort(unique(PCC_farm_size$id))) # Number of articles 47

####### CONTEXT CHARACTERISTICS -----
##### Information ------
#"agricultural extension"

## Access to agricultural extension ----
PCC_agricultural_extension<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_agricultural_extension.csv")%>%
  filter(factor_metric_unit == "agricultural extension (1= yes, 0= no)")

str(PCC_agricultural_extension)
sort(unique(PCC_agricultural_extension$factor_metric_unit))
length(sort(unique(PCC_agricultural_extension$id))) # Number of articles 27

##### Physical capital ------
#"distance to market" AND "distance to input market"
#"distance to road"

## Distance to market AND Distance to input market (Km) ----
PCC_distance_market<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_market.csv")%>%
  mutate(variance_value = as.character(variance_value))%>%
  filter(factor_metric_unit == "distance to market (km)")

str(PCC_distance_market)
sort(unique(PCC_distance_market$factor_metric_unit))
length(sort(unique(PCC_distance_market$id))) # Number of articles 47

## Distance to road (Km) ----
PCC_distance_road<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_road.csv")%>%
  filter(factor_metric_unit == "distance to road (km)")

str(PCC_distance_road)
sort(unique(PCC_distance_road$factor_metric_unit))
length(sort(unique(PCC_distance_road$id))) # Number of articles 7

#Combine all PCC data for effect size calculation
#1- "hh age" (years)
#2- "hh gender" (1=male)
#3- "hh education" (years)
#4- "hh farming experience (years)"
#5- "h size" (number of people)
#6- "farm size" (ha)
#7- "distance to market" AND "distance to input market" (km)

PCC_data<- bind_rows(PCC_access_credit,
                     PCC_agricultural_extension,
                     PCC_distance_market,
                     PCC_distance_road,
                     PCC_farm_labour,
                     PCC_farm_size,
                     PCC_h_size,
                     PCC_hh_age, 
                     PCC_hh_association_member,
                     PCC_hh_education,
                     PCC_hh_farming_experience,
                     PCC_hh_gender,
                     PCC_land_tenure_security,
                     PCC_livestock_ownership,
                     PCC_off_farm_income)
table(PPC_ES$factor_metric_unit)
sort(unique(PPC_ES$factor_metric_unit))
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
  summarise(n_articles = n_distinct(id))
  expand(intervention_recla_2)

factors_1<- factors%>%
  mutate(factor_metric_unit= str_to_sentence(factor_metric_unit))%>%
  group_by(factor_metric_unit)%>%
  mutate(n_articles = n_distinct(id))%>%ungroup()%>%
  group_by(factor_metric_unit,intervention_recla_2)%>%
  summarise(n_articles = n_distinct(id))%>%
  select(factor_metric_unit,intervention_recla_2,n_articles,n_articles)%>%
  right_join(factors_2, by= c("factor_metric_unit","intervention_recla_2"))
  pivot_wider(names_from = factor_metric_unit, values_from = "n_articles")%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

 

factors_2 <- rbind(factors_2, min, max)


# Demo data
exam_scores <- data.frame(
  row.names = c("Student.1", "Student.2", "Student.3"),
  Biology = c(7.9, 3.9, 9.4),
  Physics = c(10, 20, 0),
  Maths = c(3.7, 11.5, 2.5),
  Sport = c(8.7, 20, 4),
  English = c(7.9, 7.2, 12.4),
  Geography = c(6.4, 10.5, 6.5),
  Art = c(2.4, 0.2, 9.8),
  Programming = c(0, 0, 20),
  Music = c(20, 20, 20)
)
exam_scores

install.packages("fmsb")

library(fmsb)

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  Biology = c(20, 0), Physics = c(20, 0), Maths = c(20, 0),
  Sport = c(20, 0), English = c(20, 0), Geography = c(20, 0),
  Art = c(20, 0), Programming = c(20, 0), Music = c(20, 0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df


# Reduce plot margin using par()
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

