install.packages("plyr")
library(Rtools)
library(readr)
library(plyr)
library(dplyr)
library(funModeling)

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

##### Information ------
#"hh farming experience "

## Household head farming experience (years)----
PCC_hh_farming_experience<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_farming_experience.csv")%>%
  filter(factor_metric_unit == "hh farming experience (years)")

str(PCC_hh_farming_experience)
sort(unique(PCC_hh_farming_experience$factor_metric_unit))
length(sort(unique(PCC_hh_farming_experience$id))) # Number of articles 17


####### FARM CHARACTERISTICS -----
##### Biophysical ------
#"farm size"
## Farm size (ha)----
PCC_farm_size<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_size.csv")%>%
  filter(factor_metric_unit == "farm size (ha)")

str(PCC_farm_size)
sort(unique(PCC_farm_size$factor_metric_unit))
length(sort(unique(PCC_farm_size$id))) # Number of articles 47

####### CONTEXT CHARACTERISTICS -----
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


#Combine all PCC data for effect size calculation
#1- "hh age" (years)
#2- "hh gender" (1=male)
#3- "hh education" (years)
#4- "hh farming experience (years)"
#5- "h size" (number of people)
#6- "farm size" (ha)
#7- "distance to market" AND "distance to input market" (km)

PCC_data<- bind_rows(PCC_hh_age, 
                     PCC_hh_gender,
                     PCC_hh_education,
                     PCC_hh_farming_experience,
                     PCC_h_size,
                     PCC_farm_size,
                     PCC_distance_market)

length(sort(unique(PCC_data$id))) # Number of articles 77
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
                           PCC_data$coefficient_num > 0 ] <- 
  t_z_probit_logit_B_P(PCC_data$variance_value_num[PCC_data$model_coefficient_variance_type %in%  c("probit_B_P", "logit_B_P", "logit_ME_P") &
                                                     PCC_data$coefficient_num > 0 ])

# model_coefficient_variance_type ==
#"logit_B_P"
#"logit_ME_P"
#"probit_B_P"
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

names(PPC_ES)

PPC_ES<-escalc(measure="PCOR", ti= z_t_value_recal, ni=n_samples_num, mi=n_predictors_num, data=PPC_ES)

### Meta-analysis function
PCC_meta_analysis <- function(subset_arg) {
  rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = PPC_ES,
         method = "REML", tdist = TRUE, subset = subset_arg)
}

table(PPC_ES$intervention_recla,PPC_ES$factor_metric_unit )


###------ Gender: binary (1= male, 0= female) -------------
run_model <- function(data, metric_unit) {
  model_result <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                         data = data,
                         method = "REML", tdist = TRUE,
                         subset = (factor_metric_unit == metric_unit))
  return(summary(model_result))
}

# Assuming you have defined 'PPC_ES' and 'yi' and 'vi' variables before running the function.

# Vector of factor_metric_unit levels
factor_metric_units <- unique(PPC_ES$factor_metric_unit)

# List to store the results of all models
all_results <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- run_model(data = PPC_ES, metric_unit = unit)
  all_results[[unit]] <- result
}

# Combine all results into one table
combined_results <- do.call(rbind, all_results)



#Farm size (ha)
farm_size<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = PPC_ES,
                                method = "REML", tdist = TRUE,subset = (factor_metric_unit=="farm size (ha)"))

summary(farm_size)

farm_size_intercropping<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                                 method = "REML", tdist = TRUE,subset = (factor=="Farm size (ha)"& intervention_recla=="intercropping"))

summary(farm_size_intercropping)

farm_size_rotation<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                            method = "REML", tdist = TRUE,subset = (factor=="Farm size (ha)"& intervention_recla=="crop rotation"))

summary(farm_size_rotation)

#"Gender (1= Male, 0= Female)"
gender_agroforestry<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                             method = "REML", tdist = TRUE,subset = (factor=="Gender (1= Male, 0= Female)"& intervention_recla=="agroforestry"))

summary(gender_agroforestry)

gender_intercropping<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                              method = "REML", tdist = TRUE,subset = (factor=="Gender (1= Male, 0= Female)"& intervention_recla=="intercropping"))

summary(gender_intercropping)

gender_rotation<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                         method = "REML", tdist = TRUE,subset = (factor=="Gender (1= Male, 0= Female)"& intervention_recla=="crop rotation"))

summary(gender_rotation)



unique_factors <- unique(subset(adoption_yes_no_meta, factor == "Farm size (ha)")$intervention_recla)
unique_factors
factor_intervention

#this code works!
run_models <- function(data) {
  interventions <- unique(subset(data, factor == "Farm size (ha)")$intervention_recla)
  results <- list()
  
  for (models in models) {
    subset_data <- subset(data, factor == "Farm size (ha)" & intervention_recla == interventions)
    model_name <- paste("farm_size", model, sep = "_")
    model_result <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                           data = subset_data,
                           method = "REML", tdist = TRUE)
    results[[model_name]] <- summary(model_result)
  }
  
  return(results)
}

# Usage
results <- run_models(adoption_yes_no_meta)

results

results_df <- do.call(rbind, results)
results_df



unique(subset(adoption_yes_no_meta, factor == factor)$intervention_recla)

#______________________
run_models <- function(data, factor_var) {
  interventions <- unique(subset(data, factor == factor_var)$intervention_recla)
  results <- list()
  
  for (intervention_var in interventions) {
    subset_data <- subset(data, factor == factor_var & intervention_recla == intervention_var)
    model_name <- paste(factor_var, intervention_var, sep = "_")
    model_result <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                           data = subset_data,
                           method = "REML", tdist = TRUE)
    results[[model_name]] <- summary(model_result)
  }
  
  return(results)
}

# Usage
results_farm_size <- run_models(adoption_yes_no_meta, "Farm size (ha)")
results_hh_gender <- run_models(adoption_yes_no_meta, "Gender (1= Male, 0= Female)")
results_hh_age <- run_models(adoption_yes_no_meta, "hh age (years)")

# Combine results into a single dataframe
results_all <- do.call(rbind, c(results_farm_size, results_hh_gender,results_hh_age))


#_______________________





# Get unique factors and intervention_recla
unique_factors <- unique(adoption_yes_no_meta$factor)
unique_interventions <- unique(adoption_yes_no_meta$intervention_recla)
unique_factors
unique_interventions

# Fit the models

results_list <- list()

for (factor in unique_factors) {
  for (intervention in unique_interventions) {
    # Filter the data for the current factor and intervention
    filtered_data <- adoption_yes_no_meta %>% filter(factor == factor & intervention_recla == intervention)
    
    # Fit the model if there is any data for the current combination
    if (!is.na(nrow(filtered_data)) && nrow(filtered_data) > 1) {
      model <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                      data = filtered_data, method = "REML", tdist = TRUE)
      attr(model, "intervention_recla") <- intervention
      attr(model, "factor") <- factor
      results_list[[paste0(factor, "_", intervention)]] <- coef(summary(model))
    }
  }
}


articles_count <- adoption_yes_no_meta %>%
  group_by(factor, intervention_recla) %>%
  summarise(n_articles = n_distinct(id))

install.packages("tibble")
library(tibble)
results<- do.call(rbind, results_list)%>%
  rownames_to_column(., var = "row_id")
#mutate(factors= unique(gender_adoption_yes_no_meta$factor),
#      intervention_recla = unique(gender_adoption_yes_no_meta$intervention_recla))
left_join(articles_count, by = "intervention_recla")%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval<=0.1,"","")))))
mutate(intervention_recla_2 = c("Fallow", "Agroforestry","Intercropping","Crop rotation", 
                                "Integrated crop-livestock", "Mixed practices"))%>%
  mutate(label = paste(significance, " (", n_articles, ")", sep = ""))

install.packages("pals")
library(ggplot2)
library(pals)

ggplot(data=gender_results, aes(y=factors,x=estimate,xmin=ci.lb, xmax=ci.ub,
                                colour = factor(intervention_recla_2)))+
  geom_vline(xintercept=0, colour = "grey20",linetype = 3, size=0.7)+
  geom_errorbar(width=0.2,size=1, position = (position_dodge(width = -0.2)))+
  geom_point(size = 4, position = (position_dodge(width = -0.2)))+
  geom_text(aes(label=label, x=ci.ub, group=intervention_recla_2), vjust=0.3, hjust=-0.09,
            color="black", size=4, family="sans",position = (position_dodge(width = -0.2)))+
  scale_colour_brewer(palette = "Paired")+
  labs(x="PCOR",colour = "Diversified farming systems")+
  theme(axis.text.x = element_text(color="black",size=12,  family = "sans",
                                   margin = margin(t = 5, r = 0, b = 5, l = 0)),
        axis.text.y = element_text(color="black",size=12, family = "sans",face="bold",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="black",size=12, family = "sans",face="bold",
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.title = element_text(color="black",size=12, family = "sans",face="bold",
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.text = element_text(color="black",size=11, family = "sans",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.key = element_rect(fill = "white"),
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(3, 3, 3, 3),
        plot.background = element_rect(fill = "White", color = "White"),
        panel.background = element_rect(fill = "White", color = "White"),
        panel.spacing = unit(2.5, "lines"),
        axis.line = element_line(colour = "black"))

### Figure: Number of articles by country adoption yes=1, 0=no
#Study locations
#install.packages("countrycode")
library(countrycode)
library(ggplot2)
sort(unique(UN_subregion$Country.or.Area))

adoption_yes_no$country[adoption_yes_no$country %in% "Vietnam, Thailand"] <- "Vietnam"
adoption_yes_no$country[adoption_yes_no$country %in% "Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia"] <- "Ethiopia"

length(unique(adoption_yes_no$id))
unique(adoption_yes_no$intervention_recla)

country<- adoption_yes_no%>%
  select("id", "country", "intervention_recla", "x_metric_recla")%>%
  group_by(country)%>%
  mutate(n_articles = n_distinct(id))%>%
  group_by(country,n_articles)%>%
  tally()%>%
  left_join(continent_list, by = "country") 

country$continent[country$country %in% "Vietnam, Thailand"] <- "Asia"
country$continent[country$country %in% "Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia"] <- "Africa"

length(sort(unique(country$country))) #total number of countries #30
length(sort(unique(country$continent)))  #total number of continents #4
sort(unique(country$continent))
sort(unique(country$country))


world <- ggplot2::map_data("world")%>%filter(region != "Antarctica")

world_map <- ggplot2::map_data("world")%>%filter(region != "Antarctica")%>%
  left_join(country, by =  c("region" ="country"))%>%
  mutate_all(~replace(., is.na(.), 0))
mutate(region= if_else(continent=="0","",region))
#world_map$N_articles_frequency <- cut(world_map$n_articles,breaks = c(0,1,2,3,4,5,6,7))
sort(unique(world_map$N_articles_frequency))
View(world_map)


sort(unique(world_map$UN_subregion))

ggplot()+
  geom_polygon(data = world_map,mapping = aes(x = long, y = lat,group = group,fill= n_articles),
               color="grey10",size =0.05, show.legend = T)+
  coord_fixed()+
  scale_fill_gradient(low = "white", high = "turquoise4")+
  labs(fill = "Number of articles")+
  theme(legend.position = "bottom",
        legend.title =element_text(color="black",size=12, family = "sans",face="bold",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color="black", size=9, family = "sans",face="bold",hjust = 0.01,vjust = -7),
        plot.margin = margin(-7, 0, -4, 0, "cm"))+
  labs(x = NULL, y = NULL)

intervention<- adoption_yes_no %>%
  group_by(intervention_recla) %>%
  summarise(n_articles = n_distinct(id))

# Spider diagram showing the number of articles per factor, per system
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(scales)
library(ggplot2)
library(stringr)
library("grafify")

sort(unique(factors$x_metric_recla))
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "access to agricultural extension"] <- "Access to extension"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "access or use of irrigation"] <- "Access to irrigation"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "access to agricultural training"] <- "Access to training"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "administrative region assessed"] <- "Region assessed"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "farm labour force (household members)"] <- "farm labour force"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "h size"] <- "household size"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "distance from house to farm"] <- "Distancen\ farm-house"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh association member"] <- "Association member"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh education"] <- "Farmer education"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh engaged in off-farm activities"] <- "off-farm activities"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh farming experience"] <- "Farming experience"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh gender"] <- "Farmer gender"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh is native"] <- "Farmer is native"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "secured land tenure"] <- "Land tenure"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh off-farm income"] <- "off-farm income"


factors<- adoption_yes_no %>%
  mutate(x_metric_recla = if_else(is.na(x_metric_recla),"Others",x_metric_recla))%>%
  mutate(x_metric_recla= str_to_sentence(x_metric_recla))%>%
  group_by(x_metric_recla)%>%
  mutate(n_articles = n_distinct(id))%>%ungroup()%>%
  mutate(x_metric_recla = if_else(n_articles>=10,x_metric_recla,"Others"))%>%
  group_by(x_metric_recla,intervention_recla)%>%
  summarise(n_articles = n_distinct(id))%>%
  select(x_metric_recla,intervention_recla,n_articles)%>%
  filter(x_metric_recla!= "Others")%>%
  filter(intervention_recla!="contour farming")%>%
  pivot_wider(names_from = x_metric_recla, values_from = "n_articles")%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
  add_row(intervention_recla = "articles_total")%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 32))%>%
  mutate(intervention_recla= c("Agroforestry", "Crop rotation", "Intercropping",
                               "Integrated crop-livestock", "Mixed practices",
                               "Fallow", "Pull-push", "Embedded semi-natural","articles_total"))


p_data <- factors %>% rename(group = "intervention_recla")

circle_coords <- function(r, n_axis = ncol(p_data) - 1){
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  x <- r*cos(fi)
  y <- r*sin(fi)
  
  tibble(x, y, r)
}
central_distance <- 0.15

step_1 <- map_df(seq(0, 1, 0.25) + central_distance, circle_coords) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data = circle_coords(1 + central_distance), 
               alpha = 1, fill = "gray97") +
  geom_path(aes(group = r), lty = 2, alpha = 0.5) +
  theme_void()+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
step_1

axis_coords <- function(n_axis){
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
  x1 <- central_distance*cos(fi)
  y1 <- central_distance*sin(fi)
  x2 <- (1 + central_distance)*cos(fi)
  y2 <- (1 + central_distance)*sin(fi)
  
  tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
}

step_2 <- step_1 + geom_line(data = axis_coords(ncol(p_data) - 1), 
                             aes(x, y, group = id), alpha = 0.3)
step_2

text_data <- p_data %>%
  select(-group) %>%
  map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
  mutate(r = seq(0, 1, 0.25)) %>%
  pivot_longer(-r, names_to = "parameter", values_to = "value")

text_coords <- function(r, n_axis = ncol(p_data) - 1){
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
  x <- r*cos(fi)
  y <- r*sin(fi)
  
  tibble(x, y, r = r - central_distance)
}

labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
  bind_cols(text_data %>% select(-r))%>%
  filter(value<=45)%>%
  mutate(value= if_else(parameter == "Access to credit", value, NA))

step_3 <- step_2 + 
  geom_text(data = labels_data, aes(x, y, label = value), alpha = 0.65,fontface = "bold") +
  geom_text(data = text_coords(1 + central_distance + 0.17), 
            aes(x, y), label = labels_data$parameter[1:(ncol(p_data)-1)],fontface = "bold")

step_3

rescaled_coords <- function(r, n_axis){
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  tibble(r, fi) %>% mutate(x = r*cos(fi), y = r*sin(fi)) %>% select(-fi)
}

rescaled_data <- p_data %>% 
  mutate(across(-group, rescale))%>%
  mutate(copy = pull(., 2)) %>% 
  pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
  group_by(group) %>%
  mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
  unnest%>%
  filter(group!="articles_total")

typeof(rescaled_data)

step_4<-step_3 + 
  geom_point(data = rescaled_data, 
             aes(x, y, group = group, col = group), 
             size = 3) +
  geom_path(data = rescaled_data, 
            aes(x, y, group = group, col = group), 
            size = 1)+
  scale_colour_grafify()+
  labs(col = "Diversified farming systems")+
  theme(legend.position = "none")
legend.title =element_text(color="black",size=12, family = "sans",face="bold",
                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
plot.margin = margin(1,1,1.5,1.2, "cm"))
step_4

























