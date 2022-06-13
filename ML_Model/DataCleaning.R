library(tidyverse)
library(ggplot2)
library(lubridate)


nus_survey<-read.csv("Datasets/NUS_survey.csv")

str(nus_survey)

colnames(nus_survey)<-c("timestamps", "age", "year", "gender", "country", 
                              "univ", "major", "financial_stable", "sleep", 
                              "exercise", "meals", "loss_of_int", "concentrate", 
                              "family_history", "seek_treatment", "interference_work")

## Filter for singapore & Change column values to numerical (to help find trends and train models)
#find unique values of each variable to assign a number
unique(nus_survey$univ)
unique(nus_survey$sleep)
unique(nus_survey$exercise)
unique(nus_survey$meals)
unique(nus_survey$family_history)


nus_survey_num_sg<- nus_survey%>%
  filter(!gender=="Prefer not to say")%>%
  filter(country == "Singapore")%>%
  select(-country)%>%
  mutate(gender = case_when(gender == "Male" ~ 0, gender == "Female" ~ 1), 
         univ = as.factor(univ), 
         major = as.factor(major), 
         sleep = case_when(sleep=="0-3hrs"~0, sleep=="3-6hrs"~1, sleep=="6-8hrs"~2, sleep==">8hrs"~3), 
         exercise = case_when(exercise == "Never"~0,exercise == "Rarely"~1, exercise == "Sometimes"~2,
                              exercise =="Often"~3, exercise =="Always"~4), 
         meals = case_when(meals == "Never"~0,meals== "Rarely"~1, meals == "Sometimes"~2,
                           meals =="Often"~3, meals =="Always"~4),
         loss_of_int = case_when(loss_of_int == "Never"~0, loss_of_int== "Rarely"~1, 
                                 loss_of_int == "Sometimes"~2, loss_of_int=="Often"~3, 
                                 loss_of_int =="Always"~4), 
         concentrate = case_when(concentrate == "Never"~0,concentrate== "Rarely"~1, 
                                 concentrate == "Sometimes"~2, concentrate =="Often"~3, 
                                 concentrate =="Always"~4), 
         family_history = case_when(family_history=="No"~0, family_history=="Yes"~1), 
         seek_treatment = case_when(seek_treatment =="No"~0, seek_treatment == "Yes" ~ 1), 
         interference_work_num = case_when(interference_work == "N/A (Not Applicable)" ~ 0,
                                       interference_work == "Never"~ 1,interference_work== "Rarely"~2, 
                                      interference_work == "Sometimes"~ 3, interference_work=="Often"~4, 
                                      interference_work =="Always"~5),
         interference_work = case_when(interference_work == "Never"~ 1,interference_work== "Rarely"~2, 
                                    interference_work == "Sometimes"~ 3, interference_work=="Often"~4, 
                                    interference_work =="Always"~5))%>%
  mutate(interference_work_at_all = if_else(interference_work_num==0,0,1))%>%
  mutate(y_train = if_else(interference_work_at_all==1|seek_treatment==1, 1, 0))%>% #creating our y_train
  select(-timestamps)

### More SG data
nus_survey2<-read.csv("NUS_survey2.csv")

str(nus_survey2)

colnames(nus_survey2)<-c("timestamps", "age", "year", "gender", "country", 
                        "univ", "major", "financial_stable", "sleep", 
                        "exercise", "meals", "loss_of_int", "concentrate", 
                        "family_history", "seek_treatment", "interference_work")

## Filter for singapore & Change column values to numerical (to help find trends and train models)
#find unique values of each variable to assign a number
unique(nus_survey2$univ)
unique(nus_survey2$sleep)
unique(nus_survey2$exercise)
unique(nus_survey2$meals)
unique(nus_survey2$family_history)


nus_survey_num_sg2<- nus_survey2%>%
  filter(!gender=="Prefer not to say")%>%
  filter(country == "Singapore")%>%
  select(-country)%>%
  mutate(gender = case_when(gender == "Male" ~ 0, gender == "Female" ~ 1), 
         univ = as.factor(univ), 
         major = as.factor(major), 
         sleep = case_when(sleep=="0-3hrs"~0, sleep=="3-6hrs"~1, sleep=="6-8hrs"~2, sleep==">8hrs"~3), 
         exercise = case_when(exercise == "Never"~0,exercise == "Rarely"~1, exercise == "Sometimes"~2,
                              exercise =="Often"~3, exercise =="Always"~4), 
         meals = case_when(meals == "Never"~0,meals== "Rarely"~1, meals == "Sometimes"~2,
                           meals =="Often"~3, meals =="Always"~4),
         loss_of_int = case_when(loss_of_int == "Never"~0, loss_of_int== "Rarely"~1, 
                                 loss_of_int == "Sometimes"~2, loss_of_int=="Often"~3, 
                                 loss_of_int =="Always"~4), 
         concentrate = case_when(concentrate == "Never"~0,concentrate== "Rarely"~1, 
                                 concentrate == "Sometimes"~2, concentrate =="Often"~3, 
                                 concentrate =="Always"~4), 
         family_history = case_when(family_history=="No"~0, family_history=="Yes"~1), 
         seek_treatment = case_when(seek_treatment =="No"~0, seek_treatment == "Yes" ~ 1), 
         interference_work_num = case_when(interference_work == "N/A (Not Applicable)" ~ 0,
                                           interference_work == "Never"~ 1,interference_work== "Rarely"~2, 
                                           interference_work == "Sometimes"~ 3, interference_work=="Often"~4, 
                                           interference_work =="Always"~5),
         interference_work = case_when(interference_work == "Never"~ 1,interference_work== "Rarely"~2, 
                                       interference_work == "Sometimes"~ 3, interference_work=="Often"~4, 
                                       interference_work =="Always"~5))%>%
  mutate(interference_work_at_all = if_else(interference_work_num==0,0,1))%>%
  mutate(y_train = if_else(interference_work_at_all==1|seek_treatment==1, 1, 0))%>% #creating our y_train
  select(-timestamps)




### Exploring relationships
#Correlation matrix (only for numerical values)
nus_survey_num_sg%>%
  select(-c(univ, major))%>%
  cor()%>%
  view()

library(reshape2)
nus_survey_num_sg%>%
  select(c(age, gender, family_history, seek_treatment, interference_work_num, interference_work_at_all, y_train))%>%
  cor()%>%
  melt()%>%
  ggplot(aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+
  scale_fill_gradient2()

##Exporting
nus_survey_ml2<-nus_survey_num_sg2%>%
  select(-c(univ, interference_work))

write.csv(nus_survey_ml2, file = "Datasets/SG_data.csv")