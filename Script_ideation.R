save.image ("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Main data/R/Nsg_St.RData")

options (scipen=999)

#Packages
library (tidyverse)
library(freqtables)

#Data management 
#The first item (Suicide_1) is suicidal ideation
#The second item (Suicide_2) is the suicidal attempt 

#Descriptive analysis

Nsg_St %>% 
  freq_table(Age_cat) 
Nsg_St %>% 
  freq_table(Sex.fct) 
Nsg_St %>% 
  freq_table(Father_education.fct_new) 
Nsg_St %>% 
  freq_table(Mother_education.fct_new) 
Nsg_St %>% 
  freq_table(Family_income.fct_new) 
Nsg_St %>% 
  freq_table(Division.fct_new) 
Nsg_St %>% 
  freq_table(Division.fct) 
Nsg_St %>% 
  freq_table(Family_type.fct) 

Nsg_St %>% 
  freq_table(Course_type.fct_new) 
Nsg_St %>% 
  freq_table(Institution_type.fct) 
Nsg_St %>% 
  freq_table(Academic_year.fct)
Nsg_St %>% 
  freq_table(Nursing_choice.fct) 
Nsg_St %>% 
  freq_table(Have_qualified_teacher.fct) 
Nsg_St %>% 
  freq_table(Have_qualified_language_teacher.fct) 
Nsg_St %>% 
  freq_table(Have_qualified_clinical.teacher.fct) 
Nsg_St %>% 
  freq_table(Have_subject_specific_lab) 

### Distribution of mental health wellbeing, burnout, and suicidal ideation

confint(Nsg_St$WEMWBS_Score,  pnorm, level = 0.95)

require(manipulate)
library (manipulate)

t.test(Nsg_St$WEMWBS_Score,
       conf.level=0.95)
sd (Nsg_St$WEMWBS_Score)
mean (Nsg_St$WEMWBS_Score)


Nsg_St %>% 
  freq_table(BMS_cat)
Nsg_St %>% 
  freq_table(Suicide_1)

############# un-adjusted analysis by chi squire test/ t-test #############

chisq.test(Nsg_St$BMS_cat, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(BMS_cat,Nsg_St$Suicide_1) 

t.test (Nsg_St$WEMWBS_Score ~ Nsg_St$Suicide_1, correction =T)

aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Suicide_1), FUN=sd)

#Socio-demographic information

chisq.test(Nsg_St$Age_cat, Nsg_St$Suicide_1,  correct=T)#non-sig
Nsg_St %>% 
  freq_table(Age_cat,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Sex.fct, Nsg_St$Suicide_1,  correct=T) #non-sig
Nsg_St %>% 
  freq_table(Sex.fct,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Father_education.fct_new, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Father_education.fct_new,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Mother_education.fct_new, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Mother_education.fct_new,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Family_income.fct_new, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Family_income.fct_new,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Division.fct_new, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Division.fct_new,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Current_residence.fct_new, Nsg_St$Suicide_1,  correct=T) #non-sig
Nsg_St %>% 
  freq_table(Current_residence.fct_new,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Family_type.fct, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Family_type.fct,Nsg_St$Suicide_1) 

#Academic information
chisq.test(Nsg_St$Course_type.fct_new, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Course_type.fct_new,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Institution_type.fct, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Institution_type.fct,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Academic_year.fct, Nsg_St$Suicide_1,  correct=T) #non-sig
Nsg_St %>% 
  freq_table(Academic_year.fct,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Nursing_choice.fct, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Nursing_choice.fct,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Have_qualified_teacher.fct, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Have_qualified_teacher.fct,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Have_qualified_language_teacher.fct, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Have_qualified_language_teacher.fct,Nsg_St$Suicide_1) 

chisq.test(Nsg_St$Have_qualified_clinical.teacher.fct, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Have_qualified_clinical.teacher.fct,Nsg_St$Suicide_1)

chisq.test(Nsg_St$Have_subject_specific_lab, Nsg_St$Suicide_1,  correct=T)
Nsg_St %>% 
  freq_table(Have_subject_specific_lab,Nsg_St$Suicide_1)

#re-code Suicide_1
Nsg_St$Suicide_1_new= recode (Nsg_St$Suicide_1,
                        "Yes"="1",
                        "No"= "0")
Nsg_St$Suicide_1_new=as.factor(Nsg_St$Suicide_1_new)

#Adjusted association
#Model 1
ideation_glm.1=glm (data= Nsg_St, Suicide_1_new~Age_cat+
                   Sex.fct+
                   Father_education.fct_new+
                   Mother_education.fct_new+
                   Family_income.fct_new+
                   Division.fct_new+
                  Family_type.fct
                 ,family= binomial)

summary (ideation_glm.1)

or_ideation_glm.1=exp(cbind(coef(ideation_glm.1), confint(ideation_glm.1, level=0.95)))
or_ideation_glm.1
#
Nsg_St$Have_qualified_clinical.teacher.fct <- relevel(Nsg_St$Have_qualified_clinical.teacher.fct, ref = "Yes") 

Nsg_St$Have_subject_specific_lab <- relevel(Nsg_St$Have_subject_specific_lab, ref = "Yes") 
Nsg_St$Have_subject_specific_lab= as.factor (Nsg_St$Have_subject_specific_lab)

#Model 2
ideation_glm.2=glm (data= Nsg_St, Suicide_1_new~Age_cat+
                   Sex.fct+
                   Father_education.fct_new+
                   Mother_education.fct_new+
                   Family_income.fct_new+
                   Division.fct_new+
                     Family_type.fct+
                     
                   Course_type.fct_new+
                   Institution_type.fct+
                   Nursing_choice.fct+
                   Have_qualified_teacher.fct+
                   Have_qualified_language_teacher.fct+
                   Have_qualified_clinical.teacher.fct+
                     Have_subject_specific_lab
                 ,family= binomial)

summary (ideation_glm.2)
ideation_glm.2

or_ideation_glm.2=exp(cbind(coef(ideation_glm.2), confint(ideation_glm.2, level=0.95)))
or_ideation_glm.2

#Model 3
ideation_glm.3=glm (data= Nsg_St, Suicide_1_new~Age_cat+
                      Sex.fct+
                      Father_education.fct_new+
                      Mother_education.fct_new+
                      Family_income.fct_new+
                      Division.fct_new+
                      Family_type.fct+
                      
                      Course_type.fct_new+
                      Institution_type.fct+
                      Nursing_choice.fct+
                      Have_qualified_teacher.fct+
                      Have_qualified_language_teacher.fct+
                      Have_qualified_clinical.teacher.fct+
                      Have_subject_specific_lab+
                      
                      BMS_cat+
                      WEMWBS_Score
                    ,family= binomial)

summary (ideation_glm.3)
ideation_glm.3
or_ideation_glm.3=exp(cbind(coef(ideation_glm.3), confint(ideation_glm.3, level=0.95)))
or_ideation_glm.3

#
library (tidyverse)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

Forest_ideation = 
  plot_model(
    ideation_glm.3,
    vline.color = "#808000",
    show.values = F,
    width = 0.1,
    value.offset = 0.4,
    cex= 0.1,
    p.shape = TRUE,
    xmin=error_lower,
    xmax=error_upper,
    axis.labels = c("Burnout = Yes",
                    "Mental health well-being",
                    "Have subject specific labs",
                    "Have qualified clinical teachers = No",
                    "Have qualified English language teachers = No",
                    "Have qualified teachers = No",
                    "Own choice of being nurse = No",
                    "Type of institution = Private",
                    "Type of course = Diploma",
                    "Family type = Nuclear",
                    "Division = Sylhet",
                    "Division = Dhaka",
                    "Family income = >20k BDT",
                    "Family income = 15k - 20k BDT",
                    "Mother's education = Non-graduate",
                    "Father's education = Non-graduate",
                    "Sex = Male",
                    "Age = >20 years"),
                            title = "")+
  
  theme_minimal()

Forest_ideation

#Figures (not perfect)

Figure.df.ideation= data.frame (WEMWBS=Nsg_St$WEMWBS_Score)
Figure.df.ideation$BMS_cat=Nsg_St$BMS_cat
Figure.df.ideation$Suicide_1_new=Nsg_St$Suicide_1_new

ggplot (Figure.df.ideation, aes(x=Suicide_1_new , y= WEMWBS))+
  geom_jitter (color = "violet"
  )  +
  labs (x= "WEMWBS Score", y= "Suicidal ideation")+
  geom_smooth (method="lm", se=T)+
  theme_bw()

##
table (Figure.df.ideation$Suicide_1_new)
Figure.df.ideation$Suicide_1_new= recode (Figure.df.ideation$Suicide_1_new,
                              "1"="Yes",
                              "0"= "No")
Figure.df.ideation$Suicide_1_new=as.factor(Figure.df.ideation$Suicide_1_new)





