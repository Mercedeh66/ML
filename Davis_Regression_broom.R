#Davis's Code
library(broom)
library(lubridate)
library(tidyverse)
library(gtsummary)
library(broom)
library(visdat)
library(rstatix)
library(reshape2)
library(pheatmap)
library(flextable)
library(officer)
library(dplyr)
library(GGally)
library(rio)
library(janitor)

the_data <- read.csv("~/Desktop/clean_data_Davis.csv", 
                     header=TRUE, 
                     sep=",", 
                     stringsAsFactors = TRUE)

str(the_data)
summary(the_data)
names(the_data)

the_data <- the_data %>%
  mutate(sx_outcome= recode(sx_outcome, "1"="Failure", "0"="Success" ))

ncol(the_data)
nrow(the_data)

table(the_data$sex)
table(the_data$plac_b)


#univariate plots
hist(the_data$child_weight, main = "Child's weight", xlab = "weight", ylab = "count", col="red")
picking<-the_data[, 5:19]
boxplot(picking)


##bivariate plots showing distribution
plot(the_data$region, the_data$csf_vol)
plot(the_data$time_admission, the_data$csf_vol)

ggplot(the_data, aes(brain_vol, child_weight)) + geom_point(aes(color=sx_outcome, size=time_admission))


the_data1 <- subset(the_data, select= -c(snum, place_of_delivery, time_infection))
##You need rownames here
kk<-the_data$snum
ncol(the_data1)
rownames(the_data1)<-kk
###Note if you are set on using this on everything you just need to change everything to 0 and 1 for categorical
##selecting only numeric variables
data_num <- select_if(the_data1, is.numeric)

##You need to make sure you do this
i <- sapply(the_data1, is.factor)
the_data1[i] <- lapply(the_data1[i], as.character)

ia <- sapply(the_data1, is.integer)
the_data1[ia] <- lapply(the_data1[ia], as.numeric)

##correction matrix for all the numeric variables
#Weid odd function is not automatically detecting
#method1
cor.mat <- data_num %>% cor_mat()
cor.mat
cor.mat %>% cor_get_pval() #getting significant levels

cor.mat %>%
  pull_lower_triangle() %>%
  cor_plot(label = FALSE)



#method2
cor.pmat <- cor_pmat(the_data1)
cor.pmat

cor.test(the_data1$csf_vol, the_data1$time_headincrease)

##LOGISTIC REGRESSION
names(the_data1)

##Here you are correcting for all these things you should not do that you wanna do one by one. Also this is
#not logistic regression needs logit link this linear regression with binomial assumption
#on burnulli distribution also you are correcting for things which is not sth you should do we will talk more
#I need to teach you about regression and distributions
model_outcome <- the_data1 %>% glm(sx_outcome ~ hydro_etiology + sex + gest + plac_b + time_headincrease +
                                     time_admission + head_circum + wbc + child_weight + season_birth + 
                                     csection + csf_vol + brain_vol + region, family = binomial, data=.)
summary(model_outcome)

model <- tbl_regression(model_outcome, exponentiate = TRUE)
model

levels(the_data1$sx_outcome)

#outcome variable
#No logistic if you wanna run glm you need to change both to zero and 1 like this will never work as a binomal
#you need to use linear regression here
the_data2<-the_data1 %>%
  mutate(cat_sx_outcome = case_when(sx_outcome == c("Failure") ~ 0,
                                    sx_outcome == c("Success") ~ 1))
  
    ###hydro_etiology is a nominal variable so it is a good candidate for kruskal valis test

#You gotta choose your tests well :)

#⁡ ( p / ( 1 − p ) ) \log(p / (1-p)) log(p/(1−p)) 
model_outcome1 <- the_data2 %>% glm(cat_sx_outcome ~ hydro_etiology,family=binomial(), data=.) 
summary(model_outcome1)
summary(lm(formula = rank(cat_sx_outcome) ~ hydro_etiology, data = the_data2))
kruskal.test(cat_sx_outcome~hydro_etiology, data=the_data2)


model_outcome2 <- the_data1 %>% glm(sx_outcome ~ sex , family = binomial, data=.)
model_outcome3 <- the_data1 %>% glm(sx_outcome ~ gest, family = binomial, data=.)
model_outcome4 <- the_data1 %>% glm(sx_outcome ~ plac_b, family = binomial, data=.)
model_outcome5 <- the_data1 %>% glm(sx_outcome ~ time_headincrease, family = binomial, data=.)
model_outcome6 <- the_data1 %>% glm(sx_outcome ~ time_admission, family = binomial, data=.)
model_outcome7 <- the_data1 %>% glm(sx_outcome ~ head_circum, family = binomial, data=.)
model_outcome8 <- the_data1 %>% glm(sx_outcome ~ wbc, family = binomial, data=.)
model_outcome9 <- the_data1 %>% glm(sx_outcome ~ child_weight, family = binomial, data=.)
model_outcome10 <- the_data1 %>% glm(sx_outcome ~ season_birth, family = binomial, data=.)
model_outcome11 <- the_data1 %>% glm(sx_outcome ~ csection, family = binomial, data=.)
model_outcome12 <- the_data1 %>% glm(sx_outcome ~ csf_vol, family = binomial, data=.)
model_outcome13 <- the_data1 %>% glm(sx_outcome ~ region, family = binomial, data=.)
model_outcome14 <- the_data1 %>% glm(sx_outcome ~ brain_vol, family = binomial, data=.)

summary(model_outcome1)
summary(model_outcome2)
summary(model_outcome3)

#checking missing values
visdat::vis_miss(the_data1) #i.e no missingness observed

#categorical variables
###fisher.test(table(the_data1$sx_outcome, the_data1$sex))
fisher.test(xtabs(~sx_outcome + sex, data=the_data1))
fisher.test(xtabs(~sx_outcome + csection, data=the_data1))
fisher.test(xtabs(~sx_outcome + season_birth, data=the_data1))

fisher.test(xtabs(~sex + csection, data=the_data1))
fisher.test(xtabs(~sex + season_birth, data=the_data1))

fisher.test(xtabs(~csection + season_birth, data=the_data1))

#wilcoxon test 
wilcox.test(child_weight~sx_outcome, data=the_data1, exact=FALSE)
wilcox.test(child_weight~sex, data=the_data1, exact=FALSE)
wilcox.test(child_weight~season_birth, data=the_data1, exact=FALSE)
wilcox.test(child_weight~csection, data=the_data1, exact=FALSE)

wilcox.test(brain_vol~sx_outcome, data=the_data1, exact=FALSE)
wilcox.test(brain_vol~sex, data=the_data1, exact=FALSE)
wilcox.test(brain_vol~season_birth, data=the_data1, exact=FALSE)
wilcox.test(brain_vol~csection, data=the_data1, exact=FALSE)

wilcox.test(csf_vol~sx_outcome, data=the_data1)
wilcox.test(csf_vol~sex, data=the_data1)
wilcox.test(csf_vol~season_birth, data=the_data1)
wilcox.test(csf_vol~csection, data=the_data1)

wilcox.test(wbc~sx_outcome, data=the_data1)
wilcox.test(wbc~sex, data=the_data1)
wilcox.test(wbc~season_birth, data=the_data1)
wilcox.test(wbc~csection, data=the_data1)

wilcox.test(head_circum~sx_outcome, data=the_data1)
wilcox.test(head_circum~sex, data=the_data1)
wilcox.test(head_circum~season_birth, data=the_data1)
wilcox.test(head_circum~csection, data=the_data1)


#kruskal test for morethan two groups
kruskal.test(child_weight~gest, data=the_data1)
kruskal.test(child_weight~region, data=the_data1)
kruskal.test(child_weight~plac_b, data=the_data1)
kruskal.test(child_weight~hydro_etiology, data=the_data1)

kruskal.test(brain_vol~gest, data=the_data1)
kruskal.test(brain_vol~region, data=the_data1)
kruskal.test(brain_vol~plac_b, data=the_data1)
kruskal.test(brain_vol~hydro_etiology, data=the_data1)

kruskal.test(csf_vol~gest, data=the_data1)
kruskal.test(csf_vol~region, data=the_data1)
kruskal.test(csf_vol~plac_b, data=the_data1)
kruskal.test(csf_vol~hydro_etiology, data=the_data1)

kruskal.test(wbc~gest, data=the_data1)
kruskal.test(wbc~region, data=the_data1)
kruskal.test(wbc~plac_b, data=the_data1)
kruskal.test(wbc~hydro_etiology, data=the_data1)

kruskal.test(head_circum~gest, data=the_data1)
kruskal.test(head_circum~region, data=the_data1)
kruskal.test(head_circum~plac_b, data=the_data1)
kruskal.test(head_circum~hydro_etiology, data=the_data1)

names(the_data1)

