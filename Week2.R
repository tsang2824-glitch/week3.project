###HELLO KAREN###
# Week 2: Summaries and Statistical Tests
viz_data = readr::read_csv("data/viz_data.csv")
head(viz_data)
mean(viz_data$weight)
median(viz_data$weight)
min(viz_data$bmi)
max(viz_data$bmi)
quantile(viz_data$bmi)
sd(viz_data$bmi)
cov(viz_data$height, viz_data$weight)
cor(viz_data$height, viz_data$weight)
group_outcome_table = table(viz_data$group, viz_data$outcome)
group_outcome_table
round(prop.table(group_outcome_table, margin = 1) * 100, 1)
chisq.test(group_outcome_table)
fisher.test(group_outcome_table)
table(viz_data$site)
#??"standard"
#apropos("test")
table(viz_data$site)
mean(viz_data$bmi)
max(viz_data$response)
mean(viz_data$response)
table(viz_data$site, viz_data$group)
ttest_hc=t.test(response ~ group, data = viz_data, subset = group %in% c("High dose", "Control"))
wilcox_hc = wilcox.test(response ~ group, data = viz_data,
                        subset = group %in% c("High dose", "Control"))
wilcox_hc
wilcox.test(response ~ group, data = viz_data,
            subset = group %in% c("High dose", "Control"))
wilcox_hc
group_aov = aov(response ~ group, data = viz_data)
summary(group_aov)
group_aov = aov(response ~ group, data = viz_data)
summary(group_aov)
response_ttest = t.test(response ~ group, data = viz_data,
                        subset = group %in% c("High dose", "Low dose"))
str(response_ttest)           # peek inside: p.value, conf.int, estimate, ...
response_ttest$p.value        # 1.18611e-07
response_ttest$conf.int       # 5.18 to 10.96
ttest_hc
ttest_hc$p.value
summary(group_aov)$p.value
summary(group_aov)$p.value
aov_summary = summary(group_aov)
str(aov_summary)
aov_summary[[1]]$`Pr(>F)`[1]
quantile1=quantile(viz_data$bmi, probs=c(0.1,0.9))
quantile1
str(quantile1)
quantile1[["90%"]] - quantile1[["10%"]]
quantile2=quantile(viz_data$bmi, probs=c(0.25,0.5,0.75))
quantile2
str(quantile2)
quantile2[["75%"]] - quantile2[["25%"]]
IQR(viz_data$bmi)
ttest_hc
str(response_ttest)
response_ttest$alternative
t.test(response ~ group, data = viz_data,
       subset = group %in% c("High dose", "Low dose"),
       alternative = "greater")
list.files("data")
viz_data = readr::read_csv("data/viz_data.csv")
followup_data = readr::read_csv("data/followup_data.csv")
head(followup_data)
dim(followup_data)
followup_data = merge(viz_data, followup_data, by = 'id')
dim(followup_data)
names(followup_data)
head(followup_data)
bmilm=lm(bmi ~ group, data = followup_data)
summary(bmilm)
heightlm=lm(height ~ group, data = followup_data)
summary(heightlm)
bmiaov=aov(bmi ~ group, data = followup_data)
summary(bmiaov)
weight_lm = lm(response ~ group, data = followup_data)
summary(weight_lm)
lm_fit = lm(response ~ bmi + group, data = viz_data)
summary(lm_fit)
viz_data$improved = as.integer(viz_data$outcome == "Improved")
logistic_fit = glm(improved ~ bmi + group, data = viz_data, family = binomial)
summary(logistic_fit)
exp(coef(logistic_fit))
negbin_fit = MASS::glm.nb(clinic_visits ~ group, data = followup_data)
summary(negbin_fit)
exp(coef(negbin_fit))
exp(confint(negbin_fit))
exp(confint(logistic_fit))
library(survival)
followup_surv = Surv(followup_data$survival_months, followup_data$event)
head(followup_surv)
# [1]  9.6  4.1  4.6 24.0+ 24.0+ 24.0+
cox_fit = coxph(Surv(survival_months, event) ~ group, data = followup_data)
summary(cox_fit)
coef_table = summary(lm_fit)$coefficients
coef_table["groupHigh dose", "Pr(>|t|)"]    # 1.288714e-18
coef(lm_fit)
confint(lm_fit)
summary(lm_fit)$coefficients
coef_table = summary(lm_fit)$coefficients
coef_table
coef_table["groupLow dose", "Pr(>|t|)"]
coef_table["groupHigh dose", "Pr(>|t|)"]
lm_fit
# which = 1 selects just the first of four default diagnostic plots
# (residuals vs. fitted values), rather than showing all four at once
plot(lm_fit, which = 1)

# logistic regression also produces residuals vs. fitted diagnostics,
# even though the underlying model and residual type are different
plot(logistic_fit, which = 1)
plot(negbin_fit, which = 1)
cox_survfit = survfit(cox_fit, newdata = data.frame(group = unique(followup_data$group)))
cox_survfit = survfit(cox_fit, newdata = data.frame(group = unique(followup_data$group)))
plot(cox_survfit,
     xlab = "Time (months)",
     ylab = "Estimated survival probability")
library(survival)
cox_fit = coxph(Surv(survival_months, event) ~ group, data = followup_data)
cox_survfit = survfit(cox_fit, newdata = data.frame(group = unique(followup_data$group)))
class(cox_survfit)
plot(cox_survfit,
     xlab = "Time (months)",
     ylab = "Estimated survival probability")
list.files("data")
library(tidyverse)
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2)
head(clinic_raw)
str(clinic_raw)
unique(clinic_raw$weight_kg)
as.numeric("-")
sum(clinic_raw$weight_kg == "-", na.rm = TRUE)
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2, na = c("", "NA", "-"))
class(clinic_raw$weight_kg)
mean(clinic_raw$weight_kg, na.rm = TRUE)
mean(clinic_raw$weight_kg)     
str(clinic_raw)
table(clinic_raw$site)
table(clinic_raw$sex)
table(clinic_raw$notes)
table(clinic_raw$sex, useNA = "ifany")
sum(clinic_raw$blood_pressure == "N/A", na.rm = TRUE)
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2, na = c("", "NA", "-", "N/A"))
sum(clinic_raw$blood_pressure == "N/A", na.rm = TRUE)   # expect 0
sum(is.na(clinic_raw$blood_pressure))                    # expect at least 16
sum(is.na(clinic_raw$age))         # count missing values: 6
range(clinic_raw$age, na.rm = TRUE) # smallest and largest: 18 90
summary(clinic_raw)                 # overview of every column
library(ggplot2)
ggplot(clinic_raw, aes(x = age)) +
  geom_histogram(binwidth = 5, na.rm = TRUE)
ggplot(clinic_raw, aes(x = weight_kg)) +
  geom_histogram(binwidth = 5, na.rm = TRUE)
sum(is.na(clinic_raw$weight_kg))
table(str_to_upper(clinic_raw$sex), useNA = "ifany")
clinic_raw$sex = str_to_upper(clinic_raw$sex)
table(str_to_lower(clinic_raw$notes), useNA = "ifany")
clinic_clean = clinic_raw %>%
  rename(id = patient_id)
names(clinic_clean)
clinic_clean = clinic_clean %>%
  filter(age >= 18 | is.na(age))
dim(clinic_clean)
clinic_clean %>% arrange(age) %>% head()
clinic_clean %>% arrange(desc(age)) %>% head()
clinic_clean = clinic_clean %>%
  mutate(site = tolower(trimws(site)),
         site = gsub("_", " ", site))
table(clinic_clean$site)
table(clinic_clean$notes)
clinic_clean = clinic_clean %>%
  mutate(notes = tolower(trimws(notes)),
         notes = gsub("_", " ", notes))
table(clinic_clean$notes, useNA = "ifany")
library(stringr)
clinic_clean = clinic_clean %>%
  mutate(bp_upper = as.numeric(str_extract(blood_pressure, "^[0-9]+")),
         bp_lower = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+")))
head(clinic_clean[, c("blood_pressure", "bp_upper", "bp_lower")])
sum(is.na(clinic_clean$blood_pressure))   # before: 16
sum(is.na(clinic_clean$bp_upper))         # after: expect 16
sum(is.na(clinic_clean$bp_lower))         # after: expect 16
range(clinic_clean$bp_upper, na.rm = TRUE)
range(clinic_clean$bp_lower, na.rm = TRUE)
clinic_clean %>%
  filter(bp_upper <= bp_lower) %>%
  select(id, blood_pressure, bp_upper, bp_lower)
clinic_clean = clinic_clean %>%
  mutate(visit_date = as.Date(visit_date, format = "%d/%m/%Y"))
class(clinic_clean$visit_date)          # expect "Date"
sum(is.na(clinic_clean$visit_date))     # expect 0
range(clinic_clean$visit_date)   # should all be in 2023
clinic_clean = clinic_clean %>%
  mutate(bp_suspect = bp_upper <= bp_lower)
table(clinic_clean$bp_suspect, useNA = "ifany")
clinic_clean %>%
  group_by(site) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), n = n())

write_csv(clinic_clean, "data/clinic_clean.csv")
list.files("data")

ls()
exists("group_avo")
