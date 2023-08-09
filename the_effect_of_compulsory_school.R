## UPLOADING LIBRARIES
library(tidyverse)
library(educationdata)
library(haven)
library(estimatr)
library(fixest)
library(modelsummary)
library(RColorBrewer)

# DOWNLOAD DATA
grad <- get_education_data(level = 'schools', 
                                 source = 'edfacts', 
                                 topic = 'grad-rates', 
                                 filters = list(year = 2011:2018),
                                 add_labels = TRUE)

write_rds(grad, "grad.rds")
grad <- read_rds("grad.rds")

# filtering data
data_cleaned2 <- grad %>% 
  filter(grad_rate_low >= 0 & grad_rate_midpt >= 0 & grad_rate_high >= 0) %>% 
  filter(fips %in% c("Michigan","New York", "Vermont", "Arizona"))


# summary statistics
statistics <- data_cleaned2 %>% 
  filter(grad_rate_low >= 0 & grad_rate_midpt >= 0 & grad_rate_high >= 0) %>% 
  mutate(group = ifelse(fips == "Michigan", "Treated", "Controls"),
         period = ifelse(year < 2015, "pre-treatment", "post-treatment")) %>% 
  group_by(group) %>% 
  summarise(min = min(grad_rate_midpt),
            mean = mean(grad_rate_midpt), 
            median = median(grad_rate_midpt),
            sd = sd(grad_rate_midpt),
            max = max(grad_rate_midpt))
statistics

# data visualization
data_cleaned2 %>% 
  mutate(group = ifelse(fips == "Michigan", "Treated", "Controls")) %>% 
  group_by(group, year) %>% 
  summarise(mean_m = log(mean(grad_rate_midpt)),
            mean_l = log(mean(grad_rate_low)),
            mean_h = log(mean(grad_rate_high)),
            num = mean(cohort_num)) %>% 
  ggplot() +
  geom_line(aes(year, num, group=group))
  

data_cleaned2 %>% 
  mutate(group = ifelse(fips == "Michigan", "Treated", "Controls")) %>% 
  group_by(group, year) %>% 
  summarise(m = mean(cohort_num)) %>% 
  ggplot() +
  geom_boxplot(aes(group, m))


data_cleaned2 %>% 
  mutate(group = ifelse(fips == "Michigan", "Treated", "Controls")) %>% 
  group_by(group) %>% 
  ggplot() +
  geom_histogram(aes(x=grad_rate_midpt)) +
  facet_wrap(~group) +
  theme_classic()


data_cleaned2 %>% 
  mutate(group = ifelse(fips == "Michigan", "Treated", "Controls")) %>% 
  ggplot() +
  geom_density(aes(grad_rate_midpt, fill=group), alpha=0.7) + 
  theme_classic()


data_cleaned2 %>% 
  filter(race != "Total" & disability == "Total" & homeless == "Total" & 
           foster_care == "Total" & lep == "All students"
         & econ_disadvantaged == "Total") %>% 
  mutate(group = ifelse(fips == "Michigan", "treated", "control")) %>% 
  group_by(group, year) %>%
  summarise(grad_rates = mean(grad_rate_midpt, na.rm=T)) %>% 
  ggplot() +
  geom_point(aes(year, grad_rates, col = group, group = group),
             show.legend = F) +
  geom_line(aes(year, grad_rates, col = group, group = group), 
            size=0.8, show.legend = F) +
  geom_vline(aes(xintercept=2014.5), linetype = 3) +
  annotate("text", label = "Treatment",
           x = 2013.9, y = 76, size = 3.5, colour = "black") +
  geom_curve(aes(x = 2013.9, y = 75.9, xend = 2014.4, yend = 75.3),
             arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", label = "Treated", x=2017, y=73.2, 
           size = 4.2, colour = "#377EB8") +
  annotate("text", label = "Control", x=2017, y=76.2, 
           size = 4.2, colour = "#E41A1C") +
  labs(y="graduation rate (%)",
       x=NULL) +
  ggtitle(label = "Adjusted Cohort Graduation Rate", 
          subtitle = "Treated group vs. Control Group") +
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))
ggsave("parallel4.png", width = 7, height = 5, device='tiff', dpi=700)



# data for two-way fixed effects
mich <- data_cleaned2 %>% 
  filter(race != "Total" & disability == "Total" & homeless == "Total" & 
           foster_care == "Total" & lep == "All students"
         & econ_disadvantaged == "Total") %>% 
  mutate(group = ifelse(fips == "Michigan", "treated", "control")) %>% 
  mutate(Michigan = group == "treated",
         year_num = case_when(
                              year == 2011 ~ 1,
                              year == 2012 ~ 2,
                              year == 2013 ~ 3,
                              year == 2014 ~ 4,
                              year == 2015 ~ 5,
                              year == 2016 ~ 6,
                              year == 2017 ~ 7,
                              year == 2018 ~ 8),
         treatment = Michigan == TRUE & year_num %in% c(5, 6, 7, 8),
         post = (year >= 2015))

mich$race <- droplevels(mich$race)

mich$race <- factor(mich$race, levels = c("White", "Black",
                                          "Asian", "Two or more races",
                                          "Hispanic", "American Indian or Alaska Native"))


# only economic disadvantaged students
mich_eco <- data_cleaned2 %>% 
  filter(econ_disadvantaged == "Yes") %>% 
  mutate(group = ifelse(fips == "Michigan", "treated", "control")) %>% 
  mutate(Michigan = group == "treated",
         year_num = case_when(
           year == 2011 ~ 1,
           year == 2012 ~ 2,
           year == 2013 ~ 3,
           year == 2014 ~ 4,
           year == 2015 ~ 5,
           year == 2016 ~ 6,
           year == 2017 ~ 7,
           year == 2018 ~ 8),
         treatment = Michigan == TRUE & year_num %in% c(5, 6, 7, 8),
         post = (year >= 2015))


# only white students
mich_white <- data_cleaned2 %>% 
  filter(race == "White") %>% 
  mutate(group = ifelse(fips == "Michigan", "treated", "control")) %>% 
  mutate(Michigan = group == "treated",
         year_num = case_when(
           year == 2011 ~ 1,
           year == 2012 ~ 2,
           year == 2013 ~ 3,
           year == 2014 ~ 4,
           year == 2015 ~ 5,
           year == 2016 ~ 6,
           year == 2017 ~ 7,
           year == 2018 ~ 8),
         treatment = Michigan == TRUE & year_num %in% c(5, 6, 7, 8),
         post = (year >= 2015))

# only black students
mich_black <- data_cleaned2 %>% 
  filter(race == "Black") %>% 
  mutate(group = ifelse(fips == "Michigan", "treated", "control")) %>% 
  mutate(Michigan = group == "treated",
         year_num = case_when(
           year == 2011 ~ 1,
           year == 2012 ~ 2,
           year == 2013 ~ 3,
           year == 2014 ~ 4,
           year == 2015 ~ 5,
           year == 2016 ~ 6,
           year == 2017 ~ 7,
           year == 2018 ~ 8),
         treatment = Michigan == TRUE & year_num %in% c(5, 6, 7, 8),
         post = (year >= 2015))

#only hispaic students
mich_hisp <- data_cleaned2 %>% 
  filter(race == "Hispanic") %>% 
  mutate(group = ifelse(fips == "Michigan", "treated", "control")) %>% 
  mutate(Michigan = group == "treated",
         year_num = case_when(
           year == 2011 ~ 1,
           year == 2012 ~ 2,
           year == 2013 ~ 3,
           year == 2014 ~ 4,
           year == 2015 ~ 5,
           year == 2016 ~ 6,
           year == 2017 ~ 7,
           year == 2018 ~ 8),
         treatment = Michigan == TRUE & year_num %in% c(5, 6, 7, 8),
         post = (year >= 2015))

# estimates of MID ACGR on different categories
models_cat <- list(
  eco_dis = feols(grad_rate_midpt ~ treatment |
                  fips + year, data = mich_eco, cluster = "leaid"),
  white = feols(grad_rate_midpt ~ treatment |
                  fips + year, data = mich_white, cluster = "leaid"),
  black = feols(grad_rate_midpt ~ treatment |
                  fips + year, data = mich_black, cluster = "leaid"),
  hisp = feols(grad_rate_midpt ~ treatment |
                  fips + year, data = mich_hisp, cluster = "leaid")
)

modelsummary(models_cat, stars = c('*' = .1, '**' = .05, '***' = .01))

# DD estimates on MID LOW HIGH ACGR of economic disadvantaged students
dd_eco <- list(
  LOW = feols(grad_rate_low ~ treatment | 
                fips + year, data = mich_eco, cluster = "leaid"),
  MID = feols(grad_rate_midpt ~ treatment |
                fips + year, data = mich_eco, cluster = "leaid"),
  HIGH = feols(grad_rate_high ~ treatment |
                 fips + year, data = mich_eco,  cluster = "leaid")
)
msummary(dd_eco, stars = c('*' = .1, '**' = .05, '***' = .01))

# event study for economic disadvantaged students
events_eco <- list(
  low = feols(grad_rate_low ~ i(year, Michigan, ref = "2014") | 
                fips + year_num, data = mich_eco, cluster = "leaid"),
  mid = feols(grad_rate_midpt ~ i(year, Michigan, ref = "2014") | 
                fips + year_num, data = mich_eco, cluster = "leaid"),
  high = feols(grad_rate_high ~ i(year, Michigan, ref = "2014") | 
                 fips + year_num, data = mich_eco,  cluster = "leaid"))

# selecting only mid ACGR
mid <-  feols(grad_rate_midpt ~ i(year, Michigan, ref = "2014") | 
              fips + year_num, data = mich_eco, cluster = "leaid")
msummary(mid, stars = c('*' = .1, '**' = .05, '***' = .01))

tiff("Plot4.tiff", width = 7, height = 5, units = 'in', res = 300)
iplot(mid, ref.line = F, pt.join = T, zero.par = list(lty=3),
      pt.lwd = 1.7, ci.lwd = 1.2, ci.lty = 1,
      grid.par = list(vert=F, horiz=F),
      main="Effect on mid ACGR for Economic Disadvantaged Students")
dev.off()


# estimates on all students of MID ACGR
dd_models <- list(
  '(1)' = feols(grad_rate_midpt ~ treatment  |
                  group + post, data = mich),
  '(2)' = feols(grad_rate_midpt ~ treatment  |
                  group + post, data = mich, cluster = "fips"),
  '(3)' = feols(grad_rate_midpt ~ treatment |
                  fips + year, data = mich, cluster = "fips"),
  '(3)' = feols(grad_rate_midpt ~ treatment |
                  fips + year, data = mich, cluster = "leaid")
)

modelsummary(dd_models, stars = c('*' = .1, '**' = .05, '***' = .01))


#event study 
events = feols(grad_rate_midpt ~ i(year, Michigan, ref = "2014") | 
                     fips + year_num, data = mich,  cluster = "leaid")

msummary(events, stars = c('*' = .1, '**' = .05, '***' = .01))

tiff("Plot3.tiff", width = 7, height = 5, units = 'in', res = 300)
iplot(events, ref.line = F, pt.join = T, zero.par = list(lty=3),
      pt.lwd = 1.7, ci.lwd = 1.2, ci.lty = 1,
      grid.par = list(vert=F, horiz=F),
      main="Effect of Change in Compulsory School on mid ACGR")
dev.off()
ggsave("eventsss.png", width=5, height=4,units="cm")



#Placebo test
#choose a fake treatment period
placebo_data <- data_cleaned2 %>% 
  filter(race != "Total" & disability == "Total" & homeless == "Total" & 
           foster_care == "Total" & lep == "All students" & econ_disadvantaged == "Total") %>% 
  filter(year < 2015) %>% #we select only data before treatment
  mutate(group = ifelse(fips == "Michigan", "treated", "control")) %>% 
  mutate(Michigan = group == "treated",
         year_num = case_when(
                              year == 2011 ~ 1,
                              year == 2012 ~ 2,
                              year == 2013 ~ 3,
                              year == 2014 ~ 4),
         FakeTreatment1 = Michigan == TRUE & year_num %in% c(4),
         FakeTreatment2 = Michigan == TRUE & year_num %in% c(2,3))

placebo_data$race <- droplevels(placebo_data$race)

placebo_data$race <- factor(placebo_data$race, levels = c("White", "Black",
                                          "Asian", "Two or more races",
                                          "Hispanic", "American Indian or Alaska Native"))

#placebo estimates
placebo_models <- list(
  'PLACEBO 1' = feols(grad_rate_midpt ~ FakeTreatment1 |
                  fips + year, data = placebo_data, cluster = "leaid"),
  'Placebo 2' = feols(grad_rate_midpt ~ FakeTreatment2 |
                  fips + year, data = placebo_data, cluster = "leaid")
)

msummary(placebo_models, stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_rename = c("FakeTreatment1TRUE"= "Fake Treatment 1",
                         "FakeTreatment2TRUE"= "Fake Treatment 2"))


#event study on all levels of graduation rates
events_rates <- list(
  low = feols(grad_rate_low ~ i(year, Michigan, ref = "2014") | 
                fips + year_num, data = mich, cluster = "leaid"),
  mid = feols(grad_rate_midpt ~ i(year, Michigan, ref = "2014") | 
                fips + year_num, data = mich, cluster = "leaid"),
  high = feols(grad_rate_high ~ i(year, Michigan, ref = "2014") | 
                 fips + year_num, data = mich, cluster = "leaid"))

msummary(events_rates, stars = c('*' = .1, '**' = .05, '***' = .01))

iplot(events_rates, ref.line = T, pt.join = T )


# estimates with race as control and residual clustered at district level
mich$race <- droplevels(mich$race)

mich$race <- factor(mich$race, levels = c("White", "Black",
                                          "Asian", "Two or more races",
                                          "Hispanic", "American Indian or Alaska Native"))

dd_rates_r <- list(
  LOW = feols(grad_rate_low ~ treatment + race | 
                fips + year, data = mich, cluster = "leaid"),
  MID = feols(grad_rate_midpt ~ treatment + race |
                fips + year, data = mich, cluster = "leaid"),
  HIGH = feols(grad_rate_high ~ treatment + race |
                fips + year, data = mich,  cluster = "leaid")
)
msummary(dd_rates_r, stars = c('*' = .1, '**' = .05, '***' = .01))

# event study with race as control
events_rates_r <- list(
  low = feols(grad_rate_low ~ i(year, Michigan, ref = "2014") + race | 
                fips + year_num, data = mich, cluster = "leaid"),
  mid = feols(grad_rate_midpt ~ i(year, Michigan, ref = "2014") + race | 
                fips + year_num, data = mich, cluster = "leaid"),
  high = feols(grad_rate_high ~ i(year, Michigan, ref = "2014") + race | 
                fips + year_num, data = mich,  cluster = "leaid"))

msummary(mid, stars = c('*' = .1, '**' = .05, '***' = .01))

iplot(events_race_r)


