library(tidyverse)
# data import
exercise = read_csv(file = "./Exercise.csv") %>%
  janitor::clean_names() %>%
  select(group, systolic_pre, systolic_post) %>%
  mutate(systolic_diff = systolic_post - systolic_pre) # difference

head(exercise)


# Perform appropriate tests to assess if the Systolic BP at 6 months is significantly
# different from the baseline values for each of the groups:intervention, control

## Paired t-test
t.test(exercise$systolic_post[1:36], exercise$systolic_pre[1:36], paired = T)
t.test(exercise$systolic_post[37:72], exercise$systolic_pre[37:72], paired = T)

## More details: 
## intervention group:
intervention = exercise %>% filter(group == 1)
mean_diff_int = mean(pull(intervention, systolic_diff)) # -8.583333
sd_diff_int = sd(pull(intervention, systolic_diff)) #17.1687
n_int = nrow(intervention) #36
t_stat_int = (mean_diff_int - 0)/(sd_diff_int/sqrt(n_int)) #-2.999645
t_crit = qt(0.975, 35) #2.030108
## summary:
t.test(intervention$systolic_post, intervention$systolic_pre, paired = T)

# H0: mu2-mu1 = 0
# |t_stat_int| > |t_crit|. 
# Fail to reject H0, and conclude that the Systolic BP at 6 month is not significant different from the baseline values for the intervention group 


## control group:
control = exercise %>% filter(group == 2)
mean_diff_con = mean(control$systolic_diff) # -3.333333 
sd_diff_con = sd(control$systolic_diff)
n_con = nrow(control) #36
t_stat_con = (mean_diff_con - 0)/(sd_diff_con/sqrt(n_con)) #-1.3502
t_crit = qt(0.975, 35) #2.030108
## summary:
t.test(control$systolic_post, control$systolic_pre, paired = T)

# H0: mu2-mu1 = 0
# |t_stat_con| < |t_crit|. 
# Reject H0, and conclude that the Systolic BP at 6 month is significant different from the baseline values for the control group 



