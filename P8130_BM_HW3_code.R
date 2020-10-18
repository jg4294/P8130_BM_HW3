library(tidyverse)
library(ggplot2)
################################################################################################
#                                     Problem 1                                                #
################################################################################################
# data import
exercise = read_csv(file = "./data/Exercise.csv") %>%
  janitor::clean_names() %>%
  select(group, systolic_pre, systolic_post) %>%
  mutate(systolic_diff = systolic_post - systolic_pre) # difference

head(exercise)

##############
###Part (a)###
##############

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
control = exercise %>% filter(group == 0)
mean_diff_con = mean(control$systolic_diff) # -3.333333 
sd_diff_con = sd(control$systolic_diff) #14.81312
n_con = nrow(control) #36
t_stat_con = (mean_diff_con - 0)/(sd_diff_con/sqrt(n_con)) #-1.350154
t_crit = qt(0.975, 35) #2.030108
## summary:
t.test(control$systolic_post, control$systolic_pre, paired = T)

# H0: mu2-mu1 = 0
# |t_stat_con| < |t_crit|. 
# Reject H0, and conclude that the Systolic BP at 6 month is significant different from the baseline values for the control group 

##############
###Part (b)###
##############

# Now perform a test and provide the 95% confidence interval to assess the Systolic BP 
# absolute changes between the two groups. 

## Two-independent samples(unequal variance): mean_diff_int - mean_diff_con
## Step 1: Test for Equality of Variance 
# F_stat
F_stat = var(intervention$systolic_diff)/var(control$systolic_diff)#1.343327
# F_crit
F_crit = qf(0.975,35,35)#1.961089
## Step 2: two-samples independent t-test
# calculate s^2, s
s_2 = (35*sd_diff_int^2 + 35*sd_diff_con^2)/(36 + 36 - 2)#257.0964
s = sqrt(s_2)#16.03423
#95% C.I
qt(0.975,70) #1.994437
mean_diff_int - mean_diff_con #-5.25
lower_bound = (mean_diff_int - mean_diff_con) - qt(0.975,70)*s*sqrt(2/36)#-12.78758
upper_bound = (mean_diff_int - mean_diff_con) + qt(0.975,70)*s*sqrt(2/36)#2.287583

##############
###Part (c)###
##############

par(mfrow = c(2,2))
ggplot(intervention, aes(x = systolic_pre)) +
  geom_histogram(color = "black", fill = "pink") +
  labs(
    title = "The distribution of Systolic_pre in intervention group",
    x = "Systolic Pre",
    y = "Count"
  )

ggplot(intervention, aes(x = systolic_post)) +
  geom_histogram(color = "black", fill = "pink") +
  labs(
    title = "The distribution of Systolic_post in intervention group",
    x = "Systolic Post",
    y = "Count"
  )

ggplot(control, aes(x = systolic_pre)) +
  geom_histogram(color = "black", fill = "pink") +
  labs(
    title = "The distribution of Systolic_pre in control group",
    x = "Systolic Pre",
    y = "Count"
  )

ggplot(control, aes(x = systolic_post)) +
  geom_histogram(color = "black", fill = "pink") +
  labs(
    title = "The distribution of Systolic_post in control group",
    x = "Systolic Post",
    y = "Count"
  )

################################################################################################
#                                     Problem 2                                                #
################################################################################################

# a)
sample1 = rnorm(20,mean = 120,sd = 15)
mean1 = mean(sample1)
z_stat = (mean1 - 120)/(15 / sqrt(20))
qnorm(0.975)



