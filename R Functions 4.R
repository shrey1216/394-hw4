#1
#(a) Use the R script to regress the log return of Pfizer on the log returns of Exxon and
# Citigroup (with the intercept). Write out the fitted model explicitly. 
stocks <- read.table("/Users/shreyanwankavala/Desktop/AMS\ 394\ /d_logret_6stocks.txt", header = TRUE)
lm(stocks$Pfizer ~ stocks$Exxon + stocks$Citigroup, data = stocks)

# The fitted model is Pfizer = -0.005257 + 0.287636 * Exxon + 0.185977 * Citigroup

#(b) Use the R script to generate an ANOVA table for the multiple regression in
# part (a).
anova(lm(stocks$Pfizer ~ stocks$Exxon + stocks$Citigroup, data = stocks))

#(c) Test whether the coefficient of the log return of Exxon is zero based on the
# ANOVA table in part (b) at the level of 5%. What is your conclusion?
anova(lm(stocks$Pfizer ~ stocks$Exxon + stocks$Citigroup, data = stocks))

# On the ANOVA table from part (b), the F test for Exxon gives a p-value of 0.003475. This p-value
# is less than 0.05 so it is statistically significant, which means that we can reject the null hypothesis that the coefficient of
# the log return of Exxon is zero. 

#(d)  A researcher is interested in comparing the means of the log returns of Pfizer,
# Exxon, and Citigroup. Suppose the log returns are mutually independent. Use the R
# script to generate an ANOVA table of the log returns of Pfizer, Exxon, and Citigroup.
vals <- c(stocks$Pfizer, stocks$Exxon, stocks$Citigroup)
companies <- c(rep("Pfizer", each=64), rep("Exxon", each=64), rep("Citigroup", each=64))
anova(lm(vals ~ companies))

#(e) Test whether there are significant differences in log returns among the three
# groups at the level of 5%. What is your conclusion?

# The associated p-value from the ANOVA table is 0.2407. This p-value is greater than
# 0.05, which means that the mean from none of the groups differs from another at the 0.05
# level of significance. There aren't significant differences log returns among the three 
# groups at the 5% level.

#(f)  Use the R script to test if the proportion of positive log return of Citigroup is
# 0.5 at the level of 5%. What is your conclusion?
prop.test(sum(stocks$Citigroup > 0  ), n = 64, p = 0.5)

# The p-value is 0.578125 which is much larger than 0.05. Because of this, we can 
# not reject the null hypothesis that the proportion of positive log return
# of Citigroup is 0.5 at the level of 5%. 

#2
#(a) Use the R script to make side-by-side boxplots for the three groups. Do these
# plots indicate any obvious differences between the groups?
caffeine_taps <- matrix(c(242, 245, 244, 248, 247, 248, 242, 244, 246, 242, 248, 246, 245, 247, 248, 250, 247, 246, 243, 244, 246, 248, 250, 252, 248, 250, 246, 248, 245, 250), nrow = 3, byrow = TRUE)
rownames(caffeine_taps) <- c("0 mg", "100 mg", "200 mg")
names(dimnames(caffeine_taps)) <- c("Caffeine Dose", "Finger Taps per Minute")
boxplot(t(caffeine_taps))

# The plots do indicate obvious differences between the groups. As the mg of caffeine increases, 
# the number of taps rises which can be seen by the boxplot moving upwards along the graph. 
# There are more taps when the mg of caffeine increases. 

#(b) Use the R script to construct an ANOVA table and test whether there are
# significant differences in finger tapping between the students treated with different doses
# of caffeine. Use α = 0.10. Interpret your results
zero_mg <- c(242, 245, 244, 248, 247, 248, 242, 244, 246, 242)
one_hundred_mg <- c(248, 246, 245, 247, 248, 250, 247, 246, 243, 244)
two_hundred_mg <- c(246, 248, 250, 252, 248, 250, 246, 248, 245, 250)
taps <- c(zero_mg, one_hundred_mg, two_hundred_mg)
mg <- c(rep("0 mg", each=10), rep("100 mg", each=10), rep("200 mg", each=10))

anova(lm(taps ~ mg))

# The associated p-value from the ANOVA table is 0.006163. This p-value is less than
# 0.1, which means that the mean from at least one group differs from another at the 0.1
# level of significance. There are significant differences in finger tapping between the 
# students treated with different doses of caffeine. 

#(c) Use the R script to check the normality and constant variance assumptions by
# making residual plots.
plot(fitted(lm(taps ~ mg)), resid(lm(taps ~ mg)))
qqnorm(resid(lm(taps ~ mg)))
qqline(resid(lm(taps ~ mg)))

# The data values of the plot fall along the line, and looks to be linear. Because of it, 
# it is normally distributed. From the first plot, we can also conclude that the data has
# constant variance. This is because the spread of the values seems to be the same as the mg 
# of caffeine increases. In a data set without constant variance, this would not happen. 

#(d) Use the R script to check the normality and constant variance assumptions by
# implementing the Shapiro-Wilk test and the Bartlett’s test.
shapiro.test(taps)

# After implementing the Shapiro-Wilk test, we get a p-value of 0.3401. This value is
# greater than 0.05. We fail to reject the null hypothesis and conclude that the data 
# come from the normal distribution at the 5% level.
bartlett.test(taps ~ mg)

# In this test, we get a p-value of 0.9104. This is not less than the significance 
# level of 0.05, so the null hypothesis can not be rejected that the variance is 
# the same for all treatment groups. This is in agreement from my analysis from 
# the residual graph. 

#(e) Use the R script to compare finger tap rates of the 100 mg and 200 mg caffeine
# groups with the control group using Bonferroni’s method with α = 0.10. Interpret the
# results.
pairwise.t.test(taps, mg, p.adj = "bonferroni")

# From using Bonferroni's method, we can compare finger tap rates of the different caffeine groups.
# In the output, the p-value between 0mg and 100mg is 0.3601, the p-value between 0mg and 200mg is 
# 0.0048, and the p-value between 100mg and 200mg is 0.2019. We can see that the only significant 
# difference is between 0mg and 200mg because its p-value of 0.0048 is less than 0.10. 


#3 Is there a significant difference in the incidence rates for cold between the Vitamin C and
# Placebo groups at α = 0.05? What do you conclude about the effectiveness of Vitamin C in
# preventing cold?
success <- c(122, 109)
total <- c(139, 140)
prop.test(success, total)

# After running the two-proportions test, I found that the p-value of the test is 
# 0.04186. This p-value is less than the significance level of 0.05. Therefore, we can conclude that 
# there is a significant difference in the incidence rates for cold between the Vitamin C and 
# Placebo groups. As the observed proportion for Vitamin C is greater than that of the Placebo 
# group, it can be concluded that Vitamin C is effective in preventing cold.

#4
#(a) Use the R script to construct the above table
income_satisfaction <- matrix(c(20, 24, 80, 82, 22, 38, 104, 125, 13, 28, 81, 113, 7, 18, 54, 92), nrow = 4, byrow = TRUE)
colnames(income_satisfaction) <- c("Very Dissatisfied", "Little Dissatisfied", "Moderately Satisfied", "Very Satisfied")
rownames(income_satisfaction) <- c("< 6000", "6000 − 15,000", "15,000 − 25,000", "> 25,000")
names(dimnames(income_satisfaction)) <- c("Income (U.S.$)", "Job Satisfaction")
income_satisfaction

#(b) Use the R script to test if the income and job satisfaction are associated at
# α = 0.05.
chisq.test(income_satisfaction)

# From the test, we see that the p-value is 0.214. This p-value is larger than 0.05, so 
# we fail to reject the null hypothesis. This means that we do not have sufficient evidence 
# to say that there is an association between income and job satisfaction.
