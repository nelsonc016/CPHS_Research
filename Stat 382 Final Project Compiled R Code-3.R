# Stat 382 Final Project
# Group 1
# Eric Navarro, Nelson Corral, Zehra Amir, Pooja Patel

setwd("/Users/poojapatel/Documents/UIC Junior Fall Semester/Stat 382/Final Project")

# Cleaning and Subsetting the Original Dataset
original <-
  read.csv(
    "/Users/poojapatel/Documents/UIC Junior Fall Semester/Stat 382/Final Project/CPS School Progress Reports SY1617.csv"
  )
summary(original)

# The original dataset we found consisted of 661 observations of 161 variables.
# It consisted of schools of all primary levels and had the results of
# standardized tests that are administered to students in 3rd grade up to 8th
# grade. We decided to focus specifically on high schools and the academic
# performance of high schoolers.

# Subsetting Dataset to Include Only High Schools
just_HS <- data.frame(original[original$Primary_Category=="HS",])
write.csv(
  just_HS,
  "/Users/poojapatel/Documents/UIC Junior Fall Semester/Stat 382/Final Project/CPS High Schools All Variables.csv"
)

#Subsetting Dataset to Include A Smaller Variety of Variables
new_HS <-data.frame(just_HS[,c(1,2,3,4,5,14,20,26,94,95,96,97,98,99,100,101,114,
                               115,120,121,127,128,133,134,135,136,137,138,139,
                               140,141,142,161)])
write.csv(new_HS, "/Users/poojapatel/Documents/UIC Junior Fall Semester/Stat 382/Final Project/Final CPS High Schools Curated Variables.csv")

#Removing High Schools with Missing Data
all_NA_removed_HS <-
  data.frame(new_HS[which(
    !(
      is.na(new_HS$Suspensions_Per_100_Students_Year_1_Pct) |
        is.na(new_HS$Suspensions_Per_100_Students_Year_2_Pct) |
        is.na(new_HS$Student_Attendance_Year_1_Pct) |
        is.na(new_HS$Student_Attendance_Year_2_Pct) |
        is.na(new_HS$One_Year_Dropout_Rate_Year_1_Pct) |
        is.na(new_HS$One_Year_Dropout_Rate_Year_2_Pct) |
        is.na(new_HS$Attainment_ACT_Grade_11_Pct) |
        is.na(new_HS$Graduation_4_Year_School_Pct_Year_2) |
        is.na(new_HS$Graduation_4_Year_School_Pct_Year_1) |
        is.na(new_HS$Graduation_5_Year_School_Pct_Year_2) |
        is.na(new_HS$Graduation_5_Year_School_Pct_Year_1) |
        is.na(new_HS$College_Enrollment_School_Pct_Year_2)
    )
  ),])

write.csv(all_NA_removed_HS, "/Users/poojapatel/Documents/UIC Junior Fall Semester/Stat 382/Final Project/Final Cleaned CPS High Schools Data.csv")

# Final Dataset with Composite ACT Scores Added

high_school_data <- read.csv("Final Cleaned CPS High Schools Data 2.csv")

# Our final subsetted dataset consists of 81 observations of 39 variables. Out
# of the 39 variables our group looked at 8 particular variables during our
# exploratory analysis. We looked at the categorical variables of School Type,
# Student Attainment Ratings, Student Attainment Ratings, and School Survey
# Results Regarding the Supportive Environment of the school. We also looked at
# the following quantitative variables: Suspensions Per 100 Students in 2016,
# Student Attendance in 2016, 4 Year Graduation Rate in 2016, and Average
# Composite ACT Score.


# Exploratory Analysis
summary(high_school_data)

# Converting Categorical Variables From Characters to Factors
high_school_data$School_Type <-
  as.factor(high_school_data$School_Type)
high_school_data$Student_Attainment_Rating <-
  as.factor(high_school_data$Student_Attainment_Rating)
high_school_data$School_Survey_Supportive_Environment <-
  as.factor(high_school_data$School_Survey_Supportive_Environment)

# Summaries of Categorical Variables
summary(high_school_data$School_Type)
summary(high_school_data$Student_Attainment_Rating)
summary(high_school_data$School_Survey_Supportive_Environment)

# Summaries of Quantitative Variables
summary(high_school_data$Suspensions_Per_100_Students_Year_2_Pct)
summary(high_school_data$Student_Attendance_Year_2_Pct)
summary(high_school_data$Graduation_4_Year_School_Pct_Year_2)
summary(high_school_data$Composite.ACT.Score)

# Barplot of School Type
school_type_counts <- table(high_school_data$School_Type)
school_type_counts
barplot(
  school_type_counts,
  main = "Distribution of Schools by School Type",
  xlab = "Type of School",
  ylab = "Number of Schools",
  cex.names = 0.7,
  col = "lightblue"
)

# Barplot of Student Attainment Rating
student_attainment_counts <- table(high_school_data$Student_Attainment_Rating)
student_attainment_counts
barplot(
  student_attainment_counts,
  main = "Student Attainment Ratings",
  xlab = "Student Attainment Rating",
  ylab = "Number of Schools",
  cex.names = 0.5,
  col = "lightseagreen"
)

# Barplot of Supportive Environment
supportive_environment_counts <-
  table(high_school_data$School_Survey_Supportive_Environment)
supportive_environment_counts
barplot(
  supportive_environment_counts,
  main = "Supportive Environment Ratings",
  xlab = "Characterization of Supportive Environment",
  ylab = "Number of Schools",
  cex.names = 0.5,
  col = "lightyellow"
)

# Histogram of Suspensions Per 100 Students in 2016
hist(
  high_school_data$Suspensions_Per_100_Students_Year_2_Pct,
  main = "Histogram of Suspensions Per 100 Students in 2016",
  xlab = "Suspensions Per 100 Students in 2016 (Percentage)",
  col = "maroon"
)

# Assessing Normality of Suspension Rates
shapiro.test(high_school_data$Suspensions_Per_100_Students_Year_2_Pct) # 1.197e-10

qqnorm(high_school_data$Suspensions_Per_100_Students_Year_2_Pct) 
qqline(high_school_data$Suspensions_Per_100_Students_Year_2_Pct)

# Based on the Shapiro test (P = 1.197e-10 < 0.05) and qqplot provided, our data
# for suspension rates does not follow a normal distribution. The P-value for
# the Shapiro test is very small, which means we have to reject the null
# hypothesis that the data comes from a normal distribution. Additionally, the
# points on the qqplot don't follow the line. Therefore, our conclusion here is
# that it is very likely that our data does not come from a normal distribution.
# Some schools could have higher or lower suspension rates, depending on the
# students at the schools.

# Histogram of Student Attendance in 2016
hist(
  high_school_data$Student_Attendance_Year_2_Pct,
  main = "Histogram of Student Attendance Rates in 2016",
  xlab = "Student in Attendance Rate",
  col = "darkseagreen1"
)

# Assessing Normality of Attendance Rates
shapiro.test(high_school_data$Student_Attendance_Year_2_Pct) # 0.0003955

qqnorm(high_school_data$Student_Attendance_Year_2_Pct)  
qqline(high_school_data$Student_Attendance_Year_2_Pct)

# Based on the Shapiro test (P = 0.0003955 < 0.05) and qqplot provided, our data
# for attendance does not follow the case of normality. The p-value for the
# Shapiro test is very small, and the points on the qqplot don't follow the
# line. Therefore, we can conclude that it is very likely that our data does not
# come from a normal distribution. Some schools could have great attendance
# rates while others might not.

# Histogram of 4 Year Graduation Rate 
hist(
  high_school_data$Graduation_4_Year_School_Pct_Year_2,
  main = "Histogram of 4 Year Graduation Rates in 2016",
  xlab = "4 Year Graduation Rate",
  col = "lightpink"
)

# Assessing Normality of Graduation Rates
shapiro.test(high_school_data$Graduation_4_Year_School_Pct_Year_2) # 0.02461

qqnorm(high_school_data$Graduation_4_Year_School_Pct_Year_2)  
qqline(high_school_data$Graduation_4_Year_School_Pct_Year_2)

# Based on the Shapiro test (P = 0.02461 < 0.05) and qqplot provided, our data
# for graduation rate does not follow a normal distribution. The P-value for the
# Shapiro test is small, and the points on the qqplot don't follow the line.
# Therefore, our conclusion here is once again that it is very likely that our
# data for graduation rates does not come from a normal distribution. Some
# schools may have higher or lower graduations rates in comparison with one
# another.

# Histogram of Average Composite ACT Scores
hist(
  high_school_data$Composite.ACT.Score,
  main = "Histogram of Composite ACT Scores",
  xlab = "Composite ACT Score",
  col = "navy"
)

# Assessing Normality of ACT Scores
shapiro.test(high_school_data$Composite.ACT.Score) # 1.611e-08

qqnorm(high_school_data$Composite.ACT.Score)  
qqline(high_school_data$Composite.ACT.Score)

# Based on the Shapiro test (P = 1.611e-8 < 0.05) and qqplot provided, our data
# for Composite ACT Scores does not follow a normal distribution. The p-value
# for the Shapiro test is very small, and the points on the qqplot don't follow
# the line. Since the P-value is a very small, we have to reject the null
# hypothesis that the data comes from a normal population. Therefore, our
# conclusion here is that the our data for Composite ACT Scores very likely does
# not come from a normal distribution.

# Significance Test for Graduation Rate

# Reported 4 Year Gradution Rate in CPS Schools

# We ran a significance test to determine whether the average graduation rate of
# the schools in our dataset differs from the reported average graduation rate
# of CPS high schools.

# Hypotheses to Be Tested:
# Null Hypothesis (H_0): The population mean graduation rate is 73.5.
# Alternate Hypothesis (H_a): The population mean graduation rate is not equal
# to 73.5.

CPS_reported_average <- 73.5
t.test(
  high_school_data$Graduation_4_Year_School_Pct_Year_2,
  alternative = "two.sided",
  mu = 73.5,
  conf.level = 0.95
)
# Results:
# P-value = 0.422
# 95% CI: 68.79365 75.49030
# Mean of Our Data: 72.14198 

# Interpretation
# Because the P-value of 0.422 is larger than 0.05, there is insufficient
# evidence at 5% significance level that the mean graduation rate of the schools
# in our dataset differs from the reported average 4 year graduation rate of
# 73.5. Based on our results, we are 95% confident our interval (68.79365,
# 75.49030) contains our true mean of graduation rate percentage.The 95%
# confidence interval contains the mean value of 73.5. Therefore, we cannot
# conclude that the mean graduation rate differs from the CPS reported average
# graduation rate.

# National 4 Year High School Graduation Rate

# We also ran a significance test to determine whether the average graduation
# rate of the schools in our dataset differs from the reported national average
# graduation rate.

national_average <- 84.6
t.test(
  high_school_data$Graduation_4_Year_School_Pct_Year_2,
  alternative = "two.sided",
  mu = national_average,
  conf.level = 0.95
)

# Results:
# P-value = 1.174 * 10^-10
# 95% CI: 68.79365 75.49030

# Interpretation: Based on our results, we are 95% confident our interval
# (68.79365, 75.49030) contains our true mean of graduation rate percentage.
# Overall, our data shows then that the amount of students that graduate among
# CPS schools range from about 68.79% to 75.49%. The P-value calculated through
# our t-test was less than 0.05, which means we have to reject the null
# hypothesis that the average graduation rate of our sample of CPS high schools
# is the same as the national average graduation rate. This means that we can
# conlude the average graduation rate of our CPS high schools does differ from
# the national graduation rate at a significance level of 0.05.


# Linear Regression For Composite ACT Scores and Student Suspension Rate

# Make a Scatterplot
plot(
  high_school_data$Suspensions_Per_100_Students_Year_2_Pct,
  high_school_data$Composite.ACT.Score,
  xlab = "Suspensions Per 100 Students (Percentage)",
  ylab = "Composite ACT Score",
  main = "Composite ACT Score vs Suspensions Rate"
)
# We see that ACT scores is the dependent variable while the student suspensions
# is the independent variable.

# Computing r
cor(
  high_school_data$Suspensions_Per_100_Students_Year_2_Pct,
  high_school_data$Composite.ACT.Score
)
# r = -0.47249, hence r is negative and is closer to 0 so we see a weak negative
# linear association between the two variables.

cor.test(
  high_school_data$Suspensions_Per_100_Students_Year_2_Pct,
  high_school_data$Composite.ACT.Score
)
# This checks whether rho is non-zero and gives a confidence interval. 

regline <-
  lm(
    high_school_data$Composite.ACT.Score ~ high_school_data$Suspensions_Per_100_Students_Year_2_Pct,
    data = high_school_data
  )
regline
summary(regline)
# The regression line is: y = -0.04887x + 18.71965, where x is the composite ACT
# score and y is the student suspension rate.

abline(regline, col="red")

res<- resid(regline)
res
# Computes the residuals

# We know that residuals should be normally distributed, randomly scattered, and
# equally distributed.

plot(res)
# Plots the residuals 

hist(res)
qqnorm(res)  
qqline(res)
shapiro.test(res)
# We use multiple tests to determine normality, and we know that the residuals
# are not distributed normally.

# Linear association between percentage of students who attended the whole
# school year and composite act scores

# We believe that composite act scores are the dependent variable, and the
# percentage of students who attended the school year is the independent
# variable.

# We believe there is a strong linear association between composite act scores
# and attendance. This is because if a student attends a class more often than
# the student will know more content compared to the situation where the student
# misses class often.

plot(
  high_school_data$Student_Attendance_Year_2_Pct,
  high_school_data$Composite.ACT.Score,
  xlab = "Student Attendance Rate",
  ylab = "Composite ACT Score",
  main = "Composite ACT Scores vs Student Attendance Rates"
)
#From the plot, there does seem to be a positive linear association.

cor(
  high_school_data$Student_Attendance_Year_2_Pct,
  high_school_data$Composite.ACT.Score
)
# The co-efficient, 0.65, which tells us there seems to be a strong positive
# linear association between the student attendance rates and the schools'
# average composite act scores.


# cor.test to check confident that r is nonzero
cor.test(
  high_school_data$Student_Attendance_Year_2_Pct,
  high_school_data$Composite.ACT.Score
)
# We can reject the null hypothesis, r=0.
# We accept the alternative hypothesis that rho (r) is nonzero since p < 0.05
# We are confident that there is a linear association between two variables.

regline <-lm(Composite.ACT.Score ~ Student_Attendance_Year_2_Pct, data = high_school_data)

# It gives us the slope (0.4544) and the rate of change( how much the the
# percentage of student attendance for the school year changes the composite
# score for ACT)

summary(regline)
# Regression line is y = 0.454x -22.79, where x is the student attendance rate
# and y is the average composite ACT score.

abline(regline, col ="blue")

# Conclusion
# Since the co-efficient is closer to 1 and the plot seems to show a positive
# linear association, we can conclude that there is a strong linear association
# between percentage of students that attended the school year and composite act
# scores.

# Addressing concerns 
# Check if regression line is appropriate (residuals) and if R^2 is close
# 1(model fits data well).

resid(regline) 

# Residuals have to be normally distributed, randomly scattered equally spread
# out.



plot(high_school_data$Student_Attendance_Year_2_Pct,resid(regline)) 
abline(h=0)
# Residuals looks randomly scattered and sort of equally spread out.

# Are the residuals normal?

hist(resid(regline))
# They do not look normal.

qqnorm(resid(regline))  
qqline(resid(regline))
# A good chunk of data is not on line.

shapiro.test(resid(regline))
# Since p <0.05, we can reject the null hypothesis that the data follows a
# normal distribution.

# The residuals are not normal, which implies that regression line may not be
# appropriate

RSQUARED <-
  cor(
    high_school_data$Student_Attendance_Year_2_Pct,
    high_school_data$Composite.ACT.Score
  ) ^ 2
# R^2 is close to zero. Thus this implies that the model does not fit the data
# well.


# ANOVA Test For Average Composite ACT Scores Between Different Types of Schools

# Null Hypothesis: H_0: mean_career academy = mean_contract = mean_magnet =
# mean_military = mean_neighborhood = mean_selective_enrollment = mean_small.
# The null hypothesis is that the mean Composite ACT Score is equivalent for all
# seven types of schools - career academy, contract, magnet, military academy,
# neighborhood, selective enrollment, and small.

# Alternate Hypothesis: H_1: At least of two of the means are different. 
# The alternate hypothesis is that at least two of the means (mean ACT score for
# career academy schools, mean ACT score for contract schools, mean ACT score
# for magnet schools, mean ACT score for military academy schools, mean ACT
# score for neighborhood schools, mean ACT score for selective enrollment
# schools, and mean ACT score for small schools) are different.

school_type_ACT_anova <-
  aov(Composite.ACT.Score ~ School_Type, data = high_school_data)
summary(school_type_ACT_anova)

# Results:
# The F statistic is 18.95. The P-value is 3.1*10^-13.

# Interpretation:
# The P-value, 3.1*10^-13, calculated in the ANOVA test indicates that at a 0.05
# and 0.001 significance level there is a difference between the mean composite
# ACT scores for the seven groups included in the test. This indicates that the
# mean composite ACT scores for each group do differ from one another in a
# statistically significant manner. However, further tests are needed to
# determine what the difference is and between which groups there is a
# difference in the means.

TukeyHSD(school_type_ACT_anova, conf.level = .95)
plot(TukeyHSD(school_type_ACT_anova, conf.level = .95))

# Results and Interpretation:
# The results of the Tukey test indicate that at a 0.05 significance level that
# there are differences in the mean ACT scores between the 7 types of schools.
# In particular, there were significant differences in the mean ACT scores
# between the following pairs of school types: selective enrollment and career
# academy, selective enrollment and contract, selective enrollment and magnet,
# selective enrollment and military academy, selective enrollment and
# neighborhood, and small and selective enrollment. For each pair, the 95%
# confidence intervals generate indicate that the mean composite ACT score of
# the selective enrollment schools is larger in comparison to the mean composite
# ACT scores of the other 6 types of schools.
