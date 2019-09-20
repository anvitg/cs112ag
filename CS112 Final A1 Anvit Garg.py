#!/usr/bin/env python
# coding: utf-8

# In[1]:


### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data structure
head(foo)

# one thing to be very careful with (in this data set) is the use of dates. 8 columns involve dates.

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)


# In[89]:


for(i in date.columns)  
  
{
  
  
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  
 
  foo[which_values_are_missing, i] <- NA
  
 
  foo[, i] <- as.Date(as.character(foo[, i]))
  
}


# foo[3,12]
# [1] "1968-03-13"

# foo[4,12]
# [1] "1968-07-03"

# foo[3,12] - foo[4,12]
# Time difference of -112 days


which.have.NAs <- which(is.na(foo$Rating == TRUE)) 
revised_foo <- foo[-which.have.NAs, ]


# Read all the questions before you begin.
# IMPORTANT: For all questions below, only consider projects with non-missing “Circulation.Date” >= 2009-01-01. Exclude all other projects from your analysis. Failing to do so will
# severely impact your scores, even if your analysis is otherwise correct.
# Also, note that you must provide a link to your R code. You should create a gist with all
# of your code and include the link at the start of your assignment (see how to create gists here).
# Copying and pasting large excerpts of code in the body of your assignment will entail a 1 in
# #professionalism and may impact other scores as well. If an excerpt of code is necessary to
# demonstrate a point you’re making, limit yourself to a few lines which should nevertheless be
# included in your gist.
# Finally, note that the column called “Rating” is the success rating of the project at completion.
# 0 = lowest, 3 = highest.

# In[90]:


#Considering only non-missing Circulation dates after 01-01-2009
circ.date.values <- which(as.character(revised_foo$CirculationDate) >= "2009-01-01" &
                            as.character(revised_foo$CirculationDate) != "NA")
revised_foo <- revised_foo[circ.date.values, ]

foo$CirculationDate
foo


# 1.When projects are approved, they are approved for a certain period (until the time of “original completion date”). While projects are active, this “original” completion date is often
# extended, and then there is a “revised” completion date. You have been told that project
# duration at approval is generally about two years (24 months). In other words, (purportedly)
# when projects are approved, the difference between the original project completion date and
# the approval date is (supposedly) approximately 24 months. 
# (a) Is this claim true? Explain. 

# In[91]:


#First, we have to find which approval dates are missing, given that it has a non-NA circulation date. 
missing_app_dates <- which(is.na(revised_foo$ApprovalDate) |
                                  is.na(revised_foo$OriginalCompletionDate))
if (length(missing_app_dates) > 0) {
  project.duration <- revised_foo[-missing_app_dates, c(11, 17, 25)] # 11, 17, and 25 represent the columns as named above
} else {
  project.duration <- revised_foo[, c(11, 17, 25)]
}
project.duration 

proj_duration <- as.Date(as.character(project.duration[, 2])) -
                        as.Date(as.character(project.duration[, 1]))
proj_duration # A vector has been created with the differences between approval dates and completion dates


# In[92]:


#Calculating mean, median, quantile and histogram of the created vector proj_duration
mean(proj_duration)
median(proj_duration)
quantile(proj_duration)
hist(as.numeric(proj_duration), main = "", xlab = "Project duration (days)")


# Answer: The claim is not true. The mean number of days for duration time of project is about 651 days, which in year units is 1.78 years, less than the 2 year claim given in the question. You can also see by the median (600 days is about 1.64 years), interquartile range and histogram created above that most of the project durations are less than 730 days (2 years). The true value is likely to be closer to 600 days (median). Hence, this claim is false. 

# (b) Has the length of project delay, measured as the difference between “OriginalCompletionDate” and “RevisedCompletionDate”, changed over time (consider projects circulated earlier and circulated later)? You will need to make a choice of how to deal
# with missing information, which you should explicitly discuss. Be sure to also discuss
# mean delays, median delays, and the interquartile range of delays (using the “quantile”
# function). Approximate suggested length: 3-5 sentences.
# 

# In[108]:


dates_missing <- which(is.na(revised_foo$OriginalCompletionDate) |
                                  is.na(revised_foo$RevisedCompletionDate))
if (length(dates_missing) > 0) {
  proj_dur <- revised_foo[-dates_missing, c(17, 18, 25)]
} else {
  proj_dur <- revised_foo[, c(17, 18, 25)]
}
proj_dur
proj_durv <- as.Date(as.character(proj_dur[, 2])) -
                        as.Date(as.character(proj_dur[, 1]))

proj_durv
mean(proj_durv)
median(proj_durv)
quantile(proj_durv)
plot(proj_dur[3], proj_durv, xlab = "Circulation date (year)", ylab = "Project delay (days)")


# Answer: When project durations are compared from 2009 to 2018, we see that there are not really any trends or changes seen. Most projects have a original project duration between 250 and 100 days.

# (c) How does the original planned project duration differ from actual duration (if the actual
# duration is measured as the duration between “ApprovalDate” and “RevisedCompletionDate”)? Once again, use means, medians, and interquartile ranges to explain your results. Approximate suggested length: 3-5 sentences

# In[117]:


#Creating a vector for Actual duration, vector for predicted duration was calculated earlier
missing <- which(is.na(revised_foo$ApprovalDate) |
                                  is.na(revised_foo$RevisedCompletionDate))
if (length(missing) > 0) {
  act_dur <- revised_foo[-missing, c(11, 18, 25)]
} else {
  act_dur <- revised_foo[, c(11, 18, 25)]
}
act_dur
act_durv <- as.Date(as.character(act_dur[, 2])) -
                        as.Date(as.character(act_dur[, 1]))

act_durv
mean(act_durv)
median(act_durv)
quantile(act_durv)
hist(as.numeric(act_durv), main = "", xlab = "Actual Project duration (days)")









# Answer: To find out the change seen in project duration over time, the difference between original completion date and revised completion date was calculated (revised dates - org dates). The mean was calculated to be 569 days and the median was 457 days, it is also 50 percent quantile range. This shows us that projects are usually on average put off by 457 days, however there are some exceptions in the 4th quantile which make the mean increase. The data above suggests that according to the mean, project are usually postponed by 569 days, but if a project were arbitrarily chosen, it is likely to be postponed by 457 days. 

# 2.What % of projects completed between 2010 and now were rated 0? What % over the same
# time period were rated 1? What % were rated 2? What % were rated 3? Answer these
# questions using a table or a figure. Provide a title and an explanatory sentence or two that
# provides the numerical % results rounded to the nearest percentage point.

# In[95]:


#Considering only non-missing Ratings after 01-01-2010
rating.values <- which(as.character(revised_foo$CirculationDate) >= "2010-01-01" &
                            as.character(revised_foo$CirculationDate) != "NA"   &
                            as.character(revised_foo$Rating) != "NA")
revised1_foo <- revised_foo[rating.values, ]

revised1_foo$CirculationDate
revised1_foo


# In[96]:


colnames(revised1_foo)
rating <- revised1_foo[, c(22, 5)]
totalproj <- nrow(rating)

for (i in 0:3) {
 print(sum(rating$Rating == i) / totalproj)
}


# Answer: Distribution of Project Ratings
#         Rating         0         1        2        3
#         Percentage    2.4%     12.9%    71.3%    13.3%
#         This table shows the distribution of how projects were rated (0-3), for all projects circulated after 01-01-2010. A             large majority of the projects were rated as "2" (71.3%), followed by "3" (13.3%), "1" (12.9%) and "0" (2.4%). 

# (3) Repeat problem 2, but this time limit your analysis purely to policy and advisory technical
# assistance (”PATA”) projects.

# In[56]:



ppta_projects <- which(rating$Type == "PPTA")
ppta <- rating[ppta_projects, ]
totalproj <- nrow(ppta)

for (i in 0:3) {
  print(sum(no.ppta$Rating == i) / totalproj)
}


# Answer:Distribution of Project Ratings (only PATA projects considered)
#        Rating           0         1         2        3
#        Percentage      15.5%    37.6%     41.6%     5.1%
#        This table shows the distribution of how only PATA projects were rated (0-3), for all projects circulated after 01-01-          2010. Majority of projects were rated as "2" (41.6%), followed by "1" (37.6%), "0" (15.5%) and "3" (5.1%). 

# 4. Identify the top 10% of projects by “Revised.Amount” and the bottom 10% of projects
# by “RevisedAmount” (“RevisedAmount” shows the final project budget). Compare the
# ratings of these projects. Can you draw a causal conclusion about the effect of budget
# size on ratings? Why or why not? Hint: Compare the characteristics of the two project
# groupings, e.g., “Dept,” “Division,” “Cluster,” “Country.” Approximate suggested length:
# 3-5 sentences. 

# In[109]:


revised.amt <- rev(order(revised_foo$RevisedAmount))
revised2_foo <- revised_foo[revised.amt, ]
totalproj <- nrow(revised2_foo)

t10_revisedamt <- revised2_foo[0:round(0.1 * totalproj), ]
b10_revisedamt <- revised2_foo[(round(0.1 * totalproj) + 1):totalproj, ]
length(t10_revisedamt$RevisedAmount)
length(b10_revisedamt$RevisedAmount)

(mean(t10_revisedamt$Rating) - mean(b10_revisedamt$Rating))/mean(b10_revisedamt$Rating)

mean(t10_revisedamt$RevisedAmount) / mean(b10_revisedamt$RevisedAmount)

quantile(t10_revisedamt$Rating)
quantile(b10_revisedamt$Rating)

median.difference <- median(t10_revisedamt$Rating) - median(b10_revisedamt$Rating)
median.difference

plot(density(t10_revisedamt$Rating), col = "black", lwd = 3)
lines(density(b10_revisedamt$Rating), col = "blue", lwd = 3)

median.diff

t10_revisedamt
b10_revisedamt

ratings <- table(c(replicate(length(t10_revisedamt$Rating), 0), replicate(length(b10_revisedamt$Rating), 1)),
                 c(t10_revisedamt$Rating, b10_revisedamt$Rating))

barplot(ratings, xlab = "Rating (0 - 3)", col = c("black","grey"), ylab = "Frequency",
        ylim = c(0, 350),
        legend = c("Top 10%", "Bottom 10%"), beside = TRUE)

summary(factor(t10_revisedamt$Dept))

summary(factor(b10_revisedamt$Dept))

summary(factor(t10_revisedamt$Division))

summary(factor(b10_revisedamt$Division))

summary(factor(t10_revisedamt$Country))

summary(factor(b10_revisedamt$Country))

t10.dur <- as.Date(as.character(t10_revisedamt$RevisedCompletionDate)) -
  as.Date(as.character(t10_revisedamt$ApprovalDate))

b10.dur <- as.Date(as.character(b10_revisedamt$RevisedCompletionDate)) -
  as.Date(as.character(b10_revisedamt$ApprovalDate))

plot(density(as.numeric(t10.dur)), col = "blue", lwd = 3, xlim = c(-100, 5000), ylim = c(0, 0.001),
     xlab = "Actual Duration (days)", ylab = "Occurence (Density)", main = "")

lines(density(abs(as.numeric(b10.dur))), col = "green", lwd = 3)


# Answer: If the distribution of the ratings is taken into consideration, there is no noticeable effect of level of funding on the rating value. The average rating of the top ten percent of funded projects (1.99) is 1.76% larger than the average rating of the bottom ten percent of funded projects (1.95) and the average funding amount is about six times. If it was assumed that there is an effect of ratings on level of funding, causality cannot be established because the funding amounts are correlated with a myriad of other variables depending on country, department and other characteristics of the project. Hence, this indicates we cannot keep the control variables constant to draw a causal relationship between level of funding and rating values. The graph below shows discrepancy in one such control variable (actual project duration vs density of occurence)

# In[75]:


summary(revised_foo[which(revised_foo$Rating == 0), ]$RevisedAmount)
summary(revised_foo[which(revised_foo$Rating == 3), ]$RevisedAmount)
colnames(revised_foo)


# (5) Imagine your manager asks you to apply Jeremy Howard’s drivetrain model to the problem
# of optimal budget-setting to minimize project completion delays (i.e., the difference between
# revised and original completion dates). In such a situation, what would be the:
# (a) Decision problem or objective?
# (b) Lever or levers?
# (c) Ideal RCT design?
# (d) Dependent variable(s) and independent variable(s) in the modeler?
# (e) Why would running RCTs and modeling/optimizing over RCT results be preferable to
# using (observational, non-RCT) “foo” data?
# Approximate suggested length: 1-3 sentences for each sub-question.
# 

# a) The main objective would be to minimize project completion delays (the difference between revised and original completion dates should be minimized).
# b) In order to minimize the difference between revised and original completion dates, some of the levers or variables we can tweak are level of funding and project duration. If the budget expenditure for each project is increased optimally, project managers will be able to use the extra capital to get high-quality equipment and a productive labor force, making the project more time-efficient - so the project can be completed closer to the intended original completion date. If project duration predictions become better, allocation of funds will happen optimally. By having a well-defined, optimal budget that allows for upscale of labour and machinery, project duration estimates will be more accurate (feedback loop) and hence differences between revised and original completion dates will become diminished. 
# c) Provided that a high level of funding is available, we could start many of the same type of projects at the same time (e.g. building roadways), but the levers of revised amount of funding and and predicted project duration would be derived from a joint uniform random distribution function for each of the roadway projects with a defined range for funding ($100k-$100m) and duration (2-12 years), to see how if we randomly assign funding and duration, what effect that would have on the difference between revised and original completion date. And then with this data, we can come up with an optimal estimate on how much expenditure and time-cost should be attributed to roadway projects. 
# d) In this particular model, the dependent variable would be the difference between revised and original completion date - and the independent variables (levers we can control) would be the revised amount of funding and predicted project duration.
# e) This is because the given dataset (foo) consists of many variables that are potentially correlated with each other and we cannot compute the magnitude of this correlation and determine the extent of the causation. For example, high revised funding and longer duration seem to be positively related with project rating - but we don't know the extent to which each of these variables affects the rating. We can establish this relationship more clearly using RCTs treatment is randomly assigned and the effects of variables can be isolated. 
