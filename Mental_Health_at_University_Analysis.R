data <- read.csv("Student Mental Health.csv") # Let's load our data
install.packages("Hmisc")
library(Hmisc)

head(data) # Quick peek

# Getting a feel for it
describe(data) 
summary(data) 

# Need to clean up GPA Column unfortunately - remove extra space from the GPA column
data$What.is.your.CGPA. <- gsub("3.50 - 4.00 ", "3.50 - 4.00", data$What.is.your.CGPA.)

unique(data$What.is.your.CGPA.) # Now fixed

# Year of study column is also pretty messy as well:
data$Your.current.year.of.Study <- gsub("(?i)year", "Year", data$Your.current.year.of.Study)



# Let's look at the general depression rate
Depressed = subset(data, Do.you.have.Depression.=="Yes")
Not_Depressed = subset(data, Do.you.have.Depression. =="No")

nrow(Depressed) / nrow(data) #34.5% are depressed

# Any correlation between current year of study and depression?
library(dplyr)
library(ggplot2)

year_vs_depression <- data %>%
  select(Your.current.year.of.Study, Do.you.have.Depression.) %>%
  mutate(Do.you.have.Depression. = factor(Do.you.have.Depression.))

ggplot(year_vs_depression, aes(x = factor(Your.current.year.of.Study), fill = Do.you.have.Depression.)) +
  geom_bar(position = "fill") +
  labs(title = "Depression by Year of Study",
       x = "Year of Study",
       y = "Proportion",
       fill = "Depression") +
  theme_minimal()


ggplot(year_vs_depression, aes(x = factor(Your.current.year.of.Study), fill = Do.you.have.Depression.)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Depression by Year of Study",
       x = "Year of Study",
       y = "Count",
       fill = "Depression") +
  theme_minimal()

# If anything, it looks like our first years are fairing well mentally.
# What about for anxiety?

ggplot(data, aes(x = Your.current.year.of.Study, fill = Do.you.have.Anxiety.)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Relationship between Year of Study and Anxiety",
       x = "Year of Study",
       y = "Proportion",
       fill = "Anxiety") +
  theme_minimal()

# Year of study and panic attacks?
ggplot(data, aes(x = Your.current.year.of.Study, fill = Do.you.have.Panic.attack.)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Relationship between Year of Study and Panic Attacks",
       x = "Year of Study",
       y = "Proportion",
       fill = "Panic Attacks") +
  theme_minimal()

# No real major trends we can draw from these visuals, what about the others?
# We'll primarily focus on depression to keep things brief/simple

# Correlation between course and depression? - Oof, We've got a hefty clean up job to do here
# Let's start by grouping all the course entries into general groups
data$What.is.your.course. <- trimws(tolower(data$What.is.your.course.))

group_courses <- function(course) {
  # Engineering related courses
  if (grepl("engineer|engin|it|comput|software", course)) {
    return("Engineering")
  }
  # Islamic related courses
  if (grepl("islam|irkh|usuluddin|fiqh|fatwa|pendidikan", course)) {
    return("Islamic Education")
  }
  # Business related courses
  if (grepl("business|bcs|bank|accounting|banking", course)) {
    return("Business Studies")
  }
  # Law-related courses
  if (grepl("law", course)) {
    return("Law")
  }
  # Medical-related courses
  if (grepl("nurs|radiograph|biomed|pharm|medic|mhsc", course)) {
    return("Medical/Health Sciences")
  }
  # Social Sciences-related courses
  if (grepl("psychol|human resources|econ|biotech|com", course)) {
    return("Social Sciences")
  }
  # Other courses
  return("Other")
}

data$course_group <- sapply(data$What.is.your.course., group_courses)

# Print first few rows to check
head(data) # Looks like we're all good

ggplot(data, aes(x = course_group, fill = Do.you.have.Depression.)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Relationship between Course Group & Depression",
       x = "Course Group",
       y = "Proportion",
       fill = "Depression") +
  theme_minimal()


# Hmm, looks like there may be some sort of relation here. Let's run a statistical test to be safe


contingency_table <- table(data$course_group, data$Do.you.have.Depression.)

# is there a statistical relationship between course group and depression?
print(fisher.test(contingency_table)) 
# P-Value suggests NOT ENOUGH EVIDENCE TO SUGGEST A RELATION BETWEEN THESE GROUPS


# Any relationship between those in relationships and depression?
data$Marital.status <- gsub("No", "Single", data$Marital.status)
data$Marital.status <- gsub("Yes", "Relationship", data$Marital.status)

ggplot(data, aes(x = Marital.status, fill = Do.you.have.Depression.)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Relationship between Marital Status and Depression",
       x = "Marital Status",
       y = "Proportion",
       fill = "Depression") +
  theme_minimal()

ggplot(data, aes(x = Marital.status, fill = Do.you.have.Depression.)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Relationship between Marital Status and Depression",
       x = "Marital Status",
       y = "Proportion",
       fill = "Depression") +
  theme_minimal()

# Is there a statistical relationship here?
# Create contingency table
contingency_table_2 <- table(data$Marital.status, data$Do.you.have.Depression.)
print(fisher.test(contingency_table_2))
# It seems there is a statistical significance here * Those who are single are better off on the depression front


# Correlation between Gender and depression?

ggplot(data, aes(x = Choose.your.gender, fill = Do.you.have.Depression.)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Relationship between Gender and Depression",
       x = "Gender",
       y = "Proportion",
       fill = "Depression") +
  theme_minimal()

ggplot(data, aes(x = Choose.your.gender, fill = Do.you.have.Depression.)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Relationship between Gender and Depression",
       x = "Gender",
       y = "Proportion",
       fill = "Depression") +
  theme_minimal()


# Create contingency table 3
contingency_table_3 <- table(data$Choose.your.gender, data$Do.you.have.Depression.)
print(fisher.test(contingency_table_3)) # No statistical significance as per p-value



# Who actually sought specialist help?

contingency_table_4 <- table(data$Did.you.seek.any.specialist.for.a.treatment., data$Do.you.have.Depression.)
print(fisher.test(contingency_table_4)) # Clear statistical significance here <- obvious action point

ggplot(data, aes(x = Did.you.seek.any.specialist.for.a.treatment., fill = Do.you.have.Depression.)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Relationship between Seeking Specialist Help and Depression",
       x = "Seeking Specialist Help",
       y = "Proportion",
       fill = "Depression") +
  theme_minimal()



# Course group and anxiety? 

barplot(prop_anxiety, 
        main = "Proportion of Anxiety Across Different Course Groups",
        xlab = "Course Group", ylab = "Proportion of Anxiety",
        col = "lightblue", ylim = c(0, max(prop_anxiety) + 0.1),
        cex.names = 0.8, las = 2)
text(x = 1:length(prop_anxiety), y = prop_anxiety + 0.02, labels = round(prop_anxiety, 2), pos = 3, cex = 0.8)


contingency_table_4 <- table(data$What.is.your.course., data$Do.you.have.Anxiety.)
print(fisher.test(contingency_table_4)) # Looks like there is a relationship as per the p-value



