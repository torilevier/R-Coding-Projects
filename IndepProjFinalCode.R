#Run this chunk at beginning of Sessions (when you first open R studio)
library(tidyverse)
library(afex)
library(psych)
library(emmeans)

##Import Data## - DONT FORGET TO SET YOUR WORKING DIRECTORY
df <- read.csv(file.choose())
df <- df[3:nrow(df),]
write.csv(file = "df.csv", df, row.names = F)
df <- read.csv("df.csv")

#Filter out participants who didn't finish demographics (gender)
df <- df %>% filter(!is.na(gender))
df <- df %>% filter(!is.na(age))

#Renaming variable names to be more concise
df$ptime1 <- df$ptime1_Page.Submit
df$ptime2 <- df$ptime2_Page.Submit
df$ptime3 <- df$ptime3_Page.Submit
df$ptime4 <- df$ptime4_Page.Submit
df$ptime5 <- df$ptime5_Page.Submit

#Renaming variable names to be more concise
df$ntime1 <- df$ntime1_Page.Submit
df$ntime2 <- df$ntime2_Page.Submit
df$ntime3 <- df$ntime3_Page.Submit
df$ntime4 <- df$ntime4_Page.Submit
df$ntime5 <- df$ntime5_Page.Submit

df$id <- rownames(df)

#Selecting rows that we will keep
df <- df %>% select(id, age:gender, affectprime:framecond, phelp1, phelp2, phelp3, 
                    phelp4, phelp5, nhelp1, nhelp2, nhelp3, nhelp4, nhelp5, 
                    sreffort:emot, ptime1:ntime5)

# Getting the means of all altruism values
df <- df %>% mutate(phelp_scale = rowMeans(across(contains("phelp"))))
df <- df %>% mutate(nhelp_scale = rowMeans(across(contains("nhelp"))))

# Getting the means of all cogntive effort values (measured by time)
df <- df %>% mutate(ptime_scale = rowMeans(across(contains("ptime"))))
df <- df %>% mutate(ntime_scale = rowMeans(across(contains("ntime"))))

#If else statement 
df$altruism <- ifelse(df$framecond == "negative",df$nhelp_scale, df$phelp_scale)
df$cogEffort <- ifelse(df$framecond == "negative",df$ntime_scale, df$ptime_scale)

# Mutating data to modify the naming for the two conditions/IVs
# df <- df %>%
#   mutate(
#     affectprime = recode(affectprime,
#                       "negative" = '0',
#                       "positive" = '1',
#                       .default = NA_character_),
#     framecond = recode(framecond,
#                       "negative" = '0',
#                       "positive" = '1',
#                       .default = NA_character_)
#  )





#Not necessary
# df <- df %>%
#   mutate(
#     affectprime = recode(affectprime,
#                          '0' = "negative",
#                          '1' = "positive",
#                          .default = NA_character_),
#     framecond = recode(framecond,
#                        '0' = "negative",
#                        '1' = "positive",
#                        .default = NA_character_)
#   )

#Conduct 2 x 2 ANOVA
# a1 <- aov_ez( data = df,
#               id = "id",
#               dv = "altruism",
#               between = c("affectprime", "framecond"))
# 
# a1
# 
# a2 <- aov_ez( data = df,
#               id = "id",
#               dv = "cogEffort",
#               between = c("affectprime", "framecond"))
# 
# a2

#Filter out people who spent 
#more than 3 SDs above the mean on time

df_filtered <- df %>% filter(cogEffort < 3*sd(cogEffort))

a1 <- aov_ez( data = df_filtered,
              id = "id",
              dv = "altruism",
              between = c("affectprime", "framecond"))
a1

a2 <- aov_ez( data = df_filtered,
              id = "id",
              dv = "cogEffort",
              between = c("affectprime", "framecond"))
a2

df_filtered <- df_filtered %>%
mutate(
  gender = recode(gender,
                   '1' = "male",
                   '2' = "female",
                   '3' = "nonbinary",
                   .default = NA_character_)
)

# Filtering gender, tells us how much of each gender participated
table(df_filtered$gender)

# Filtering to get proportions, dividing by number of responses
table(df_filtered$gender) / length(df_filtered$gender)

describe(df_filtered$cogEffort)

emmeans(a1, ~framecond*affectprime)

emmeans(a2, ~framecond*affectprime)

describe(df_filtered$altruism)

# df_final <- df_filtered %>% filter (!is.na(altruism))
# df_final <- df_filtered %>% filter (!is.na(cogEffort))
# 
# a1 <- aov_ez( data = df_final,
#               id = "id",
#               dv = "altruism",
#               between = c("affectprime", "framecond"))
# a1
# 
# a2 <- aov_ez( data = df_final,
#               id = "id",
#               dv = "cogEffort",
#               between = c("affectprime", "framecond"))
# a2

# calculate the means and standard error of the conditions
# We can find these data from the Table of EMMs for interaction
emmeans(a1, ~framecond*affectprime)
emmeans(a2, ~framecond*affectprime)

#Plot for Altruism
# set up the x-axis with the first factor (two bars for each of the two conditions) 
condition.x <- c(rep("Negative", 2), rep("Positive", 2)) 

# set up the second factor (two conditions are differentiated by two colors)
condition.color <- rep(c("Negative", "Positive"), 2)

# create a new variable with the means of four conditions (make sure you make it with the correct order)
means.4Group <- c(4.25, 4.95, 4.51, 5.08) #the order is primepos + framepos, primepos + frameneg, primeneg + framepos, primeneg + frameneg (reversed it for now)

# create a new variable with the standard error (SE) of four conditions (keep the correct order)
se.4Group <- c(0.124, 0.142, 0.142, 0.129) #same order as the means (reversed as a test)

# create a new dataframe and add all the new variables in it, so that we will not mess up our original dataset
data.plot <- data.frame(condition.x, condition.color, means.4Group, se.4Group)

data.plot #check whether the conditions and data are matching

plot1 <- ggplot(data.plot, aes(fill=condition.color, y=means.4Group, x=condition.x)) + 
  geom_bar(position="dodge", color="black", stat="identity") + 
  geom_errorbar(aes(ymax=means.4Group + se.4Group, ymin=means.4Group - se.4Group), width=.1, position=position_dodge(.9)) + labs(fill="Framing Condition", title = "Positive Framing of Questions Led to Greater \nLevels of Altruism measured by Willingness to Help", x = "Priming Type", y = "Willingness to Help") +
  scale_fill_manual(values=c("#99FFFF", "#FFFF99", "#99FFFF", "#FFFF99")) #if you are not satisfied with the default color, you can put the name of the color you choose here

plot1

show(plot1)
plot(plot1)

# Plot for Cognitive Effort
# set up the x-axis with the first factor (two bars for each of the two conditions) 
condition.x <- c(rep("Positive", 2), rep("Negative", 2))

# set up the second factor (two conditions are differentiated by two colors)
condition.color <- rep(c("Positive", "Negative"))

# create a new variable with the means of four conditions (make sure you make it with the correct order)
means.4Group <- c(13.3, 12.2, 11.9, 19.1) #the order is primepos + framepos, primepos + frameneg, primeneg + framepos, primeneg + frameneg

# create a new variable with the standard error (SE) of four conditions (keep the correct order)
se.4Group <- c(1.93, 2.12, 2.12, 1.86) #same order as the means

# create a new dataframe and add all the new variables in it, so that we will not mess up our original dataset
data.plot <- data.frame(condition.x, condition.color, means.4Group, se.4Group)

data.plot #check whether the conditions and data are matching

plot2 <- ggplot(data.plot, aes(fill=condition.color, y=means.4Group, x=condition.x))+ 
  geom_bar(position="dodge", color="black", stat="identity")+ 
  geom_errorbar(aes(ymin=means.4Group - se.4Group, ymax=means.4Group + se.4Group), width=.1, position=position_dodge(.9)) + labs(fill="Framing Condition", title = "The Interaction of Negative Priming and Negative \nFraming had the Strongest Effect on Cognitive Effort", x = "Priming Type", y = "Time Spent per Question") +
  scale_fill_manual(values=c("#FFFF99", "#99FFFF", "#99FFFF", "#FFFF99")) #if you are not satisfied with the default color, you can put the name of the color you choose here

plot2

describe(df_filtered$age)

M = 23.27, SD = 6.34
