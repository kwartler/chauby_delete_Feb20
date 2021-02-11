#' Author: Ted Kwartler
#' Date: 7-14-2018
#' Purpose: Use a logistic regression to help customer service operations
#' 

# Cu Satisfaction Survey
#Question ID	Question	Scale (Low to High)
#Q1:	Overall, how satisfied are you with the quality of service?	1 - 10
#Q2:	How friendly was the agent you spoke with?	1 - 10
#Q3:	How well did the agent understand your issue?	1 - 10
#Q4:	How timely did we answer your initial inquiry?	1 - 10
#Q5:	How satisfied are you with the product variety?	1 - 10
#Q6:	How satisfied are you with the resolution of your inquiry?	1 - 10
#Q7:	How satisfied are you with the pricing of the product you chose?	1 - 10

# Wd
setwd("~/Desktop/chauby_delete_Dec10/C_Sum of Beta")

# libs
library(ggplot2)
library(ggthemes)

# You can build a model to understand selection bias:
# Using xVars as predictors, build an explanatory model to predict the probability of a customer completing a survey.  What characteristics of a contact impact the c-sat survey completion?
#xVars <- c('talkTimeSec', 'holdTimeSec','wrapUpTimeSec','dispositionCodes',
#           'preCallWaitSec','socialMediaClout')
# Use a cuttoff of 0.42 
#contacts  <- read.csv("CuSatCase_oversampledResp_10K_contacts.csv")

# Or below you can build a model to understand the drivers of good customer service interactions

# Data
responses <- read.csv('CuSatCase_SurveyResponses.csv')

# Using 'responses' change  q1OverallSat to be a binary outcome where values 9 or 10 are considered a success. 
responses$success <- ifelse(responses$q1OverallSat>=10,1,0)

# Fit a model with the other question variables as predictors
fit <- glm(success ~., data = responses[,7:13], family ='binomial')

# Extract the coefficients and drop the intercept term
betas <- coefficients(fit)
betas <- betas[2:length(betas)]

# Perform a sum of beta calc, this represents how much the specific attibute can affect the outcome relative to all variables ability to affect the outcome
cuImpacts<- abs(betas) / sum(abs(betas))
cuImpacts

# Now calculate the amount of time the customer service agents are performing well (either 9 or 10) in each of the attributes as a pct of all opportunities
PctofAllSurveys9_10 <- vector()
for (i in 1:length(betas)){
  var <- names(betas)[i]
  x <- sum(table(responses[,var])[9:10]) / sum(table(responses[,var])[1:10])
  PctofAllSurveys9_10[[var]]<- x
}
PctofAllSurveys9_10

# Organize
vizDF<-data.frame(questions        = names(betas),
                  impact           =cuImpacts, 
                  pct_interactions = PctofAllSurveys9_10)
rownames(vizDF) <- NULL
vizDF

impactLine          <- mean(vizDF$impact)
pctInteractionsLine <- mean(vizDF$pct_interactions)

# Create a plot for operations to examine
p <- ggplot(vizDF, aes(x = pct_interactions, y = impact)) + 
  geom_point(size = 1, color = 'darkred') +
  geom_text(aes(label = questions), angle = 15, vjust = 0, size = 3, hjust = -0.05) +
  geom_hline(aes(yintercept = impactLine), color = 'blue', size = 1, alpha = 0.75) +
  geom_vline(aes(xintercept = pctInteractionsLine), color = 'blue', size = 1, alpha = 0.75) +
  theme_hc() +theme(legend.position="none") 
p

# Add annotations to make interpretation easier
annotations <- data.frame(xpos = c(-Inf,-Inf,Inf,Inf),
                          ypos =  c(-Inf, Inf,-Inf,Inf),
                          annotateText = c("Low Impact Opportunities",
                                           "High Impact Opportunities",
                                           "Core Competencies","Continue to Support"),
                          hjustvar = c(-.05,-.05,1.05,1.05), vjustvar = c(-1,2,-1,2))

p + geom_text(data = annotations, 
              aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar, label=annotateText),
              color='blue', alpha = 0.50, size=2.5)

# What should the operational leaders continue to coach?
# What is the organization good at but doesn't have much impact on survey results?
# What is the next item agents should be coached on because it has a higher impact and is an opportunity area?

# End