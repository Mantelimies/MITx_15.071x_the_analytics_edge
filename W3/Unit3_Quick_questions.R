#Suppose the coefficients of a logistic regression model with two independent variables are as follows:
#And we have an observation with the following values for the independent variables:
#What is the value of the Logit for this observation? Recall that the Logit is log(Odds).


logit <- (-1.5+1*3+5*-.5)
logit

#What is the value of the Odds for this observation? Note that you can compute e^x, for some number x, in your R console by typing exp(x). The function exp() computes the exponential of its argument.

odds <- exp(logit)
odds


#What is the value of P(y = 1) for this observation?

e <- exp(1)
p = 1/(1+e^-logit)
p
