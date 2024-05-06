rm(list = ls(all = TRUE)) #clear data


library(Hmisc)
library("MASS")


####Reading the data
gillette <- read.csv("Gillette_data.csv", header = T)    		# read csv file and label the data as "data"

#print the names of the columns
names(gillette)

# here our target variable is revenue, and we would want to see a visual peak in to its relation with our dependent variable

year = gillette[,1]
revenue = gillette[,2]
advt = gillette[,3]
lag.revenue = gillette[,4]
margin = gillette[,5]
sales_multiple = gillette[,6]

##  year vs revenue
plot(year, revenue)

## sqrt_advt vs revenue
plot(advt, revenue)

## lag.revenue vs revenue
plot(lag.revenue, revenue)

## Now we will run regression to get 
lm_model <- lm(revenue~advt + lag.revenue -1)
summary(lm_model)


# Now we will use nonlinear regression using initial values of A(theta) and B (theta)

beta<-50.82
lambda<-0.78
rho<-0.06

## A<-beta/2(rho+(1-lambda)+theta)
## B<-beta*theta/2(rho+(1-lambda)+theta)

# nonlinear equation is advt = A *margin + B * sales_multiple + error

# applyng nls to estimate beta

nls_model <- nls(advt ~ beta/(2*(rho+(1-lambda)+b1)) * margin + beta*b1/(2*(rho+(1-lambda)+b1)) * sales_multiple, 
          data = gillette, 
          start = list(b1 = 0.22),
          control = nls.control(maxiter = 1000) , trace= TRUE)  

summary(nls_model)

print("Here the value of the theta estimate is 0.065.")
theta = coefficients(nls_model)

# finding the value of coefficients of margin and sales_multiple (i.e A & B)
A = beta/(2*(rho+(1-lambda)+theta))
B = beta*theta/(2*(rho+(1-lambda)+theta))

print(A)
print(B)


#Storing covariance of nonlinear regression model
nls_cov = vcov(nls_model)

#Storing covariance of linear  model

lm_cov = vcov(lm_model)

#Preparing covariance matrix for montecarlo simulation
montecarlo_cov = matrix(0,nrow=3,ncol=3)

montecarlo_cov[1:2,1:2] = lm_cov
montecarlo_cov[3,3] =  nls_cov

print(montecarlo_cov)

####################################
# Monte Carlo Starts Here
####################################

set.seed(123)
n = 1000

#creating a matrix for A and B
coeff = matrix(0,nrow = n, ncol = 2)
mu <- matrix(c(beta, lambda, theta),nrow=3,ncol=1)
Sigma<-montecarlo_cov

#Loop for monte carlo simulation

b.out = mvrnorm(n = n, mu, Sigma)
A1 = b.out[,1]/(2*(rho+(1-b.out[,2])+b.out[,3]) )
B1 = b.out[,1]*b.out[,3]/(2*(rho+(1-b.out[,2])+b.out[,3]))
coeff = cbind(A1,B1)
#Displaying the coefficients across all the A1 and B1 values
coeff

#Plotting the coefficeints
hist(coeff[,1])
hist(coeff[,2])

#Findingth confidence intervals
CI_A = quantile(coeff[,1],probs=c(0.025,0.5,0.975))
CI_B = quantile(coeff[,2],probs=c(0.025,0.5,0.975))

## confidence interval of A
print(CI_A)

## confidence interval of B
print(CI_B)
