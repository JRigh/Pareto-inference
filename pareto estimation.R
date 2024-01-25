#------------------
# Pareto estimation 
#------------------

library(tidyverse)

# 1 Define a function that returns a Pareto density
dpareto = function(x, theta1, theta2 = 1){
  pareto = (theta1*(theta2^theta1)) / (x^(theta1 + 1))
  return(pareto)
}

# 2. Create plot
x_lower = 1; x_upper = 5

ggplot(data.frame(x = c(x_lower, x_upper)), aes(x = x)) + 
  stat_function(fun = dpareto, args = list(theta1 = 0.5, theta2 = 1), aes(linetype = "Pareto(0.5)"), lwd = 1.2) + 
  stat_function(fun = dpareto, args = list(theta1 = 1, theta2 = 1), aes(linetype = "Pareto(1)"), lwd = 1.2) + 
  stat_function(fun = dpareto, args = list(theta1 = 2, theta2 = 1), aes(linetype = "Pareto(2)"), lwd = 1.2) + 
  stat_function(fun = dpareto, args = list(theta1 = 5, theta2 = 1), aes(linetype = "Pareto(5)"), lwd = 1.2) + 
  scale_linetype_manual(values=c('solid', 'dashed','twodash', 'dotted' ))+ 
  xlim(1, 5) + ylim(0, 5) +
  labs(title = 'Different Pareto Densities',
       subtitle = 'Pa(0.5,1), Pa(1,1), Pa(2,1), Pa(5,1)',
       y="density", x="x") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


# 3. Function that generates random one-parameter Pareto realizations
rpareto <- function(n, theta1, theta2 = 1){
  data <- ((1/(1-runif(n)))^{1/theta1})
  return(data)
}

# 4. Create an artificial dataset of size n = 40
set.seed(2024)
xi = rpareto(40, 4, 1)

# 5. Estimation of the parameter using MLE and MOM

MoM.estimator = mean(xi) / (mean(xi) - 1)
MoM.estimator # [1] 4.006584

ML.estimator = 1 / mean(log(xi))
ML.estimator # [1] 3.886276

# 6. Bootstrapped confidence interval estimation

# Bootstrap
num_bootstraps = 10000
bootstrap_mom = bootstrap_mle = numeric(num_bootstraps)
set.seed(2024)  
for (i in 1:num_bootstraps) {
  resample = sample(xi, replace = TRUE)
  bootstrap_mom[i] = mean(resample) / (mean(resample) - 1)
  bootstrap_mle[i] = 1 / mean(log(resample))
}

# Mean and standard error of the estimators
mean_mom = mean(bootstrap_mom); mean_mle = mean(bootstrap_mle)
standard_error_mom = sd(bootstrap_mom); standard_error_mle = sd(bootstrap_mle)
results = matrix(c(mean_mle, standard_error_mle, mean_mom, standard_error_mom),
                 ncol = 2, byrow = TRUE)
rownames(results) = c('mle', 'mom'); colnames(results) = c('mean', 'se')
results
#         mean        se
# mle 3.972500 0.5943127
# mom 4.098544 0.5458712

# Asymptotic confidence intervals
CI = matrix(cbind(c(mean_mle - qnorm(1-0.05/2)*standard_error_mle, 
                    mean_mle + qnorm(1-0.05/2)*standard_error_mle),
                  c(mean_mom - qnorm(1-0.05/2)*standard_error_mom, 
                    mean_mom + qnorm(1-0.05/2)*standard_error_mom)),
            ncol = 2, byrow = 2)
rownames(CI) = c('mle', 'mom'); colnames(CI) = c('lower bound', 'upper bound')
CI
#     lower bound upper bound
# mle    2.807668    5.137331
# mom    3.028656    5.168432

#----
# end
#----