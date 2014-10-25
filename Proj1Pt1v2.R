library(ggplot2)

##initializing parameters
lambda <- 0.2
nosim <- 1000    #of sets to repeat
mean <- 1/lambda
sd <- 1/lambda

##simulation code

#using set.seed(346) in order to reproduce results
set.seed(346)

#taking the mean of rexp(40,lambda) nosim times, or in this case 1000 times
means <- replicate(nosim, mean(rexp(40,lambda)))     

#evaluating the std. dev. of rexp(40,lambda) nosim times, or in this case 1000 times
set.seed(346)
sim_sd <- replicate(nosim, sd(rexp(40,lambda)))

## part 1,2 of project

est_mean <- mean(means) 
cat("Simulations distribution refers to the 40 exponentials repeated",nosim,"times", "\n")
cat("\n")
cat("Simulations distribution centered at:", round(est_mean,3),"\n")
cat("Theoretical distribution centered at:", mean, "\n")

cat("Simulations variance:", round(var(means),3),"\n")
cat("Theoretical variance:", ((sd^2)/40),"\n")

## part 3 of project

#creating a theoretical normal distribution with the same parameters
theo <- data.frame(means = rnorm(nosim, mean=mean, sd=sd))
theo$category <- 'Theoretical value'

#converting the simulated distribution into a dataframe
sim_df <- data.frame(means)
sim_df$category <- 'Simulated value'

#combining the theoretical and simulated distributions into a single dataframe for plotting
plot_df <- rbind(theo, sim_df)

#code to plot and overlay histograms of both theoretical and simulated distributions
plot <- ggplot(plot_df, aes(means, fill = category)) + 
        geom_histogram(alpha=0.3, binwidth=.25) +
        xlim(-5,15)

print(plot)

## part 4 of project

#adding the sd for each 1000 observation to sim_df, and calculating the corresponding CI limits
sim_df$sd <- sim_sd
sim_df$ll <- sim_df$means - qnorm(0.975) * (sim_df$sd)/sqrt(40)
sim_df$ul <- sim_df$means + qnorm(0.975) * (sim_df$sd)/sqrt(40)
sim_df$coverage <- (est_mean > sim_df$ll & est_mean < sim_df$ul)

coverage_eval <- sum(sim_df$coverage==TRUE) / length(sim_df$coverage)

cat("Evaluation of the 95% CI coverage:", coverage_eval*100, "%", "of observations fall within the 95% CI")
