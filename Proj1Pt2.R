library(ggplot2)
library(xtable)

df <- data.frame(ToothGrowth)

#Part 1 of the project, some basic EDA

hist(df$dose)
hist(df$len)

qplot(dose, len, data=df, color=supp, fill=supp) + theme(legend.position="bottom")
print(qplot(dose, len, data=df, facets = .~supp))
print(qplot(supp, len, data=df, facets = .~dose))

#Part 2 of the project a summary
options(xtable.comment = FALSE)
print(xtable(summary(df), caption="Summary of entire dataset"))
print(xtable(summary(df[df$supp=="OJ",]), caption="Summary of OJ supp"))
print(xtable(summary(df[df$supp=="VC",]), caption="Summary of VC supp"))

#Part 3 of the project
qplot(supp, len, data=df)
qplot(dose, len, data=df)

VC_df <- df[df$supp=="VC",]
OJ_df <- df[df$supp=="OJ",]

#note I am using T-CI and the qt() function rather than using qnorm()

##T-CI by supp
OJ_len_CI <- mean(OJ_df$len) + c(-1,1) * qt(0.975, length(OJ_df$len)-1) + 
              sd(OJ_df$len) / sqrt(length(OJ_df$len))
VC_len_CI <- mean(VC_df$len) + c(-1,1) * qt(0.975, length(VC_df$len)-1) + 
              sd(VC_df$len) / sqrt(length(VC_df$len))

CI_supp_df <- t(data.frame(OJ_len_CI, VC_len_CI))
colnames(CI_supp_df) <- c("lower limit", "upper limit")
xtable(CI_supp_df, caption="Summary of T-CI lengths by supp")

##T-CI by dose
dose1_df <- df[df$dose==0.5,]
dose2_df <- df[df$dose==1,]
dose3_df <- df[df$dose==2,]

dose1_len_CI <- mean(dose1_df$len) + c(-1,1) * qt(0.975, length(dose1_df)-1) + 
  sd(dose1_df$len) / sqrt(length(dose1_df$len))
dose2_len_CI <- mean(dose2_df$len) + c(-1,1) * qt(0.975, length(dose2_df)-1) + 
  sd(dose2_df$len) / sqrt(length(dose2_df$len))
dose3_len_CI <- mean(dose3_df$len) + c(-1,1) * qt(0.975, length(dose3_df)-1) + 
  sd(dose3_df$len) / sqrt(length(dose3_df$len))

CI_dose_df <- t(data.frame(dose1_len_CI, dose2_len_CI, dose3_len_CI))
colnames(CI_dose_df) <- c("lower limit", "upper limit")
xtable(CI_dose_df, caption="Summary of T-CI lengths by dose")

#taking a look at the density of the tooth growth broken down by dose
plot_dose <- ggplot(df, aes(x=df$len, fill=factor(df$dose))) +
  geom_density(alpha=0.2, binwidth=1) + 
  theme(legend.position="bottom")
print(plot_dose)

#taking a look at the density of the tooth growth broken down by supp
plot_supp <- ggplot(df, aes(x=df$len, fill=factor(df$supp))) + 
  geom_density(alpha=0.2, binwidth=1) + 
  theme(legend.position="bottom")
print(plot_supp)


## need to create hypothesis tests
t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data=df)

t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data= subset(df, dose %in% c(0.5,1)))
t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data= subset(df, dose %in% c(1,2)))
t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data= subset(df, dose %in% c(0.5,2)))

## conclusions/assumptions




