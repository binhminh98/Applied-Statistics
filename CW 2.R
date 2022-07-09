# 1. The ConcHard:csv file on the Blackboard provides data on the compressive strength
# of Concrete Hardness (PSI) for a company producing concrete slabs under a range of
# industrial process Types A-E, as well as control data for the currently sold type.

# (a) Load the data into R, and provide a graphical display to demonstrate concrete hardness variation 
# across the product types, and provide an initial interpretation.

path <- file.path('D:/UEA/Applied Statistics/CW2/ConcHard.csv')
concrete_data = read.csv(path,header=T)
concrete_data

types <- factor(concrete_data$Type)

# Rearrange control group to be the first group
types<-factor(types,levels=c("Control","A","B","C","D","E"))

y <- concrete_data$Hardness

tapply(y,types,mean)
tapply(y,types,length)

boxplot(
  y~types, 
  data = concrete_data, 
  main="Concrete hardness among processing types",
  xlab="Type", 
  ylab="Hardness"
)

points(types, y)

# Group A seems to have an outlier that falls outside of boxplot and looks  not so different comparing 
# to the Control group. Boxplots of Group B,D looks higher in general, while C,E is lower than 
# Control Group. The spread of data in groups: Control and D look normal because the median is in 
# the middle. Group A, B, C, E looks a bit skewed because the median is higher or lower.

# (b) Set up the null and alternative hypotheses and carry out a one way ANOVA to determine 
# if there are any significant differences in the types of concrete produced.

# An experiment comparing the effects of 5 concrete types. There are no other types
# for the condition of interest at the moment -> fixed model.

# Are the effects of different type of concrete real?
# Ho: m1=m2=...=m6 (Group means are equal)
# H1: Exist i,j where mi != mj (Exist 2 groups that have different means)

concrete.lm = lm(y ~ types)

anova(concrete.lm)

summary(concrete.lm)

# Omnibus significant p-value: 8.583e-10 => there are overall differences between concreted produced using different techniques.

# (c) Check the assumptions of your analysis and comment on the results.

# Independence: We assume the concrete experiments were independently done -> cannot have 2 concrete related to each other 

# Normality The data looked symmetric and normal and only one outlier in type A
# (more formal test of normality could have been done).

plot(concrete.lm, which=2)

# In Q-Q plot, As all the points fall approximately along this reference line, we can assume normality.

# Shapiro-Wilk test:

# Ho: data come from a normal distribution
# H1: data do not come from a normal distribution

shapiro.test(x = concrete.lm$residuals)

# p > 0.05: then the null hypothesis cannot be rejected (i.e. the variable MAY BE normally distributed).

# Equal group variances The magnitude of the residuals does not vary over
# the groups in a statistically significant manner, as judged by the omnibus
# Levene F-test.

# The residuals look quite normally distributed because the data is reasonably central, except for group A.
plot(concrete.lm,which=1)

# Run ANOVA on residuals
# Ho: m1=m2=...=m6 (Group means are equal)
# H1: Exist i,j where mi != mj (Exist 2 groups that have different means)

tmp <- lm(abs(concrete.lm$residuals)~types)
anova(tmp)
summary(tmp)

# The Levene test is defined as:
# H0:	??21=??22=.=??2k
# H1:	??2i?????2j for at least one pair (i,j).

require(lawstat)
levene.test(abs(concrete.lm$residuals),types,location = "mean")

# Omnibus test, and levene test both are not significant => variance does not differ from types

# The assumptions of the ANOVA can be considered reasonable for this dataset.

# (d) Implement comparison analyses for the following questions. (i) Are any concrete
# types significantly harder than the control? (ii) Are types B and D significantly different?

# B and D is significantly harder than Control
summary(concrete.lm)

# B and D is not significantly different
pairwise.t.test(y,types,p.adj="none")

# (e) For all significance levels analysed, explicitly implement (i) Bonferonni and (ii) Holm
# correction for multiple testing. Comment on any differences to the conclusions for
# each case, and any differences observed between (i) and (ii).

# i. Bonferonni
pairwise.t.test(y,types,p.adj="bonferroni")

# ii. Holm
pairwise.t.test(y,types,p.adj="holm")

# A is not significantlly different from B and D in Bonferroni, 
# while Holm is the same as the original test by lowering the significant level.

# 2. The data in the file OliveYield.csv contains the olive oil yield (kg/season) from some
# olive trees. The Height (m) of the trees are also known, which are also grouped into a
# HeightGroup variable (low and high). The Type of tree (thought to be related to yield) is
# also known for each tree tested (Arbequina, Koroneiki and Maurino). You may presume
# the assumptions for ANOVAs and ANCOVAs are satisfied in the following questions.

# Load data
path <- file.path('D:/UEA/Applied Statistics/CW2/OliveYield.csv')
olive_data = read.csv(path,header=T)
olive_data

# (a) Visualize the data and interpret what you see.
yield = olive_data$Yield..kg.season.
height = olive_data$Height..m.
height_group = factor(olive_data$HeightGroup)
type = factor(olive_data$Type)

# height_group to yield
boxplot(
  yield~height_group, 
  data = olive_data, 
  main="Olive oil yield among height groups",
  xlab="Group", 
  ylab="Oil Yield")

points(height_group, yield)

# high tree groups looks like they produce more olive oil

# type to yield
boxplot(
  yield~type, 
  data = olive_data, 
  main="Olive oil yield among types of tree",
  xlab="Tree types", 
  ylab="Oil Yield")

points(type, yield)

# Maurino tree types looks like it produces more olive oil than the other, and Koroneiki is a bit higher than Arbequina

# interaction plot of height_group and type
interaction.plot(height_group,type,yield)

# Tree height effects for Maurino looks a bit different (steeper) to tree height effects of other two

# scatterplot of height_group versus height
plot(yield ~ height, type = "n")
points(height[height_group=="low"], yield[height_group=="low"], pch="low", col=2)
points(height[height_group=="high"], yield[height_group=="high"], pch="high", col=4)

# a strong increasing relationship between the covariate (height) and the response
# (yield) within each group, and the slopes in the two groups look similar.

# scatterplot of type versus height
plot(yield ~ height, type = "n")
points(height[type=="Arbequina"], yield[type=="Arbequina"], pch="Arbequina", col=2)
points(height[type=="Koroneiki"], yield[type=="Koroneiki"], pch="Koroneiki", col=4)
points(height[type=="Maurino"], yield[type=="Maurino"], pch="Maurino", col=6)

# The slopes in the two groups: K and A look similar, while M is a little bit steeper. -> assumption of constant gradient

# (b) Run a two way ANOVA to look for significant effects, using HeightGroup and Type
# as factors, and provide an interpretation of what you discover.

# Check the number of observation is balanced -> balanced
table(height_group,type)

# Set contrasts (so parameters sum to zero)
contrasts(height_group)<-contr.sum
contrasts(type)<-contr.sum

t1<-lm(yield~height_group*type, data=olive_data)
anova(t1)
summary(t1)

# type1: Arbequina and type3: Maurino (p=1.096779e-05) is significant while type2: Koroneiki is not

(1.94111+0.70444)/0.36778
-1.05444/0.26006

pt(q = 7.193295, df = 12, lower.tail = F)*2
pt(q = -4.054603, df = 12, lower.tail = T)*2

# Omnibus significant => there are overall effect
# Height group is significant -> has effect on olive oil yield
# Type is significant -> has effect on olive oil yield
# Height group and type interaction is not significant -> has no effect on olive oil yield
# R-squared: The model explains 85.81% variances of the data -> quite good

# (c) Run an ANCOVA using Type factor and Height covariate to look for significant effects, 
# and provide an interpretation of what you discover.

# Center the covariate
height_centered <- height - mean(height)
olive_ancova_lm <- lm(yield ~ height_centered + type)
anova(olive_ancova_lm)
summary(olive_ancova_lm)

# Height is significant -> has overall effect on olive oil yield
# Type is significant -> has overall effect on olive oil yield
# type1: Arbequina and type3: Maurino (p=6.535963e-07) is significant while type2: Koroneiki is not

(1.81201+0.59863)/0.28296

pt(q = 8.519367, df = 14, lower.tail = F)*2

# (d) Comment with reasons on whether Height is a suitable covariate for the ANCOVA.

# Verify the means as shown in the table
Xbari. <- tapply(height, type, mean)
Xbari.

# The covariate (Height) should (i) not be correlated with the factors (type) and (ii) be 
# correlated with the feature of interest (yield). Look at the plot, it seems like height and yield are 
# possitively correlated. And also calculated r = 0.6013225 > 0 -> so (ii) is OK.

# For (i), we might expect that the type of tree will be unrelated to height. 
# Looking at the mean height by types of trees, there may be some connection (Maurino seems to be higher),
# but the effect does not look too large.
plot(
  yield~height,	
  main="Scatterplot of height and yield",	
  xlab="height",	
  ylab="yield"
  )

abline(lm(yield~height))

lm(yield~height)
a1 = 0.6215

lm(height~yield)
a2 = 0.5818

r = sqrt(a1*a2)
r

# (e) Comment with reasons on which of (b) or (c) is the better approach.
# R-squared: 90.26% -> higher than 2 ways ANOVA above -> more variance explained
# Omnibus significant (overall effect) with lower p-value: 2.508e-07 than 2 ways ANOVA above
# MSE: 0.8464 better than 1.271 -> 1.5 times better
1.103/0.8464

# => (c) is a better approach in general

# 3. The duration of a staphylococcus infection (days) for patients are recorded in a clinical 
# trial comparing two new antibiotics (Entromyacin and Selovyacin) over a period of 2
# weeks. Censoring information is also available (0=censored, 1=cured). The data can be
# found in StaphSurv.csv

# Load data and packages
path <- file.path('D:/UEA/Applied Statistics/CW2/StaphSurv.csv')
drug_data = read.csv(path,header=T)
drug_data

drug_data$Drug = factor(drug_data$Drug)
library(survival)

# (a) Produce Kaplan Meier survival plot(s) for the data and provide an initial interpretation
fit <- survfit(Surv(Time, Status) ~ Drug,data = drug_data)

summary(fit)

plot(
  fit,
  main="Censored by Drug",
  xlab="Time (days)", 
  ylab="S(t)", 
  mark.time=TRUE, 
  conf.int=FALSE,
  col = c(1,2)
  )

plot(
  fit,
  main="Censored by Drug",
  xlab="Time (days)", 
  ylab="S(t)", 
  mark.time=TRUE, 
  conf.int=TRUE,
  col = c(1,2)
  )

# Add legend
legend("bottomleft", legend = levels(drug_data$Drug),lty = 1, col = c(1,2))

# (b) Do a log-rank test to investigate the significance of differences between the two
# treatments, and interpret the results

# Ho: no difference in hazard rates between groups.
# H1: there is a difference.

survdiff(formula = Surv(Time, Status) ~ Drug, data = drug_data,rho = 0)

# A p value of 0.03 provides strong evidence the two survival curves
# are different...which is better?

# The Selovyacin group (red) has a lower survival curve indicating that it is
# (significantly) better for those with the drug Selovyacin (overall shorter duration of infection).

# log-rank test is considered to be powerful as the survival curves are not crossing each other.