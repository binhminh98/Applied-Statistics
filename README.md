# :star2: Applied Statistics :star2: #
## :chart_with_upwards_trend: Applied Statistics Courseworks :chart_with_upwards_trend: ##

Using R to perform regression analysis (linear and logistic), hypothesis testing, Analysis of Variance (one-way and two-way ANOVAs), Analysis of Covariance (ANCOVA), and Survival Analysis on multiple datasets.

Example:

### Question: The data in the file OliveYield.csv contains the olive oil yield (kg/season) from some olive trees. The Height (m) of the trees is also known, which are also grouped into a HeightGroup variable (low and high). The Type of tree (thought to be related to yield) is also known for each tree tested (Arbequina, Koroneiki and Maurino). Run a two-way ANOVA to look for significant effects, using HeightGroup and Type as factors, and provide an interpretation of what you discover. ###

### Step 1: Check the number of observations is balanced. ###

![image](https://user-images.githubusercontent.com/100490285/178085004-5136aa54-45e4-42cd-835b-f5aaaa1353a1.png)

:arrow_right: This dataset is balanced.

### Step 2: Set contrasts (so parameters sum to zero). And run two-way ANOVA. ###

![image](https://user-images.githubusercontent.com/100490285/178085040-1eca2121-ea4b-467d-ae18-c560d9f394d2.png)

### Step 3: Calculate p-value for height_group2: low and type3: Maurino ###

As the parameters sum to zero:

![image](https://user-images.githubusercontent.com/100490285/178085059-30b3dc04-06d5-4504-b3de-88abc1dc6c89.png)

:arrow_right: p-value = 0.001597088.

![image](https://user-images.githubusercontent.com/100490285/178085069-fba00b9e-e11c-4e1f-90df-015899ffe78a.png)

:arrow_right: p-value = 1.096779e-05.

Interpretation:

* There are overall effects of height groups and tree types on olive oil yield as the omnibus test p-value is 9.873e-05 < 0.05 indicates high significance
* Height groups are significant (p-value = 0.001597 < 0.05) -> they have effects on olive oil yield.
* Tree types are significant (p-value = 3.153e-05 < 0.05) -> they have effects on olive oil yield.
* Height groups and tree types interaction is not significant (p-value = 0.750460 > 0.05-> it has no effect on olive oil yield.
* Olive oil yield depends on both height groups: high and low with p-value = 0.001597 < 0.05.
* Olive oil yield depends on type1: Arbequina (p = 0.000195 < 0.05) and type3: Maurino (p = 1.096779e-05 < 0.05), while it does not depend on type2: Koroneiki (p = 0.079571 > 0.05).
* R-squared: The model explains 85.81% variances of the data -> overall a good model.








