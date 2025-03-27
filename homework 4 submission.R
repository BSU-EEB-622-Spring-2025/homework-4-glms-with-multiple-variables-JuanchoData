## Homework 4 Submission ## 
#by JuanCho 
## Question 1:

## 1a)
library(MASS)
#install.packages("performance")
#install.packages("marginaleffects")
library(performance)
library(marginaleffects)
library(modelr)

mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)

#GLM Structure Selection:
#  Response Variable: Seedlings (count data).
#  Predictor Variable: Treatment (categorical: parasitized vs. unparasitized).
#  Link Function: Since seedling count data is non-negative and discrete, a negative binomial distribution is appropriate.

mod.pois <- glm(Seedlings~Treatment, data=mistletoe, family = poisson(link = "log"))
summary(mod.pois)

# Check for overdispersion
check_overdispersion(mod.pois)
#dispersion ratio =   332.240 indicates overdispersion. That means I will fit a negative binomial 
nb_model <- glm.nb(Seedlings ~ Treatment, data = mistletoe)
print(nb_model)
#(Intercept)  Treatmentunparasitized  
#5.731                  -3.157  

# Calculate Mean Absolute Error (MAE) for model fit
mae <- mean(abs(predict(nb_model, type = "response") - mistletoe$Seedlings))
print(mae)
# An MAE of 145.841 suggests that, on average, the predicted seedling counts deviate from observed values by about 146 seedlings per plot 
# This could indicate that the model is struggling to capture the variation in seedling counts effectively.

## ASW: nice! though the seedling values do range from 0 to >2000.


## 1b)
plot_predictions(nb_model, condition = c("Treatment"))
# Visual Interpretation: The plot shows that parasitized trees tend to have more seedlings than unparasitized trees, 
# showing a value close to 0. In contrast, parasitized reveals more that 300 seedlings. 
# Extract coefficients and exponentiate to get counts
exp_coef <- exp(coef(nb_model))
print(exp_coef)

# From the model output: - **Intercept (`exp(Intercept) = 308.21`)**  
# This represents the estimated **mean seedling count** under the baseline condition, which is a **parasitized**
# This means that under parasitized trees, the expected number of seedlings is **~308 per plot**.
# - **Effect of `Treatmentunparasitized` (`exp(-3.16) = 0.0425`)**  This means that seedling counts under **unparasitized trees** are **only ~4.25%** of those under parasitized trees.  
# Equivalently, parasitized trees have **~23.5 times more seedlings** than unparasitized trees (**1/0.0425 ≈ 23.5**).

##ASW: Nice work!!


# We could say that
#1. **Strong Effect of Mistletoe Parasitism:**  
#  - Parasitized trees have **significantly higher** seedling densities than unparasitized trees.
# I think that this supports the hypothesis that mistletoe **modifies light availability and soil conditions**, creating a more favorable environment for seedlings.
## 1c) 
mistletoe$Year <- as.factor(mistletoe$Year)
nb_model21 <- glm.nb(Seedlings ~ Treatment+Year+Treatment*Year, data = mistletoe)
print(nb_model21)
#Coefficients: (Intercept)           Treatmentunparasitized                         Year2012  Treatmentunparasitized:Year2012  
#               5.3864                          -3.8178                           0.6000                           0.8956 
#exp(-3.8178) = ~0.022 → Unparasitized trees had ~98% fewer seedlings than parasitized trees in 2011.

#The effect of mistletoe parasitism on seedling recruitment was weaker in 2012, a wetter year, as seedling counts increased under both parasitized and unparasitized trees. 
#This suggests that mistletoe’s role in shaping forest regeneration is more pronounced in drier conditions, potentially because parasitized trees create microhabitats that retain 
#moisture or improve soil conditions for seedling establishment."

## 30/30

## Question 2:
treemortality <- read.csv("treemortality.csv")
head(treemortality)
## 2a)
glm_model <- glm(mortality ~ thinning, family = binomial(link = "logit"), data = treemortality)
summary(glm_model)
#βo =0.9933, which represents the log-odds of mortality for trees in unthinned areas. 
#Thinning: β1=−1.8559, which represents the change in the log-odds of mortality when a tree is in a thinned area compared to an unthinned area.

#This regression model provides strong evidence that thinning significantly reduces the probability of tree mortality in wildfire-prone areas. 
#The odds of tree mortality in thinned areas are about 84% lower compared to unthinned areas, 
#supporting the hypothesis that thinning treatments are effective in reducing tree mortality due to wildfire.

## ASW: beautiful! what kind of metric of model fit might you use here?

## 2b)
# researchers randomized their sampling by tree size. This means that, during the post-fire resampling in 2023, they made sure to select both small 
# and large trees equally from both thinned and unthinned areas. That means, that model can estimate the effect of thinning on mortality without worrying 
# about tree size influencing the outcome.

## ASW: Good! No influence on estimation of thinning effect

## 2c) 
# Refit the model with slope and roaddist as additional predictors
glm_model_updated <- glm(mortality ~ thinning + slope + roaddist, 
                         family = binomial(link = "logit"), data = treemortality)

# Summarize the updated model
summary(glm_model_updated)
# (Intercept) -22.99461 
# slope  (odds)       0.82480 
# roaddist(odds)      0.54468 

# In Model 2a, thinning had a large effect on reducing tree mortality (odds ratio of 0.16), whereas in Model 2c, the effect is still significant but 
# smaller (odds ratio of 0.4). The inclusion of slope and roaddist reduced the effect of thinning because those factors were likely confounding the 
#relationship between thinning and mortality in the first model.
#In Model 2, slope and roaddist are both statistically significant and contribute substantially to explaining tree mortality. The positive 
# coefficients suggest that steeper slopes and greater distance from roads increase the likelihood of mortality, highlighting their importance as 
# confounders.


## ASW: Nicely done! Yes, the key thing here is that slope and distance from roads are biasing the effect of thinning in the first model, making it appear more effective than it is because of the fact that thinning treatments are more likely to occur in locations where fire severity is already lower (closer to roads, on shallower slopes). The predicted effect of thinning in the first model is a decrease in mortality from 73% to 29%, but in the second model, this effect decreases (Mortality decreases from 54% to 29%)

## 19/20
## total = 49/50

