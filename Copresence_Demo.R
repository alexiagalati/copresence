#The accompanying data are from: 
#Galati, A. (2009). Assessing common ground in conversation:The effect of linguistic and physical co-presence on early planning. Dissertation, Stony Brook University. Stony Brook: ProQuest/UMI

#Install lme library if you don't have it already
install.packages('lme4')

#To get citations for R and the lme4 library for your write-up:
citation()
citation('lme4')

# setwd = set working directory; where your files are
setwd(getwd())
#Or, altetnatively, go to Session > Set Working Directory > To Source File Location

# Let's load the lme4 library and some other stuff
library(lme4)
library(gdata)
library(pander)
source('printStats.R') # this also contains some functions we'll need! (print_stats)
#library(pander)

#The original data file in Excel contains some "#NULL!" strings, which we need to change to missing values ("NA")
Phase2Data = read.xls('Galati_Copresence_Phase2_demo.xlsx', na.strings=c("NA", "#NULL!")) 

#PRELIMINARY DATA CHECK AND DATA PLOTTING 

#OK let's take a peak into the data:
dim(Phase2Data) #get dimensions of dataframe
colnames(Phase2Data) #let's check the column names

#Let's check for missing values. There are a few ways to do this: 
#is.na(Phase2Data$wordsin) #checking missing values (NA as TRUE)
which(is.na(Phase2Data$wordsin)) #this shows that there is missing value in row 449, due to an uncodable case 
#which(!complete.cases(Phase2Data))

#There are a ton of DVs in this dataset, let's group them together
DVs <- Phase2Data[11:24] 
head(DVs)
summary(DVs) #let's get some summary statistics for the DVs

#OK, let's focus just on the DV of initial words (wordsin) in the descriptions.
#Let's check the distribution of initial words by plotting a histogram.
hist(Phase2Data$wordsin, main = paste("Histogram of", "number of initial words"), xlab='number of initial words',ylab='Number of instances') # a bit of a skew to the right
#There's a bit of a skew in this distribution but we're actually interested in whether the residuals are normally distributed.
#We'll check that later, once we build the lmer models. 

#Let's get mean and sds for wordsin (it's also in the summary() output above)
mean(Phase2Data$wordsin, na.rm = TRUE)
sd(Phase2Data$wordsin, na.rm= TRUE)
#summary(Phase2Data$wordsin)

#Let's look at the relationship between copresence and initial words by means of a boxplot
boxplot(wordsin ~ copresen, col=c("white"), Phase2Data)
#Look at the median lines of these boxplots (the thick lines); the intercepts of the lmer models for these conditions will be about there

#OK, LET'S BUILD SOME MIXED MODELS NOW!

#Our random effects are: director (N=32) and item (N=16)
#Our fixed effects are: 
#copres (i.e., whether item is shared Linguistically, Physically, Linguistically&Physically, New)
#partner (i.e., partner identity: Matcher A, Matcher B)
#partnero (i.e., partner order: ABBA, ABAB)
#All are within-participant factors and within-item factors

#BEFORE BUILDING A MAXIMAL MODEL, LET'S BUILD SOME PRELIMINARY, SIMPLER MODELS, 
#TO GAIN A BETTER UNDERSTANDING OF LMERs

#RANDOM INTERCEPT MODELS
#MODEL 1

#Let's start with a simple model examining only the effect of co-presence on the number of words, 
#with random effects for subjects (directors) and items

#This model uses the fixed effect “copresence” (LP, L, P, N) to predict number of words, 
#while controlling for by-subject and by-item variability.
#(This is a random intercept model!)
word.model1= lmer(wordsin~copresen +(1|director) + (1|item),data = Phase2Data, na.rm = TRUE)
#word.model1= lmer(wordsin~copresen +(1|director) + (1|item),data = Phase2Data, na.rm = TRUE, REML=FALSE) #use this version for model comparisons
summary(word.model1)
#print('Initial Words:'); pander(print_stats(word.model1)) #this print_stats function gives us p-values, which are not available in summary()

#Let's look at the summary() output. 
#Under Random effects, note that items have less variability than subjects
#“Residual”  stands for the variability that’s not due to either subjects or items. 
#That variability reflects the fact that every description has some factors that affect the description's length 
#(in terms of initial words) that are not captured by this experiment.

#Under Fixed effects, we have the coefficients for 3 of the 4 co-presence conditions, 
#with L being used as the reference category. 
#The difference between L and LP conditions involves a drop of 5.08 words, 
#whereas the difference between L and P conditions involves an increase in 5.69 words.
#Each of these slopes has a standard error associated with it, as well as a t-value (which is the estimate divided by the standard error--you can check)

#Using the print_stats function, we can get the associated p-values from a t-distribution (I got this from Rick Dale--thanks, Rick!).
#Bodo Winter, in his tutorials, describes an alernative approach for getting p-values (through model comparison). [http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf]


#MODEL 2 

#Let's practice comparing models.
#Let's add partner order as a fixed effect in the model to see if it makes a difference.
#We actually don't have specific predictions about how the two orders (ABBA, ABAB) might differ.

#(We have a prediction about a potential interaction between partner and partner order: 
#speakers in Phase 2 might keep better track of how they shared items with the most recent partner from Phase 1
#so, they may keep better track of how they've shared things with B when they've interacted with B consecutively (in order ABBA vs. ABAB)
#and similarly they may keep better track of how they've shared things with A when interacting in round 3 vs. 4 (in order ABAB vs. ABBA)
#We'll get to that interaction later!)

#For now, let's build the model with partner order as a fixed factor, and then compare it to model 1, to examine if it improves the fit of the data.
#For comparing models, we add the argument REML = FALSE
#This changes some internal computations (wrt the likelihood estimator), which is necessary for comparing models using the likelihood ratio test
word.model2= lmer(wordsin~copresen+partnero +(1|director) + (1|item),data = Phase2Data, na.rm = TRUE, REML=FALSE) #use this version for model comparisons
summary(word.model2)
#print('Initial Words:'); pander(print_stats(word.model2)) #this print_stats function gives us p-values, which are not available in summary()

#Let's rerun model 1 with the added REML = FALSE argument
word.model1= lmer(wordsin~copresen +(1|director) + (1|item),data = Phase2Data, na.rm = TRUE, REML=FALSE)

#Let's compare these models with the likelihood ratio test using the anova() function:
anova(word.model1, word.model2)
#word.model2 is not better than word.model2; adding partner order to the model does not improve its fit. 
#The difference between the models is non-significant (p=.23)
#If you look at the AIC and BIC values, word.model1 has smaller values, which indicate a preferable mode. 
#AIC and BIC  penalize the number of parameters differently. There is also a correction to the AIC for smaller sample sizes.
#The p-value of this model comparison gives as the p-value for partner order as a fixed factor, which is non-significant.


#BUILDING RANDOM SLOPE MODELS!

#So far, the models we've considered are called "random intercept models".

#Let's go back to model1 --the model with just copresence as the only fixed factor-- 
#and let's have a look at its coefficients by director and by item
coef(word.model1)

#As you can see in the outpout, each director and item have different intecepts. 
#This makes sense, since directors and items are specified as random effects in the model.
#However, note that the fixed effect (co-presence) is the same for all directors and for all items.
#That is, this model assumes that the contribution of co-presence is the same for all directors and all items. 
#This may not be a valid assumption, as the effect of co-presence may be different across speakers (directors). And across items for that matter.
#For instance, some directors may keep better track of the co-presence conditions than others. Or some items may be more easily remembered or distinguished in descriptions.

#Let's redefine our model 1 to capture this, by adding the copresence factor into the random effect structure of directors and items: 
word.model3= lmer(wordsin~copresen +(1+copresen|director) + (1+copresen|item),data = Phase2Data, na.rm = TRUE, REML=FALSE)
summary(word.model3)

#Let's check the coefficients again:
coef(word.model3)
#As you can see in the output, the coefficients for each co-presence condition now differ across both directors and items.

anova(word.model1, word.model3)
#There is a significant difference between these models.
#(Note that the AIC and log likelihood recommend model3, whereas BIC recommends model1)

#Let's build yet another model, adding the theoretically motivated interaction between partner and partner order as a fixed factor
word.model4= lmer(wordsin~copresen+partner*partnero +(1+copresen|director) + (1+copresen|item),data = Phase2Data, na.rm = TRUE, REML=FALSE)
summary(word.model4)
print('Initial Words:'); pander(print_stats(word.model4)) #this print_stats function gives us p-values, which are not available in summary()

#You can look at the means to better understand the main effect of partner order and the interaction between partner*partner order
#pander(aggregate(wordsin~copresen+partnero+partner,data=Phase2Data,FUN=mean)) #reveals more words in Phase 2 for order ABAB than ABBA

#Let's compare it with model 3
anova(word.model3, word.model4)
#There is a significant difference between these models.
#(Note, again, that the AIC and log likelihood recommend model4, whereas BIC recommends model3)

#LET'S CHECK SOME ASSUMPTIONS 

#Let's check the distribution of residuals of this model to see if they're normally distributed
#A histogram
hist(resid(word.model4)) #does this look approximately normal?
#A density plot
plot(density(resid(word.model4))) #does this look approximately normal?
#A Q-Q plot:
qqnorm(resid(word.model4)) 
qqline(resid(word.model4)) #check if they fall on a straight line. 
#You may re-run the model on log-transformed wordsin, but in my opinion it's pretty OK!



#BULDING A MAXIMAL MODEL

#OK, so we've established that makes sense to have random slope models, 
#and we've already practiced building a few theoretically motivated random slope models
#in a bottom-up fashion, by comparing models. 

#An alternative approach for building models comes from Barr, Levy, Scheepers, and Tilly (2013).
#Barr et al. recommend "keeping it maximal", wrt the random effect structure, at least for controlled experiments.
#This means including all random slopes justified by the experimental design.

#For this experiment this yields the model below.
#WARNING: This model might take a while to run! I recommend running it at home if you want to see what happens 

word.model5 = lmer(wordsin~copresen*partnero*partner + (1+copresen*partnero*partner|director) + (1+copresen*partnero*partner|item), data = Phase2Data, na.rm = TRUE, REML=FALSE)
summary(word.model5)
#print('Initial Words:'); pander(print_stats(word.model5))


#This model does not converge. So, what's next? How do we simplify this model?
#Barr et al. recommend a couple of principled ways to simplify models.
#One is to treat as independent the random intercept and random slope of each item / subject. e.g., break (1+copresen*partnero*partner/director) into (1/director) + (0+copresen*partnero*partner/director) 
#Another is to remove the random intercept from the model, e.g., turn (1+copresen*partnero*partner/director) into (0+copresen*partnero*partner/director)

#For a different approach to simplifying models, see:
#Bates, D., Kliegl, R., Vasishth, S., & Baayen. H. (2015). Parsimonious mixed models. arXiv preprint arXiv:1506.04967, 
#One of the recommendations is to remove the higher order terms from the random effect structure.
#So we may turn (1+copresen*partnero*partner) into (1+copresence + partnero*partner), and continue removing interactions (and even fixed effects), until the model converges.
