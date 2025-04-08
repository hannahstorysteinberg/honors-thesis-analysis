# We need to make sure that the legible pictures are rated as significantly more legible than the illegible pictures :)
# The right way to do this would be with a linear, mixed-effects model, that predicts legibility rating as a function of condition (legible vs. illegible) and presentation type (full sentence vs. single word) with random effects by sentence and by participant:
#   
#   rating ~ 1 + condition*presentation + (1 + condition*presentation | sentence) + (1 + condition*presentation | participant)
# 
# If the model doesn’t converge, we can try removing some of the random slope.
# 
# We also need a plot showing two bars (one per condition, in including standard errors) of average ratings across pictures, with individual datapoints overlaid on top. This can be done (I think?) with geom_bar and geom_jitter in ggplot2. 
# 
# A quick-and-dirty test to see what the results are is to run a t-test:
#   * Average the ratings for each picture
# * Run a dependent-samples t-test comparing the ratings between the legible and illegible versions of each sentence
# 
# In addition, we should also look at the ratings of the fillers to see that they are legible as well. Hopefully they would be rated as more legible than the illegible critical sentences; and I expect them to not be statistically distinguishable from the legible critical sentences. For this, we can run a model just on full sentences (not individual words), and now the random effect is by “picture” (i.e., png file), not “sentence” (because some sentences only have the “filler” condition and some sentences only have the “legible”+”illegible” conditions):
#   
#   rating ~ 1 + condition + (1 | sentence) + (1 + condition | participant)
# 
# Here, condition has 3 levels (filler, legible, illegible), and we can do pairwise comparisons between filler and each of the other two levels.

