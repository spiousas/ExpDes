# Additional Exercises for Chapter 7: Quantifying Uncertainty

****************************************************************************************************
IMPORTANT: Feel free to draw on these materials when using the book as the main textbook in your course, but please do not share the additional real-world datasets provided or the solutions to these exercises with anyone outside your class. For example, please do not post them on a non-gated website or in a public GitHub repository.
****************************************************************************************************

* Effects of A Criminal Record in Labor Market - Part IV: Focus on White Applicants
   * Research Question: Does Having a Criminal Record Affect the Chances of Receiving A Call Back for a Job Interview? (Focus on White Applicants, binary outcome)
   * Skills Practiced: Estimate an average treatment effect using data from a randomized experiment and a linear model, and determine whether the estimated average treatment effect is statistically significant at the 5% level.
   * Code Used: read.csv(), <-, head(), lm(), $, summary()$coeff
   * Based on: Pager, Devah. 2003. "The Mark of a Criminal Record." American Journal of Sociology, 108 (5): 937-75.
   * Difficulty: Easy     

 * Effects of A Criminal Record in Labor Market - Part V: Focus on Black Applicants
   * Research Question: Does Having a Criminal Record Affect the Chances of Receiving A Call Back for a Job Interview? (Focus on Black Applicants, binary outcome)
   * Skills Practiced: Answer (1) What is the estimated average treatment effect? (2) Is the effect statistically significant at the 5% level? (3) Can we interpret the effect as causal? And (4) Can we generalize the results?
   * Code Used: read.csv(), <-, head(), lm(), $, summary()$coeff
   * Based on: Pager, Devah. 2003. "The Mark of a Criminal Record." American Journal of Sociology, 108 (5): 937-75.
   * Difficulty: Easy
   
* Effects of Female Leaders in India - Part V: Effect on Drinking Water Facilities
   * Research Question: Do Women Promote Different Policies than Men? Effect of Having a Female Politician on Drinking Water Facilities in Rural India (Note: The outcome is non-binary)
   * Skills Practiced: Estimate average treatment effects using data from a randomized experiment and a linear model, determine whether the estimated average treatment effect is statistically significant at the 5% level, and discuss the internal and external validity of the study. (Note: The effect here is statistically significant.)
   * Code Used: read.csv(), <-, head(), lm(), $, summary()$coeff
   * Based on: Raghabendra Chattopadhyay and Esther Duflo. 2004. "Women as Policy Makers: Evidence from a Randomized Policy Experiment in India." Econometrica, 72 (5): 1409–43.
   * Difficulty: Easy

* Effects of Female Leaders in India - Part VI: Effect on Irrigation Facilities
   * Research Question: Do Women Promote Different Policies than Men? Effect of Having a Female Politician on Irrigation Facilities in Rural India (Note: The outcome is non-binary)
   * Skills Practiced: Estimate average treatment effects using data from a randomized experiment and a linear model, determine whether the estimated average treatment effect is statistically significant at the 5% level, and discuss the internal and external validity of the study. (Note: The effect here is NOT statistically significant.)
   * Code Used: read.csv(), <-, head(), lm(), $, summary()$coeff
   * Based on: Raghabendra Chattopadhyay and Esther Duflo. 2004. "Women as Policy Makers: Evidence from a Randomized Policy Experiment in India." Econometrica, 72 (5): 1409–43.
   * Difficulty: Easy
   
* Predicting Course Grades - Part IV: Quantifying Uncertainty
   * Goal: Using Midterm Scores, Predict (i) Final Exam Scores, (ii) Overall Course Scores, and (iii) Probability of Earning an A or A- in the Course, and Construct 95% Confidence Intervals for the Predictions. (Note: Two of the outcomes are non-binary, the other is binary.)
   * Skills Practiced: Fit a line to make predictions, create scatter plots, add the fitted line to the scatter plot, and construct 95% confidence intervals for our predictions
   * Code Used: read.csv(), <-, head(), $, lm(), plot(), abline(), predict()
   * Difficulty: Easy
   * Note: We recommend using real, historical performance data from your own class.   
   
* Effects of Black-Sounding Names on Call Backs for Job Interviews 
   * Research Question: Is There Racial Discrimination in the Labor Market? (Note: The outcome is binary)
   * Skills Practiced: Answer (1) What is the estimated average treatment effect? (2) Is the effect statistically significant at the 5% level? (3) Can we interpret the effect as causal? And (4) Can we generalize the results?
   * Code Used: read.csv(), <-, head(), hist(), $, ifelse(), ==, lm(), plot(), abline()
   * Based on: Marianne Bertrand and Sendhil Mullainathan. 2004. "Are Emily and Greg more employable than Lakisha and Jamal? A field experiment on labor market discrimination." American Economic Review, 94 (4): 991–1013.
   * Difficulty: Easy
   
* Effect of Small Classes on Student Outcomes
   * Research Question: Do Small Classes in Elementary School Improve Student Outcomes? (Note: Two of the outcomes are non-binary, the other is binary.)
   * Skills Practiced: Estimate average treatment effects using data from a randomized experiment and a linear model, and determine whether the estimated average treatment effects are statistically significant at the 5% level.
   * Code Used: read.csv(), <-, head(), ifelse(), $, ==, lm(), plot(), abline(), summary()$coeff
   * Based on: Frederick Mosteller, "The Tennessee Study of Class Size in the Early School Grades," Future of Children 5, no.2 (1995): 113–27.
   * Difficulty: Easy
   
 * Effect of Social Pressure Message on Probability of Voting - Part III: Estimate an Average Causal Effect, Determine Statistical Significance, and Discuss Internal and External Validity
   * Research Question: Does Social Pressure Affect Turnout? (Note: The outcome is binary)
   * Skills Practiced: Estimate average treatment effects using data from a randomized experiment and a linear model, determine whether the estimated average treatment effect is statistically significant at the 5% level, and discuss the internal and external validity of the study.
   * Code Used: read.csv(), <-, head(), ifelse(), $, ==, lm(), summary()$coeff
   * Based on: Alan Gerber, Donald Green, and Christopher Larimer. 2008. "Social Pressure and Voter Turnout: Evidence from a Large-Scale Field Experiment." American Political Science Review, 102(1): 33-48.
   * Difficulty: Easy-Intermediate
