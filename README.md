# Statistics Project
In this project, we examined the importance of feature selection in two types of problems: regression and classification.
<br>
<br>
In the regression problem, we investigated the impact of changing the criterion in the Stepwise Regression algorithm (the penalty) on variable selection for the model and its predictive ability. Our research hypothesis was that a low penalty would lead to an excess of explanatory variables and overfitting, while a high penalty would result in a scarcity of variables and an inefficient model. Therefore, the ideal penalty would be at some point in the middle, regardless of the number of variables in the initial model (empty, full, or "medium").
<br>
<br>
In the classification problem, we examined the following methods for feature selection: feature importance using Random Forest and XGBoost, feature ranking using Fisher Score, and feature selection using the Chi-Squared test for independence. The effectiveness of the different methods was tested using four classification models: Logistic Regression, Random Forest, XGBoost, and a Neural Network. Our research hypothesis was that a feature selection method would suit models whose classification mechanism is similar to its ranking mechanism.
<br>
<br>
The data we used to test our hypotheses was a list of the most played songs on Spotify in 2023 taken from Kaggle, with the target variable being the number of plays of a song. In the regression model, we aimed to predict the number of plays, and in the classification models, we divided the songs into popular or not popular based on their number of plays (if the number of plays is greater than the median value, the song is considered popular).
<br>
<br>
Gilad Erez, May Hakim and Liel Partosh 
