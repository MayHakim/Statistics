#install.packages("corrplot", repos = "http://cran.us.r-project.org")
#install.packages("glmnet", repos = "http://cran.us.r-project.org")
#install.packages("caret", dependencies = c("Depends", "Suggests"))

library(dplyr)
library(corrplot)
library(car)
library(glmnet)
library(Metrics)
library(ggplot2)

# Loading the data
dataset <- read.csv(file.choose(),header = T)

# Fill empty values
dataset$key[is.na(dataset$key)] <- "Unknown"
dataset$in_shazam_charts[is.na(dataset$in_shazam_charts)] <- 0

# Remove row 575 from the dataset
dataset <- dataset[-575, ]

# Convert 'streams' to numeric if possible
dataset$streams <- as.numeric(as.character(dataset$streams))

df_cont <- subset(dataset, select = -c(track_name, artist.s._name, key, mode))

# ------------ Some Plots -------------#

# Subset the dataset to include only the specified columns
for_pairs_one <- c("streams","released_month", 
                      "in_spotify_playlists", "in_spotify_charts",
                      "in_deezer_playlists", "in_deezer_charts", "in_shazam_charts",
                      "bpm")
for_pairs_two <- c("streams", "bpm", "danceability_.", "valence_.", 
                   "energy_.", "acousticness_.", "instrumentalness_.", 
                   "liveness_.", "speechiness_.")

# Set the output file name and dimensions
png("pairs_plot1.png", width = 1100, height = 900, res = 200)
pairs(df_cont[1:100,for_pairs_one],pch = 1,col="blue",cex = 0.25,cex.labels=0.7,
      labels = c("streams","month", 
      "spot_list", "spot_chart",
      "deez_list", "deez_chart", "shaz_chart",
      "bpm"))
# Save the plot
dev.off()

png("pairs_plot2.png", width = 1100, height = 900, res = 200)
pairs(df_cont[1:100,for_pairs_two],pch = 1,col="blue",cex = 0.25,cex.labels=0.6,
      labels = c("streams", "bpm", "dance", "valence", 
                 "energy", "acoust.", "instrum", 
                 "live", "speech"))
# Save the plot
dev.off()

png("correlations.png", width = 1100, height = 900, res = 200)
correlation_matrix <- cor(df_cont)
# Plot correlation matrix as a heatmap with reduced text size
corrplot(correlation_matrix, method = "color", tl.cex = 0.7)

# ----------- Train-Test Split ---------- #

#make this reproducible
set.seed(42)

# Number of samples for the test set
n_test <- 100

# Randomly select indices for the test set
test_indices <- sample(1:nrow(dataset), n_test)

# Create test set
test <- dataset[test_indices, ]
x_test <- test[, !names(test) %in% "streams", drop = FALSE]  # Predictor variables for testing
y_test <- test[, "streams"]  # Response variable for testing

# Create train set (excluding test set indices)
train <- dataset[-test_indices, ]
x_train <- train[, !names(train) %in% "streams", drop = FALSE]  # Predictor variables for training
y_train <- train[, "streams"]  # Response variable for training

# ----------- Models ------------ #

# only continuous variables of "train" set
train_cont <- subset(train, select = -c(track_name, artist.s._name, key, mode))


cont_lm = lm(formula='streams ~ .',data=train_cont)
summary(cont_lm)

vif_values <- car::vif(cont_lm)
print(vif_values)
sorted_vif <- sort(vif_values, decreasing = TRUE)

# Plot bar chart of VIF values

png("VIF.png",width = 1100, height = 900, res = 200)
barplot(sorted_vif, main = "VIF Values",
        ylab = "VIF",col = "purple", las = 2,cex.names = 0.6,
        names.arg = c("spot_list","deez_list","apple_list","spot_chart","deez_chart",
                      "energy","apple_chart","acoustic","shazam_chart","valence",
                      "dance","year","speech","artists","month","bpm","day","live","instrum"))
dev.off()

# Make "full" and "empty" models for later use in stepwise regression

full_lm = lm(formula='streams ~ artist_count+released_year+released_month+released_day+
                                in_spotify_playlists+in_spotify_charts+in_apple_playlists+  
                                in_apple_charts+in_deezer_playlists+in_deezer_charts+
                                in_shazam_charts+bpm+factor(key)+factor(mode)+danceability_.+
                                valence_.+energy_.+acousticness_.+instrumentalness_.+
                                liveness_.+speechiness_.',data=train)
summary(full_lm)

full_formula = 'streams ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_.'

full_full_lm <- lm(formula = full_formula, data = train)
summary(full_full_lm)

emp_lm <- lm(formula='streams ~ 1', data = train)
summary(emp_lm)

step.model.full <- step(full_full_lm, direction = "both", scope= ~1, trace=0)
summary(step.model.full)

step.model.emp <- step(emp_lm, direction = "both", scope= ~ artist_count + released_year + released_month + released_day +
                         in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                         in_apple_charts + in_deezer_playlists + in_deezer_charts +
                         in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                         valence_. + energy_. + acousticness_. + instrumentalness_. +
                         liveness_. + speechiness_. + 
                         factor(key):artist_count + factor(mode):released_year +
                         factor(key):released_month + factor(mode):released_day +
                         factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                         factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                         factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                         factor(key):in_shazam_charts + factor(mode):bpm + 
                         factor(key):danceability_. + factor(mode):valence_. +
                         factor(key):energy_. + factor(mode):acousticness_. +
                         factor(key):instrumentalness_. + factor(mode):liveness_. +
                         factor(key):speechiness_., trace=0)

summary(step.model.emp)

step.model.full.BIC <- step(full_full_lm, direction = "both", scope= ~1, k = log(nrow(dataset)),trace = 0)
summary(step.model.full.BIC)

step.model.emp.BIC <- step(step(emp_lm, direction = "both", scope= ~ artist_count + released_year + released_month + released_day +
                                  in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                                  in_apple_charts + in_deezer_playlists + in_deezer_charts +
                                  in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                                  valence_. + energy_. + acousticness_. + instrumentalness_. +
                                  liveness_. + speechiness_. + 
                                  factor(key):artist_count + factor(mode):released_year +
                                  factor(key):released_month + factor(mode):released_day +
                                  factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                                  factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                                  factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                                  factor(key):in_shazam_charts + factor(mode):bpm + 
                                  factor(key):danceability_. + factor(mode):valence_. +
                                  factor(key):energy_. + factor(mode):acousticness_. +
                                  factor(key):instrumentalness_. + factor(mode):liveness_. +
                                  factor(key):speechiness_.), k = log(nrow(dataset)),trace = 0)
summary(step.model.emp.BIC)


# ---------- Comparing the Models on the Test Sets ------------ #

# Create an empty matrix
results_matrix <- matrix(NA, nrow = 3, ncol = 4)

# Assign column names
colnames(results_matrix) <- c("emp_AIC", "emp_BIC", "full_AIC", "full_BIC")

# Assign row names (index)
rownames(results_matrix) <- c("num_features", "train_MSE", "test_MSE")

# Fill in the matrix with the desired values
results_matrix["num_features", "emp_AIC"] <- length(step.model.emp$coefficients)
results_matrix["num_features", "emp_BIC"] <- length(step.model.emp.BIC$coefficients)
results_matrix["num_features", "full_AIC"] <- length(step.model.full$coefficients)
results_matrix["num_features", "full_BIC"] <- length(step.model.full.BIC$coefficients)

results_matrix["train_MSE", "emp_AIC"] <- mse(predict(step.model.emp),y_train)
results_matrix["train_MSE", "emp_BIC"] <- mse(predict(step.model.emp.BIC),y_train)
results_matrix["train_MSE", "full_AIC"] <- mse(predict(step.model.full),y_train)
results_matrix["train_MSE", "full_BIC"] <- mse(predict(step.model.full.BIC),y_train)

results_matrix["test_MSE", "emp_AIC"] <- mse(predict(step.model.emp,newdata = x_test),y_test)
results_matrix["test_MSE", "emp_BIC"] <- mse(predict(step.model.emp.BIC,newdata = x_test),y_test)
results_matrix["test_MSE", "full_AIC"] <- mse(predict(step.model.full,newdata = x_test),y_test)
results_matrix["test_MSE", "full_BIC"] <- mse(predict(step.model.full.BIC,newdata = x_test),y_test)

result_plot = rbind(results_matrix, c(1,2,3,4))

png("resultas.png",width = 1100, height = 900, res = 200)
par(mar = c(5, 5, 4, 2) + 0.1, cex.axis = 0.6) # Adjust the margins to make room for labels
plot(result_plot[4,], result_plot[1,], ylim = c(6.5e+16, 1.1e+17), main = "Train / Test MSE", col = "blue", pch = 19, xaxt = "n", yaxt = 'n', xlab='Model',ylab='MSE')
axis(side = 1, at = c(1,2,3,4), labels = colnames(result_plot))
axis(side = 2, las = 1)
points(result_plot[4,], result_plot[3,], col = "red", pch = 19)
points(result_plot[4,], result_plot[2,], col = "blue", pch = 19)
legend("topright", legend = c("Train", "Test"), col = c("blue", "red"), pch = 19,cex = 0.75)
grid()
dev.off()

# ------------------- Penalty Experiment --------------------- #

# ----- start full. Train with changing penalty K
model_full_0.1log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.1*log(nrow(dataset)),trace = 0)
model_full_0.2log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.2*log(nrow(dataset)),trace = 0)
model_full_0.3log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.3*log(nrow(dataset)),trace = 0)
model_full_0.4log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.4*log(nrow(dataset)),trace = 0)
model_full_0.5log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.5*log(nrow(dataset)),trace = 0)
model_full_0.6log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.6*log(nrow(dataset)),trace = 0)
model_full_0.7log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.7*log(nrow(dataset)),trace = 0)
model_full_0.8log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.8*log(nrow(dataset)),trace = 0)
model_full_0.9log <- step(full_full_lm, direction = "both", scope= ~1, k = 0.9*log(nrow(dataset)),trace = 0)
model_full_1.0log <- step(full_full_lm, direction = "both", scope= ~1, k = log(nrow(dataset)),trace = 0)
model_full_1.1log <- step(full_full_lm, direction = "both", scope= ~1, k = 1.1*log(nrow(dataset)),trace = 0)
model_full_1.2log <- step(full_full_lm, direction = "both", scope= ~1, k = 1.2*log(nrow(dataset)),trace = 0)
model_full_1.3log <- step(full_full_lm, direction = "both", scope= ~1, k = 1.3*log(nrow(dataset)),trace = 0)
model_full_1.4log <- step(full_full_lm, direction = "both", scope= ~1, k = 1.4*log(nrow(dataset)),trace = 0)
model_full_1.8log <- step(full_full_lm, direction = "both", scope= ~1, k = 1.9*log(nrow(dataset)),trace = 0)

# Create an empty matrix
Penalty_results_matrix <- matrix(NA, nrow = 3, ncol = 14)

# Assign column names
colnames(Penalty_results_matrix) <- c('0.1log(n)','0.2log(n)','0.3log(n)','0.4log(n)','0.5log(n)','0.6log(n)',
                              '0.7log(n)','0.8log(n)','0.9log(n)','1.0log(n)','1.1log(n)','1.2log(n)','1.3log(n)','1.4log(n)')

# Assign row names (index)
rownames(Penalty_results_matrix) <- c("num_features", "train_MSE", "test_MSE")

# Fill in the matrix with the desired values
Penalty_results_matrix["num_features", '0.1log(n)'] <- length(model_full_0.1log$coefficients)-1
Penalty_results_matrix["num_features", '0.2log(n)'] <- length(model_full_0.2log$coefficients)-1
Penalty_results_matrix["num_features", '0.3log(n)'] <- length(model_full_0.3log$coefficients)-1
Penalty_results_matrix["num_features", '0.4log(n)'] <- length(model_full_0.4log$coefficients)-1
Penalty_results_matrix["num_features", '0.5log(n)'] <- length(model_full_0.5log$coefficients)-1
Penalty_results_matrix["num_features", '0.6log(n)'] <- length(model_full_0.6log$coefficients)-1
Penalty_results_matrix["num_features", '0.7log(n)'] <- length(model_full_0.7log$coefficients)-1
Penalty_results_matrix["num_features", '0.8log(n)'] <- length(model_full_0.8log$coefficients)-1
Penalty_results_matrix["num_features", '0.9log(n)'] <- length(model_full_0.9log$coefficients)-1
Penalty_results_matrix["num_features", '1.0log(n)'] <- length(model_full_1.0log$coefficients)-1
Penalty_results_matrix["num_features", '1.1log(n)'] <- length(model_full_1.1log$coefficients)-1
Penalty_results_matrix["num_features", '1.2log(n)'] <- length(model_full_1.2log$coefficients)-1
Penalty_results_matrix["num_features", '1.3log(n)'] <- length(model_full_1.3log$coefficients)-1
Penalty_results_matrix["num_features", '1.4log(n)'] <- length(model_full_1.4log$coefficients)-1

Penalty_results_matrix["train_MSE", '0.1log(n)'] <- mse(predict(model_full_0.1log),y_train)
Penalty_results_matrix["train_MSE", '0.2log(n)'] <- mse(predict(model_full_0.2log),y_train)
Penalty_results_matrix["train_MSE", '0.3log(n)'] <- mse(predict(model_full_0.3log),y_train)
Penalty_results_matrix["train_MSE", '0.4log(n)'] <- mse(predict(model_full_0.4log),y_train)
Penalty_results_matrix["train_MSE", '0.5log(n)'] <- mse(predict(model_full_0.5log),y_train)
Penalty_results_matrix["train_MSE", '0.6log(n)'] <- mse(predict(model_full_0.6log),y_train)
Penalty_results_matrix["train_MSE", '0.7log(n)'] <- mse(predict(model_full_0.7log),y_train)
Penalty_results_matrix["train_MSE", '0.8log(n)'] <- mse(predict(model_full_0.8log),y_train)
Penalty_results_matrix["train_MSE", '0.9log(n)'] <- mse(predict(model_full_0.9log),y_train)
Penalty_results_matrix["train_MSE", '1.0log(n)'] <- mse(predict(model_full_1.0log),y_train)
Penalty_results_matrix["train_MSE", '1.1log(n)'] <- mse(predict(model_full_1.1log),y_train)
Penalty_results_matrix["train_MSE", '1.2log(n)'] <- mse(predict(model_full_1.2log),y_train)
Penalty_results_matrix["train_MSE", '1.3log(n)'] <- mse(predict(model_full_1.3log),y_train)
Penalty_results_matrix["train_MSE", '1.4log(n)'] <- mse(predict(model_full_1.4log),y_train)

Penalty_results_matrix["test_MSE", '0.1log(n)'] <- mse(predict(model_full_0.1log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.2log(n)'] <- mse(predict(model_full_0.2log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.3log(n)'] <- mse(predict(model_full_0.3log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.4log(n)'] <- mse(predict(model_full_0.4log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.5log(n)'] <- mse(predict(model_full_0.5log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.6log(n)'] <- mse(predict(model_full_0.6log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.7log(n)'] <- mse(predict(model_full_0.7log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.8log(n)'] <- mse(predict(model_full_0.8log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.9log(n)'] <- mse(predict(model_full_0.9log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.0log(n)'] <- mse(predict(model_full_1.0log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.1log(n)'] <- mse(predict(model_full_1.1log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.2log(n)'] <- mse(predict(model_full_1.2log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.3log(n)'] <- mse(predict(model_full_1.3log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.4log(n)'] <- mse(predict(model_full_1.4log,newdata = x_test),y_test)

penalty_plot_f = rbind(Penalty_results_matrix, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4))

png("penalty_full.png", width = 1200, height = 900, res = 200)
par(mar = c(5, 5, 4, 2) + 0.1, cex.axis = 0.6) # Adjust the margins to make room for labels
plot(penalty_plot_f[4,], penalty_plot_f[1,], ylim = c(6.2e+16, 1.1e+17), main = "Stepwise Penalty Comparison, Starting: Full Model", col = "blue", pch = 19, xaxt = "n", yaxt = 'n', xlab='Penalty',ylab='MSE')
axis(side = 1, at = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4), labels = colnames(penalty_plot_f))
axis(side = 2, las = 1)
grid()
points(penalty_plot_f[4,], penalty_plot_f[3,], col = "red", pch = 19)
points(penalty_plot_f[4,], penalty_plot_f[2,], col = "blue", pch = 19)
text(penalty_plot_f[4,], 1.07e+17, labels = format(penalty_plot_f[1,], scientific = FALSE), pos = 3, cex = 0.7) # Add the values over each point
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), pch = 19, cex = 0.75)

mtext("Top numbers: number of features in model", side = 3, line = 0, cex = 0.8) # Add sub-main-title lower

dev.off()

#("penalty_full.png",width = 1100, height = 900, res = 200)
#par(mar = c(5, 5, 4, 2) + 0.1, cex.axis = 0.6) # Adjust the margins to make room for labels
#plot(penalty_plot_f[4,], penalty_plot_f[1,], ylim = c(6.2e+16, 1.1e+17), main = "Stepwise Penalty Comparison, Strating: Full Model", col = "blue", pch = 19, xaxt = "n", yaxt = 'n', xlab='Penalty',ylab='MSE')
#axis(side = 1, at = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4), labels = colnames(penalty_plot_f))
#axis(side = 2, las = 1)
#points(penalty_plot_f[4,], penalty_plot_f[3,], col = "red", pch = 19)
#points(penalty_plot_f[4,], penalty_plot_f[2,], col = "blue", pch = 19)
#legend("topright", legend = c("Train", "Test"), col = c("blue", "red"), pch = 19,cex = 0.75)
#grid()
#dev.off()

# ----- now start empty. Train with changing penalty K
model_emp_0.1log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.1*log(nrow(dataset)),trace = 0)
model_emp_0.2log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.2*log(nrow(dataset)),trace = 0)
model_emp_0.3log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.3*log(nrow(dataset)),trace = 0)
model_emp_0.4log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.4*log(nrow(dataset)),trace = 0)
model_emp_0.5log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.5*log(nrow(dataset)),trace = 0)
model_emp_0.6log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.6*log(nrow(dataset)),trace = 0)
model_emp_0.7log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.7*log(nrow(dataset)),trace = 0)
model_emp_0.8log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.8*log(nrow(dataset)),trace = 0)
model_emp_0.9log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 0.9*log(nrow(dataset)),trace = 0)
model_emp_1.0log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = log(nrow(dataset)),trace = 0)
model_emp_1.1log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 1.1*log(nrow(dataset)),trace = 0)
model_emp_1.2log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 1.2*log(nrow(dataset)),trace = 0)
model_emp_1.3log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 1.3*log(nrow(dataset)),trace = 0)
model_emp_1.4log <- step(emp_lm, direction = "both", scope = ~ artist_count + released_year + released_month + released_day +
                           in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
                           in_apple_charts + in_deezer_playlists + in_deezer_charts +
                           in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
                           valence_. + energy_. + acousticness_. + instrumentalness_. +
                           liveness_. + speechiness_. + 
                           factor(key):artist_count + factor(mode):released_year +
                           factor(key):released_month + factor(mode):released_day +
                           factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
                           factor(key):in_apple_playlists + factor(mode):in_apple_charts +
                           factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
                           factor(key):in_shazam_charts + factor(mode):bpm + 
                           factor(key):danceability_. + factor(mode):valence_. +
                           factor(key):energy_. + factor(mode):acousticness_. +
                           factor(key):instrumentalness_. + factor(mode):liveness_. +
                           factor(key):speechiness_., k = 1.4*log(nrow(dataset)),trace = 0)


# Create an empty matrix
Penalty_results_matrix_e <- matrix(NA, nrow = 3, ncol = 14)

# Assign column names
colnames(Penalty_results_matrix_e) <- c('0.1log(n)','0.2log(n)','0.3log(n)','0.4log(n)','0.5log(n)','0.6log(n)',
                                      '0.7log(n)','0.8log(n)','0.9log(n)','1.0log(n)','1.1log(n)','1.2log(n)','1.3log(n)','1.4log(n)')

# Assign row names (index)
rownames(Penalty_results_matrix_e) <- c("num_features", "train_MSE", "test_MSE")

# Fill in the matrix with the desired values
Penalty_results_matrix_e["num_features", '0.1log(n)'] <- length(model_emp_0.1log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.2log(n)'] <- length(model_emp_0.2log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.3log(n)'] <- length(model_emp_0.3log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.4log(n)'] <- length(model_emp_0.4log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.5log(n)'] <- length(model_emp_0.5log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.6log(n)'] <- length(model_emp_0.6log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.7log(n)'] <- length(model_emp_0.7log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.8log(n)'] <- length(model_emp_0.8log$coefficients)-1
Penalty_results_matrix_e["num_features", '0.9log(n)'] <- length(model_emp_0.9log$coefficients)-1
Penalty_results_matrix_e["num_features", '1.0log(n)'] <- length(model_emp_1.0log$coefficients)-1
Penalty_results_matrix_e["num_features", '1.1log(n)'] <- length(model_emp_1.1log$coefficients)-1
Penalty_results_matrix_e["num_features", '1.2log(n)'] <- length(model_emp_1.2log$coefficients)-1
Penalty_results_matrix_e["num_features", '1.3log(n)'] <- length(model_emp_1.3log$coefficients)-1
Penalty_results_matrix_e["num_features", '1.4log(n)'] <- length(model_emp_1.4log$coefficients)-1

Penalty_results_matrix_e["train_MSE", '0.1log(n)'] <- mse(predict(model_emp_0.1log),y_train)
Penalty_results_matrix_e["train_MSE", '0.2log(n)'] <- mse(predict(model_emp_0.2log),y_train)
Penalty_results_matrix_e["train_MSE", '0.3log(n)'] <- mse(predict(model_emp_0.3log),y_train)
Penalty_results_matrix_e["train_MSE", '0.4log(n)'] <- mse(predict(model_emp_0.4log),y_train)
Penalty_results_matrix_e["train_MSE", '0.5log(n)'] <- mse(predict(model_emp_0.5log),y_train)
Penalty_results_matrix_e["train_MSE", '0.6log(n)'] <- mse(predict(model_emp_0.6log),y_train)
Penalty_results_matrix_e["train_MSE", '0.7log(n)'] <- mse(predict(model_emp_0.7log),y_train)
Penalty_results_matrix_e["train_MSE", '0.8log(n)'] <- mse(predict(model_emp_0.8log),y_train)
Penalty_results_matrix_e["train_MSE", '0.9log(n)'] <- mse(predict(model_emp_0.9log),y_train)
Penalty_results_matrix_e["train_MSE", '1.0log(n)'] <- mse(predict(model_emp_1.0log),y_train)
Penalty_results_matrix_e["train_MSE", '1.1log(n)'] <- mse(predict(model_emp_1.1log),y_train)
Penalty_results_matrix_e["train_MSE", '1.2log(n)'] <- mse(predict(model_emp_1.2log),y_train)
Penalty_results_matrix_e["train_MSE", '1.3log(n)'] <- mse(predict(model_emp_1.3log),y_train)
Penalty_results_matrix_e["train_MSE", '1.4log(n)'] <- mse(predict(model_emp_1.4log),y_train)

Penalty_results_matrix_e["test_MSE", '0.1log(n)'] <- mse(predict(model_emp_0.1log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.2log(n)'] <- mse(predict(model_emp_0.2log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.3log(n)'] <- mse(predict(model_emp_0.3log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.4log(n)'] <- mse(predict(model_emp_0.4log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.5log(n)'] <- mse(predict(model_emp_0.5log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.6log(n)'] <- mse(predict(model_emp_0.6log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.7log(n)'] <- mse(predict(model_emp_0.7log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.8log(n)'] <- mse(predict(model_emp_0.8log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '0.9log(n)'] <- mse(predict(model_emp_0.9log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '1.0log(n)'] <- mse(predict(model_emp_1.0log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '1.1log(n)'] <- mse(predict(model_emp_1.1log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '1.2log(n)'] <- mse(predict(model_emp_1.2log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '1.3log(n)'] <- mse(predict(model_emp_1.3log,newdata = x_test),y_test)
Penalty_results_matrix_e["test_MSE", '1.4log(n)'] <- mse(predict(model_emp_1.4log,newdata = x_test),y_test)

penalty_plot_e = rbind(Penalty_results_matrix_e, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4))

png("penalty_emp.png", width = 1200, height = 900, res = 200)
par(mar = c(5, 5, 4, 2) + 0.1, cex.axis = 0.6) # Adjust the margins to make room for labels
plot(penalty_plot_e[4,], penalty_plot_e[1,], ylim = c(6.2e+16, 1.1e+17), main = "Stepwise Penalty Comparison, Starting: Empty Model", col = "blue", pch = 19, xaxt = "n", yaxt = 'n', xlab='Penalty',ylab='MSE')
axis(side = 1, at = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4), labels = colnames(penalty_plot_e))
axis(side = 2, las = 1)
grid()
points(penalty_plot_e[4,], penalty_plot_e[3,], col = "red", pch = 19)
points(penalty_plot_e[4,], penalty_plot_e[2,], col = "blue", pch = 19)
text(penalty_plot_e[4,], 1.07e+17, labels = format(penalty_plot_e[1,], scientific = FALSE), pos = 3, cex = 0.7) # Add the values over each point
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), pch = 19, cex = 0.75)

mtext("Top numbers: number of features in model", side = 3, line = 0, cex = 0.8) # Add sub-main-title lower

dev.off()

#png("penalty_emp.png",width = 1200, height = 900, res = 200)
#par(mar = c(5, 5, 4, 2) + 0.1, cex.axis = 0.6) # Adjust the margins to make room for labels
#plot(penalty_plot_e[4,], penalty_plot_e[1,], ylim = c(6.2e+16, 1.1e+17), main = "Stepwise Penalty Comparison, Starting: Empty Model", col = "blue", pch = 19, xaxt = "n", yaxt = 'n', xlab='Penalty',ylab='MSE')
#axis(side = 1, at = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4), labels = colnames(penalty_plot_e))
#axis(side = 2, las = 1)
#points(penalty_plot_e[4,], penalty_plot_e[3,], col = "red", pch = 19)
#points(penalty_plot_e[4,], penalty_plot_e[2,], col = "blue", pch = 19)
#legend("topright", legend = c("Train", "Test"), col = c("blue", "red"), pch = 19,cex = 0.75)
#grid()
#dev.off()

# -------------- now medium

a = formula( ~ artist_count + released_year + released_month + released_day +
               in_spotify_playlists + in_spotify_charts + in_apple_playlists +  
               in_apple_charts + in_deezer_playlists + in_deezer_charts +
               in_shazam_charts + bpm + factor(key) + factor(mode) + danceability_. +
               valence_. + energy_. + acousticness_. + instrumentalness_. +
               liveness_. + speechiness_. + 
               factor(key):artist_count + factor(mode):released_year +
               factor(key):released_month + factor(mode):released_day +
               factor(key):in_spotify_playlists + factor(mode):in_spotify_charts +
               factor(key):in_apple_playlists + factor(mode):in_apple_charts +
               factor(key):in_deezer_playlists + factor(mode):in_deezer_charts +
               factor(key):in_shazam_charts + factor(mode):bpm + 
               factor(key):danceability_. + factor(mode):valence_. +
               factor(key):energy_. + factor(mode):acousticness_. +
               factor(key):instrumentalness_. + factor(mode):liveness_. +
               factor(key):speechiness_.)

# ----- start full. Train with changing penalty K
model_med_0.1log <- step(full_lm, direction = "both", scope= a, k = 0.1*log(nrow(dataset)),trace = 0)
model_med_0.2log <- step(full_lm, direction = "both", scope= a, k = 0.2*log(nrow(dataset)),trace = 0)
model_med_0.3log <- step(full_lm, direction = "both", scope= a, k = 0.3*log(nrow(dataset)),trace = 0)
model_med_0.4log <- step(full_lm, direction = "both", scope= a, k = 0.4*log(nrow(dataset)),trace = 0)
model_med_0.5log <- step(full_lm, direction = "both", scope= a, k = 0.5*log(nrow(dataset)),trace = 0)
model_med_0.6log <- step(full_lm, direction = "both", scope= a, k = 0.6*log(nrow(dataset)),trace = 0)
model_med_0.7log <- step(full_lm, direction = "both", scope= a, k = 0.7*log(nrow(dataset)),trace = 0)
model_med_0.8log <- step(full_lm, direction = "both", scope= a, k = 0.8*log(nrow(dataset)),trace = 0)
model_med_0.9log <- step(full_lm, direction = "both", scope= a, k = 0.9*log(nrow(dataset)),trace = 0)
model_med_1.0log <- step(full_lm, direction = "both", scope= a, k = log(nrow(dataset)),trace = 0)
model_med_1.1log <- step(full_lm, direction = "both", scope= a, k = 1.1*log(nrow(dataset)),trace = 0)
model_med_1.2log <- step(full_lm, direction = "both", scope= a, k = 1.2*log(nrow(dataset)),trace = 0)
model_med_1.3log <- step(full_lm, direction = "both", scope= a, k = 1.3*log(nrow(dataset)),trace = 0)
model_med_1.4log <- step(full_lm, direction = "both", scope= a, k = 1.4*log(nrow(dataset)),trace = 0)

# Create an empty matrix
Penalty_results_matrix <- matrix(NA, nrow = 3, ncol = 14)

# Assign column names
colnames(Penalty_results_matrix) <- c('0.1log(n)','0.2log(n)','0.3log(n)','0.4log(n)','0.5log(n)','0.6log(n)',
                                      '0.7log(n)','0.8log(n)','0.9log(n)','1.0log(n)','1.1log(n)','1.2log(n)','1.3log(n)','1.4log(n)')

# Assign row names (index)
rownames(Penalty_results_matrix) <- c("num_features", "train_MSE", "test_MSE")

# Fill in the matrix with the desired values
Penalty_results_matrix["num_features", '0.1log(n)'] <- length(model_med_0.1log$coefficients)-1
Penalty_results_matrix["num_features", '0.2log(n)'] <- length(model_med_0.2log$coefficients)-1
Penalty_results_matrix["num_features", '0.3log(n)'] <- length(model_med_0.3log$coefficients)-1
Penalty_results_matrix["num_features", '0.4log(n)'] <- length(model_med_0.4log$coefficients)-1
Penalty_results_matrix["num_features", '0.5log(n)'] <- length(model_med_0.5log$coefficients)-1
Penalty_results_matrix["num_features", '0.6log(n)'] <- length(model_med_0.6log$coefficients)-1
Penalty_results_matrix["num_features", '0.7log(n)'] <- length(model_med_0.7log$coefficients)-1
Penalty_results_matrix["num_features", '0.8log(n)'] <- length(model_med_0.8log$coefficients)-1
Penalty_results_matrix["num_features", '0.9log(n)'] <- length(model_med_0.9log$coefficients)-1
Penalty_results_matrix["num_features", '1.0log(n)'] <- length(model_med_1.0log$coefficients)-1
Penalty_results_matrix["num_features", '1.1log(n)'] <- length(model_med_1.1log$coefficients)-1
Penalty_results_matrix["num_features", '1.2log(n)'] <- length(model_med_1.2log$coefficients)-1
Penalty_results_matrix["num_features", '1.3log(n)'] <- length(model_med_1.3log$coefficients)-1
Penalty_results_matrix["num_features", '1.4log(n)'] <- length(model_med_1.4log$coefficients)-1

Penalty_results_matrix["train_MSE", '0.1log(n)'] <- mse(predict(model_med_0.1log),y_train)
Penalty_results_matrix["train_MSE", '0.2log(n)'] <- mse(predict(model_med_0.2log),y_train)
Penalty_results_matrix["train_MSE", '0.3log(n)'] <- mse(predict(model_med_0.3log),y_train)
Penalty_results_matrix["train_MSE", '0.4log(n)'] <- mse(predict(model_med_0.4log),y_train)
Penalty_results_matrix["train_MSE", '0.5log(n)'] <- mse(predict(model_med_0.5log),y_train)
Penalty_results_matrix["train_MSE", '0.6log(n)'] <- mse(predict(model_med_0.6log),y_train)
Penalty_results_matrix["train_MSE", '0.7log(n)'] <- mse(predict(model_med_0.7log),y_train)
Penalty_results_matrix["train_MSE", '0.8log(n)'] <- mse(predict(model_med_0.8log),y_train)
Penalty_results_matrix["train_MSE", '0.9log(n)'] <- mse(predict(model_med_0.9log),y_train)
Penalty_results_matrix["train_MSE", '1.0log(n)'] <- mse(predict(model_med_1.0log),y_train)
Penalty_results_matrix["train_MSE", '1.1log(n)'] <- mse(predict(model_med_1.1log),y_train)
Penalty_results_matrix["train_MSE", '1.2log(n)'] <- mse(predict(model_med_1.2log),y_train)
Penalty_results_matrix["train_MSE", '1.3log(n)'] <- mse(predict(model_med_1.3log),y_train)
Penalty_results_matrix["train_MSE", '1.4log(n)'] <- mse(predict(model_med_1.4log),y_train)

Penalty_results_matrix["test_MSE", '0.1log(n)'] <- mse(predict(model_med_0.1log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.2log(n)'] <- mse(predict(model_med_0.2log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.3log(n)'] <- mse(predict(model_med_0.3log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.4log(n)'] <- mse(predict(model_med_0.4log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.5log(n)'] <- mse(predict(model_med_0.5log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.6log(n)'] <- mse(predict(model_med_0.6log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.7log(n)'] <- mse(predict(model_med_0.7log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.8log(n)'] <- mse(predict(model_med_0.8log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '0.9log(n)'] <- mse(predict(model_med_0.9log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.0log(n)'] <- mse(predict(model_med_1.0log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.1log(n)'] <- mse(predict(model_med_1.1log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.2log(n)'] <- mse(predict(model_med_1.2log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.3log(n)'] <- mse(predict(model_med_1.3log,newdata = x_test),y_test)
Penalty_results_matrix["test_MSE", '1.4log(n)'] <- mse(predict(model_med_1.4log,newdata = x_test),y_test)

penalty_plot_m = rbind(Penalty_results_matrix, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4))



png("penalty_med.png",width = 1200, height = 900, res = 200)
par(mar = c(5, 5, 4, 2) + 0.1, cex.axis = 0.6) # Adjust the margins to make room for labels
plot(penalty_plot_m[4,], penalty_plot_m[1,], ylim = c(6.2e+16, 1.1e+17), main = "Stepwise Penalty Comparison, Strating: Middle Model", col = "blue", pch = 19, xaxt = "n", yaxt = 'n', xlab='Penalty',ylab='MSE')
axis(side = 1, at = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4), labels = colnames(penalty_plot_m))
axis(side = 2, las = 1)
points(penalty_plot_m[4,], penalty_plot_m[3,], col = "red", pch = 19)
points(penalty_plot_m[4,], penalty_plot_m[2,], col = "blue", pch = 19)
legend("topright", legend = c("Train", "Test"), col = c("blue", "red"), pch = 19,cex = 0.75)
grid()
dev.off()


# ------------------- plot them all ---------------------- #
penalty_plot_total = rbind(penalty_plot_f,penalty_plot_e,penalty_plot_m)

png("penalty_all.png",width = 1100, height = 900, res = 170)
par(mar = c(5, 5, 4, 2) + 0.1, cex.axis = 0.6) # Adjust the margins to make room for labels
plot(penalty_plot_m[4,], penalty_plot_m[1,], ylim = c(6.2e+16, 1.1e+17), main = "Stepwise Penalty Comparison", col = "blue", pch = 19, xaxt = "n", yaxt = 'n', xlab='Penalty',ylab='MSE')
axis(side = 1, at = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4), labels = colnames(penalty_plot_m))
axis(side = 2, las = 1)
points(penalty_plot_e[4,], penalty_plot_e[3,], col = "red", pch = 19)
points(penalty_plot_e[4,], penalty_plot_e[2,], col = "blue", pch = 19)
points(penalty_plot_f[4,], penalty_plot_f[3,], col = "orange", pch = 19)
points(penalty_plot_f[4,], penalty_plot_f[2,], col = "deepskyblue", pch = 19)
points(penalty_plot_m[4,], penalty_plot_m[3,], col = "darkred", pch = 19)
points(penalty_plot_m[4,], penalty_plot_m[2,], col = "darkblue", pch = 19)
legend("bottomright", legend = c("Empty Test","Full Test","Middle Test", "Empty Train", "Full Train", "Middle Train"), 
       col = c("red","orange","darkred","blue","deepskyblue","darkblue"), pch = 19,cex = 0.75)
grid()
dev.off()





results_matrix_plot <- results_matrix[-1,]



# Extract only train and test MSE columns
train_test_mse <- results_matrix_t[, c("train_MSE", "test_MSE")]

# Create bar plot
barplot(results_matrix_plot, beside = TRUE, legend.text = c("Train", "Test"),
        args.legend = list(x=1,y=1), xlab = "Model", ylab = "MSE Value",
        main = "Train/Test MSE Comparison", col = c("lightblue", "salmon"),grid = TRUE)

# Add legend
legend(legend = c("Train", "Test"), fill = c("lightblue", "salmon"))
dev.off()


# Predictions for four models
predictions_emp <- predict(step.model.emp, newdata = x_test)
predictions_full <- predict(step.model.full, newdata = x_test)
predictions_emp_BIC <- predict(step.model.emp.BIC, newdata = x_test)
predictions_full_BIC <- predict(step.model.full.BIC, newdata = x_test)

# Number of Features in Each Model 
num_features_emp <- length(coef(step.model.emp))
num_features_emp_BIC <- length(coef(step.model.emp.BIC))
num_features_full <- length(coef(step.model.full))
num_features_full_BIC <- length(coef(step.model.full.BIC))

# Calculate MSE for both models
mse_emp <- mse(predictions_emp, y_test)
mse_full <- mse(predictions_full, y_test)
mse_emp_BIC <- mse(predictions_emp_BIC, y_test)
mse_full_BIC <- mse(predictions_full_BIC, y_test)

# Print the MSE for both models
print(paste("MSE for step.model.emp:", mse_emp))
print(paste("MSE for step.model.full:", mse_full))


# ---------- Lasso Regression for Model Selection ------------ #

# Perform k-fold cross-validation to find the optimal lambda value
cv_model <- cv.glmnet(as.matrix(x_train), y_train, alpha = 1)
