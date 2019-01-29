# Strava Activity Classification
In this project, I developed a model to classify activities (swimming, biking, running, and walking) using a collection of GPS-tracked activities to create covariates. The covariates consisted of variables detailing speed, distance, elevation change, etc., and there were ~700 runs, ~600 bikes, ~70 swims, and ~30 walks total.

The R-package glmnet was used for multinomical regression with ridge regression. The package runs an iterative scheme to select a λ (penalization coefficient) that yields the lowest cross-validation error (use: cv.glmnet); I used 10-folds cross-validation. The objective function that is minimized is the -log of the likelihood. See ⁨Strava-Activity-Classification/II_WriteUps⁩/ProjectWriteUp_3Parts.pdf for full details.

The training accuracy of the multinomial regression model was 98.57%, the test accuracy was 83.24%, but the baseline model (always guessing run as the activity, since it had the highest occurrence), was only 25% accurate. 

