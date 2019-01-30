# Strava Activity Classification
In this project, I developed a model to classify activities (swimming, biking, running, and walking) using a collection of GPS-tracked activities to create features (covariates). The covariates consisted of variables detailing speed, distance, elevation change, etc., and there were ~700 runs, ~600 bikes, ~70 swims, and ~30 walks total. When tested on a hold-out set of data, the final model had an average accuracy of 83.2%; average here refers to the average accuracy of each of the activities, i.e. it was 96.9% accurate at classifying rides, 98.6% accurate for runs, 100% for swims, and 37.5% for walks.

Model Details:  The R-package glmnet was used for multinomical regression with ridge regression. The package runs an iterative scheme to select a λ (penalization coefficient) that yields the lowest cross-validation error (use: cv.glmnet); I used 10-folds cross-validation.  The objective function that is minimized is the -log of the likelihood. The training accuracy of the multinomial regression model was 98.6%, the test accuracy was 83.2%, but the baseline model (always guessing run as the activity, since it had the highest occurrence), was only 25% accurate. 
