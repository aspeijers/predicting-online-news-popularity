1) news_.... - The data from the competition 

2) action_recommend.csv - outputs from the most robust accuracy function 

3) train49.csv and test49.csv - xgboost 49 features matrices. 

4) train50.csv and test50.csv - gbm 50 features matrices. 

5) Test_pre_final.csv and Train_pre_final.csv - features matrices that contain selected interaction terms and original features that are common among xgboost 49 and gbm 50 matrices. 

6) train_dciweekday.csv and Test_dciweekday.csv - features matrices that contain the generated dci features and weekday features (all 4). 

7) training70.csv and test30.csv - the features matrices generated from the training dataset to be used for multinom function. 

8) XX_anova_train2.csv and XX_anov_test2.csv - interaction terms generated before the interim evaluation. 

9) XX_anov_train3.csv and XX_anov_test3.csv - interation terms generated towards the end and used in our final submissions. 

10) train & test datasets with _accuracy - all can be ignored, the results were poor in comparison to the 2 final features matrices. 