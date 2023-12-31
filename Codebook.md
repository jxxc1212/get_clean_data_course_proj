Data Table 1: Merge_TT_select
Description:  This data contains the Selected columns from both Train and Test data
Creation: This data is created using the following method:
1. combine  /train/X_train.txt with y_train.txt (act_label) and subject_train.txt (SUB_NUM)
2. combine  /test/X_test.txt with y_test.txt (act_label) and subject_test.txt (SUB_NUM)
3. use a new column named (Src) to label the source of these two data as "TRAIN" or "TEST"
4. perform row bind on the TRAIN and the TEST tables to merge them.
5. use features.txt to get the features with the text string "mean(" and "std(" only.
6. use the index from features.txt to select only desired features from the merged data.
7. use the feature names from features.txt to make descriptive names for the merged data.
8. use activity_labels.txt to add a new column (activity) to label the description for each activity.
9. reorder them as follow for readability.

Variables and data dimensions:
Classes ‘data.table’ and 'data.frame':	10299 obs. of  70 variables:
 $ Src                      : chr  "TRAIN" "TRAIN" "TRAIN" "TRAIN" ...
 $ SUB_NUM                  : int  1 1 1 1 1 1 1 1 1 1 ...
 $ act_label                : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity                 : chr  "WALKING" "WALKING" "WALKING" "WALKING" ...
 $ tBodyAcc_mean_X          : num  0.282 0.256 0.255 0.343 0.276 ...
 $ tBodyAcc_mean_Y          : num  -0.0377 -0.06455 0.00381 -0.01445 -0.02964 ...
 $ tBodyAcc_mean_Z          : num  -0.1349 -0.0952 -0.1237 -0.1674 -0.1426 ...
 $ tBodyAcc_std_X           : num  -0.328 -0.229 -0.275 -0.23 -0.227 ...
 $ tBodyAcc_std_Y           : num  -0.1372 0.0165 0.0131 0.1739 0.1643 ...
 $ tBodyAcc_std_Z           : num  -0.189 -0.26 -0.284 -0.213 -0.123 ...
 $ tGravityAcc_mean_X       : num  0.945 0.941 0.946 0.952 0.947 ...
 $ tGravityAcc_mean_Y       : num  -0.246 -0.252 -0.264 -0.26 -0.257 ...
 $ tGravityAcc_mean_Z       : num  -0.0322 -0.0329 -0.0256 -0.0261 -0.0284 ...
 $ tGravityAcc_std_X        : num  -0.984 -0.984 -0.963 -0.981 -0.977 ...
 $ tGravityAcc_std_Y        : num  -0.929 -0.917 -0.956 -0.964 -0.989 ...
 $ tGravityAcc_std_Z        : num  -0.933 -0.949 -0.972 -0.964 -0.96 ...
 $ tBodyAccJerk_mean_X      : num  -0.156 -0.208 0.202 0.336 -0.236 ...
 $ tBodyAccJerk_mean_Y      : num  -0.143 0.358 0.417 -0.464 -0.112 ...
 $ tBodyAccJerk_mean_Z      : num  -0.11308 -0.4524 0.13908 -0.00503 0.17265 ...
 $ tBodyAccJerk_std_X       : num  -0.184 -0.108 -0.178 -0.12 -0.192 ...
 $ tBodyAccJerk_std_Y       : num  -0.1705 -0.0187 -0.0296 0.0287 0.054 ...
 $ tBodyAccJerk_std_Z       : num  -0.614 -0.548 -0.58 -0.521 -0.469 ...
 $ tBodyGyro_mean_X         : num  -0.47973 0.09409 0.2112 0.09608 0.00874 ...
 $ tBodyGyro_mean_Y         : num  0.082 -0.3092 -0.2729 -0.1634 0.0117 ...
 $ tBodyGyro_mean_Z         : num  0.25644 0.08644 0.10199 0.02586 0.00417 ...
 $ tBodyGyro_std_X          : num  -0.324 -0.399 -0.445 -0.36 -0.378 ...
 $ tBodyGyro_std_Y          : num  -0.1419 -0.0884 -0.0631 0.0423 0.1337 ...
 $ tBodyGyro_std_Z          : num  -0.457 -0.402 -0.347 -0.276 -0.308 ...
 $ tBodyGyroJerk_mean_X     : num  0.0942 0.1667 -0.1632 -0.0546 -0.0757 ...
 $ tBodyGyroJerk_mean_Y     : num  -0.47621 -0.0338 -0.00556 0.34029 0.17147 ...
 $ tBodyGyroJerk_mean_Z     : num  -0.1421 -0.0893 -0.2316 -0.2697 0.1365 ...
 $ tBodyGyroJerk_std_X      : num  -0.346 -0.25 -0.264 -0.102 -0.129 ...
 $ tBodyGyroJerk_std_Y      : num  -0.487 -0.454 -0.425 -0.243 -0.19 ...
 $ tBodyGyroJerk_std_Z      : num  -0.422 -0.37 -0.343 -0.312 -0.418 ...
 $ tBodyAccMag_mean         : num  -0.2246 -0.1265 -0.1601 -0.0735 -0.0495 ...
 $ tBodyAccMag_std          : num  -0.238 -0.213 -0.258 -0.195 -0.211 ...
 $ tGravityAccMag_mean      : num  -0.2246 -0.1265 -0.1601 -0.0735 -0.0495 ...
 $ tGravityAccMag_std       : num  -0.238 -0.213 -0.258 -0.195 -0.211 ...
 $ tBodyAccJerkMag_mean     : num  -0.289 -0.139 -0.194 -0.129 -0.16 ...
 $ tBodyAccJerkMag_std      : num  -0.165 -0.199 -0.22 -0.174 -0.15 ...
 $ tBodyGyroMag_mean        : num  -0.0344 -0.1409 -0.0946 -0.0493 -0.0214 ...
 $ tBodyGyroMag_std         : num  -0.1682 -0.2161 -0.2908 -0.0901 -0.0446 ...
 $ tBodyGyroJerkMag_mean    : num  -0.466 -0.39 -0.374 -0.236 -0.22 ...
 $ tBodyGyroJerkMag_std     : num  -0.434 -0.439 -0.418 -0.229 -0.213 ...
 $ fBodyAcc_mean_X          : num  -0.261 -0.151 -0.23 -0.151 -0.226 ...
 $ fBodyAcc_mean_Y          : num  -0.1226 -0.029 0.0254 0.1953 0.1103 ...
 $ fBodyAcc_mean_Z          : num  -0.331 -0.257 -0.377 -0.321 -0.205 ...
 $ fBodyAcc_std_X           : num  -0.357 -0.262 -0.294 -0.263 -0.227 ...
 $ fBodyAcc_std_Y           : num  -0.1996 -0.0239 -0.0577 0.0879 0.1188 ...
 $ fBodyAcc_std_Z           : num  -0.178 -0.322 -0.29 -0.217 -0.146 ...
 $ fBodyAccJerk_mean_X      : num  -0.21 -0.178 -0.193 -0.183 -0.285 ...
 $ fBodyAccJerk_mean_Y      : num  -0.2635 -0.1208 -0.1096 -0.026 -0.0111 ...
 $ fBodyAccJerk_mean_Z      : num  -0.536 -0.499 -0.526 -0.487 -0.426 ...
 $ fBodyAccJerk_std_X       : num  -0.228 -0.114 -0.236 -0.132 -0.169 ...
 $ fBodyAccJerk_std_Y       : num  -0.12427 0.02785 -0.00582 0.02037 0.05578 ...
 $ fBodyAccJerk_std_Z       : num  -0.698 -0.595 -0.633 -0.553 -0.51 ...
 $ fBodyGyro_mean_X         : num  -0.185 -0.205 -0.317 -0.162 -0.237 ...
 $ fBodyGyro_mean_Y         : num  -0.198 -0.2458 -0.2082 0.0266 0.0472 ...
 $ fBodyGyro_mean_Z         : num  -0.308 -0.311 -0.186 -0.18 -0.258 ...
 $ fBodyGyro_std_X          : num  -0.368 -0.461 -0.486 -0.423 -0.422 ...
 $ fBodyGyro_std_Y          : num  -0.11505 -0.00984 0.00973 0.04465 0.17602 ...
 $ fBodyGyro_std_Z          : num  -0.565 -0.49 -0.469 -0.377 -0.389 ...
 $ fBodyAccMag_mean         : num  -0.1668 -0.0793 -0.1563 -0.1044 -0.1232 ...
 $ fBodyAccMag_std          : num  -0.4 -0.423 -0.437 -0.376 -0.388 ...
 $ fBodyBodyAccJerkMag_mean : num  -0.154 -0.178 -0.149 -0.132 -0.116 ...
 $ fBodyBodyAccJerkMag_std  : num  -0.185 -0.231 -0.321 -0.233 -0.201 ...
 $ fBodyBodyGyroMag_mean    : num  -0.22218 -0.26828 -0.30867 -0.06013 -0.00382 ...
 $ fBodyBodyGyroMag_std     : num  -0.274 -0.315 -0.401 -0.275 -0.246 ...
 $ fBodyBodyGyroJerkMag_mean: num  -0.432 -0.428 -0.401 -0.218 -0.188 ...
 $ fBodyBodyGyroJerkMag_std : num  -0.476 -0.493 -0.482 -0.299 -0.3 ...
 
 
 
 
Data Table 2: SUMM_TT_select
Description:  This data summarized Merge_TT_select by grouping the data using:
				Src, SUB_NUM, act_label, activity
				the summary provides the means for all other numeric features.

Creation: This data is created using the following method:
1. perform Group_by() on Merge_TT_select by Src, SUB_NUM, act_label, activity
2. use summarise_all() to calculate the mean for all selected feature.

Variables and data dimensions:
gropd_df [180 × 70] (S3: grouped_df/tbl_df/tbl/data.frame)
 $ Src                      : chr [1:180] "TEST" "TEST" "TEST" "TEST" ...
 $ SUB_NUM                  : int [1:180] 2 2 2 2 2 2 4 4 4 4 ...
 $ act_label                : int [1:180] 1 2 3 4 5 6 1 2 3 4 ...
 $ activity                 : chr [1:180] "WALKING" "WALKING_UPSTAIRS" "WALKING_DOWNSTAIRS" "SITTING" ...
 $ tBodyAcc_mean_X          : num [1:180] 0.276 0.247 0.278 0.277 0.278 ...
 $ tBodyAcc_mean_Y          : num [1:180] -0.0186 -0.0214 -0.0227 -0.0157 -0.0184 ...
 $ tBodyAcc_mean_Z          : num [1:180] -0.106 -0.153 -0.117 -0.109 -0.106 ...
 $ tBodyAcc_std_X           : num [1:180] -0.4236 -0.3044 0.0464 -0.9868 -0.9873 ...
 $ tBodyAcc_std_Y           : num [1:180] -0.0781 0.108 0.2629 -0.9507 -0.9573 ...
 $ tBodyAcc_std_Z           : num [1:180] -0.425 -0.112 -0.103 -0.96 -0.95 ...
 $ tGravityAcc_mean_X       : num [1:180] 0.913 0.791 0.862 0.94 0.897 ...
 $ tGravityAcc_mean_Y       : num [1:180] -0.347 -0.416 -0.326 -0.106 -0.37 ...
 $ tGravityAcc_mean_Z       : num [1:180] 0.0847 -0.1959 -0.0439 0.1987 0.1297 ...
 $ tGravityAcc_std_X        : num [1:180] -0.973 -0.934 -0.94 -0.98 -0.987 ...
 $ tGravityAcc_std_Y        : num [1:180] -0.972 -0.924 -0.94 -0.957 -0.974 ...
 $ tGravityAcc_std_Z        : num [1:180] -0.972 -0.878 -0.931 -0.954 -0.946 ...
 $ tBodyAccJerk_mean_X      : num [1:180] 0.0618 0.0745 0.11 0.0723 0.0748 ...
 $ tBodyAccJerk_mean_Y      : num [1:180] 0.01825 -0.00971 -0.00328 0.0117 0.01033 ...
 $ tBodyAccJerk_mean_Z      : num [1:180] 0.0079 0.01948 -0.02094 0.00761 -0.00837 ...
 $ tBodyAccJerk_std_X       : num [1:180] -0.278 -0.276 0.147 -0.988 -0.981 ...
 $ tBodyAccJerk_std_Y       : num [1:180] -0.0166 -0.1856 0.1268 -0.978 -0.9711 ...
 $ tBodyAccJerk_std_Z       : num [1:180] -0.586 -0.574 -0.34 -0.988 -0.983 ...
 $ tBodyGyro_mean_X         : num [1:180] -0.053 -0.0577 -0.1159 -0.0455 -0.0239 ...
 $ tBodyGyro_mean_Y         : num [1:180] -0.04824 -0.03209 -0.00482 -0.05993 -0.08204 ...
 $ tBodyGyro_mean_Z         : num [1:180] 0.0828 0.0688 0.0972 0.0412 0.0878 ...
 $ tBodyGyro_std_X          : num [1:180] -0.562 -0.439 -0.321 -0.986 -0.973 ...
 $ tBodyGyro_std_Y          : num [1:180] -0.538 -0.466 -0.416 -0.979 -0.971 ...
 $ tBodyGyro_std_Z          : num [1:180] -0.481 -0.164 -0.279 -0.96 -0.965 ...
 $ tBodyGyroJerk_mean_X     : num [1:180] -0.0819 -0.0829 -0.0581 -0.0936 -0.1056 ...
 $ tBodyGyroJerk_mean_Y     : num [1:180] -0.0538 -0.0424 -0.0421 -0.0416 -0.0422 ...
 $ tBodyGyroJerk_mean_Z     : num [1:180] -0.0515 -0.0445 -0.071 -0.0436 -0.0547 ...
 $ tBodyGyroJerk_std_X      : num [1:180] -0.39 -0.465 -0.244 -0.99 -0.979 ...
 $ tBodyGyroJerk_std_Y      : num [1:180] -0.634 -0.645 -0.469 -0.991 -0.983 ...
 $ tBodyGyroJerk_std_Z      : num [1:180] -0.435 -0.468 -0.218 -0.986 -0.974 ...
 $ tBodyAccMag_mean         : num [1:180] -0.29 -0.107 0.09 -0.968 -0.966 ...
 $ tBodyAccMag_std          : num [1:180] -0.423 -0.206 0.216 -0.953 -0.958 ...
 $ tGravityAccMag_mean      : num [1:180] -0.29 -0.107 0.09 -0.968 -0.966 ...
 $ tGravityAccMag_std       : num [1:180] -0.423 -0.206 0.216 -0.953 -0.958 ...
 $ tBodyAccJerkMag_mean     : num [1:180] -0.28142 -0.32127 0.00566 -0.98677 -0.98049 ...
 $ tBodyAccJerkMag_std      : num [1:180] -0.164 -0.217 0.23 -0.984 -0.977 ...
 $ tBodyGyroMag_mean        : num [1:180] -0.447 -0.22 -0.162 -0.946 -0.963 ...
 $ tBodyGyroMag_std         : num [1:180] -0.553 -0.378 -0.275 -0.961 -0.954 ...
 $ tBodyGyroJerkMag_mean    : num [1:180] -0.548 -0.573 -0.411 -0.991 -0.984 ...
 $ tBodyGyroJerkMag_std     : num [1:180] -0.558 -0.597 -0.343 -0.99 -0.977 ...
 $ fBodyAcc_mean_X          : num [1:180] -0.346 -0.267 0.113 -0.986 -0.984 ...
 $ fBodyAcc_mean_Y          : num [1:180] -0.0219 0.00992 0.27835 -0.95734 -0.95987 ...
 $ fBodyAcc_mean_Z          : num [1:180] -0.454 -0.281 -0.131 -0.97 -0.962 ...
 $ fBodyAcc_std_X           : num [1:180] -0.4577 -0.3206 0.0161 -0.9874 -0.9891 ...
 $ fBodyAcc_std_Y           : num [1:180] -0.1692 0.0849 0.172 -0.9501 -0.9579 ...
 $ fBodyAcc_std_Z           : num [1:180] -0.4552 -0.0945 -0.162 -0.9569 -0.9464 ...
 $ fBodyAccJerk_mean_X      : num [1:180] -0.305 -0.259 0.138 -0.988 -0.981 ...
 $ fBodyAccJerk_mean_Y      : num [1:180] -0.0788 -0.1878 0.0962 -0.9771 -0.9709 ...
 $ fBodyAccJerk_mean_Z      : num [1:180] -0.555 -0.523 -0.271 -0.985 -0.98 ...
 $ fBodyAccJerk_std_X       : num [1:180] -0.314 -0.365 0.05 -0.989 -0.983 ...
 $ fBodyAccJerk_std_Y       : num [1:180] -0.0153 -0.2436 0.0808 -0.9808 -0.9735 ...
 $ fBodyAccJerk_std_Z       : num [1:180] -0.616 -0.625 -0.408 -0.989 -0.985 ...
 $ fBodyGyro_mean_X         : num [1:180] -0.43 -0.332 -0.146 -0.983 -0.967 ...
 $ fBodyGyro_mean_Y         : num [1:180] -0.555 -0.488 -0.362 -0.982 -0.973 ...
 $ fBodyGyro_mean_Z         : num [1:180] -0.3967 -0.2486 -0.0875 -0.9598 -0.9606 ...
 $ fBodyGyro_std_X          : num [1:180] -0.604 -0.476 -0.379 -0.987 -0.975 ...
 $ fBodyGyro_std_Y          : num [1:180] -0.533 -0.46 -0.459 -0.977 -0.971 ...
 $ fBodyGyro_std_Z          : num [1:180] -0.56 -0.218 -0.423 -0.964 -0.97 ...
 $ fBodyAccMag_mean         : num [1:180] -0.324 -0.145 0.293 -0.961 -0.964 ...
 $ fBodyAccMag_std          : num [1:180] -0.5771 -0.3667 -0.0215 -0.9556 -0.9605 ...
 $ fBodyBodyAccJerkMag_mean : num [1:180] -0.169 -0.19 0.222 -0.984 -0.977 ...
 $ fBodyBodyAccJerkMag_std  : num [1:180] -0.164 -0.26 0.227 -0.984 -0.975 ...
 $ fBodyBodyGyroMag_mean    : num [1:180] -0.531 -0.451 -0.321 -0.972 -0.962 ...
 $ fBodyBodyGyroMag_std     : num [1:180] -0.652 -0.439 -0.373 -0.961 -0.957 ...
 $ fBodyBodyGyroJerkMag_mean: num [1:180] -0.583 -0.601 -0.38 -0.99 -0.978 ...
 $ fBodyBodyGyroJerkMag_std : num [1:180] -0.558 -0.622 -0.344 -0.99 -0.978 ...
