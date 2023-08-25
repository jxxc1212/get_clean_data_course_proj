############################################################################
# name this program: run_analysis.R                                    
############################################################################
library(data.table)

# it should do the following: 
# 
# 1. Merges the training and the test sets to create one data set.
#    Read in data from C:\Users\Jason Chen\OneDrive\Desktop\DATA\UCI HAR Dataset
    X_train <- data.table::fread(paste(getwd(),"DATA/UCI HAR Dataset/train/X_train.txt",sep="/"))
    Y_train <- data.table::fread(paste(getwd(),"DATA/UCI HAR Dataset/train/y_train.txt",sep="/"), col.names = "act_label")
    S_train <- data.table::fread(paste(getwd(),"DATA/UCI HAR Dataset/train/subject_train.txt",sep="/"), col.names = "SUB_NUM")
    TRAIN <-cbind(X_train, Y_train, S_train)
    TRAIN$Src <- "TRAIN"
    
    X_test  <- data.table::fread(paste(getwd(),"DATA/UCI HAR Dataset/test/X_test.txt",sep="/"))
    Y_test <- data.table::fread(paste(getwd(),"DATA/UCI HAR Dataset/test/y_test.txt",sep="/"), col.names = "act_label")
    S_test <- data.table::fread(paste(getwd(),"DATA/UCI HAR Dataset/test/subject_test.txt",sep="/"), col.names = "SUB_NUM")
    TEST <-cbind(X_test, Y_test, S_test)    
    TEST$Src <- "TEST"  
    
    Merge_TT <-rbind(TRAIN, TEST)
    
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#    Read in data from features.txt to get mean and std features
    features <- fread(paste(getwd(),"DATA/UCI HAR Dataset/features.txt",sep="/"),col.names = c("index", "feature"))
    featureskeep <- features[grep("(mean|std)\\(", features$feature),]
    feature_index_vec <- featureskeep[,index] 
    feature_index_vec <- as.character(paste0("V",feature_index_vec))  

    Merge_TT_select <- select(Merge_TT, all_of(feature_index_vec), SUB_NUM, act_label, Src)

# 3. Uses descriptive activity names to name the activities in the data set
#    read in data from activity_labels.txt to label activity names
    activityLabels <- fread(paste(getwd(),"DATA/UCI HAR Dataset/activity_labels.txt",sep="/"),col.names = c("act_label", "activity"))
    Merge_TT_select <-merge(Merge_TT_select,activityLabels)
    
# 4. Appropriately labels the data set with descriptive variable names. 
#    reuse the featureskeep above to derive the descriptive names
    feature_name_vec <- featureskeep[,feature] 
    feature_name_vec <- gsub("[()]", "",feature_name_vec)
    feature_name_vec <- gsub("-", "_",feature_name_vec) 
    feature_name_vec_all <- c("Src","SUB_NUM","act_label","activity", feature_name_vec)
    
    Merge_TT_select <- select(Merge_TT_select, Src, SUB_NUM, act_label, activity, all_of(feature_index_vec))
    data.table::setnames(Merge_TT_select, colnames(Merge_TT_select), feature_name_vec_all)

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
    GRP_TT_select <- group_by(Merge_TT_select, Src, SUB_NUM, act_label, activity)
    SUMM_TT_select <-summarise_all(GRP_TT_select,mean)
