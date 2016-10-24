require(plyr)

# Defines directories and files from downloaded raw data
uci_har <- "UCI\ HAR\ Dataset"
feature_file <- paste(uci_har, "/features.txt", sep = "")
labels_f <- paste(uci_har, "/activity_labels.txt", sep = "")
train_f<- paste(uci_har, "/train/X_train.txt", sep = "")
y_train_f <- paste(uci_har, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_har, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_har, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_har, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_har, "/test/subject_test.txt", sep = "")

# Load and assigsn raw data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(labels_f, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_f)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

# Binding test sensor and sensor  data by row and columns
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

#Define measurements on the mean and standard deviation for each measurement.

sensor_data_mean <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]
# Uses descriptive activity names to name the activities in the data set

sensor_data_mean<- join(sensor_data_mean, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean <- sensor_data_mean[,-1]

# Subs columns, lables and etc

names(sensor_data_mean) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
names(sensor_data_mean) <- make.names(names(sensor_data_mean_std))
names(sensor_data_mean) <- gsub('Acc',"Acceleration",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Mag',"Magnitude",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('^t',"TimeDomain.",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('\\.mean',".Mean",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Freq$',"Frequency",names(sensor_data_mean))

#Creates the final  tidy data

sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "sensor_avg_by_act_sub.txt")
