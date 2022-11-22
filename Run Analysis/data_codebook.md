CODEBOOK

The dataset is a human Activity Recognition database built from the recordings of 30 subjects. The experiment shows volunteers performing various activities such as walking, walking upstairs, walking downstairs, sitting, standing, and laying. A smartphone (Samsung Galaxy S II) was used to capture data from the differnt physical actions of each volunteer.

Each record in the dataset provides:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope.
- A 561-feature vector with time and frequency domain variables.
- Its activity label.
- An identifier of the subject who carried out the experiment.


The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.


For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The original files in the document were:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

These files were then combined into one dataset with all the collective information.
Afterwards, R functions were used to substitute certain phrases in the data and make the information tidy and easier to read or understand.


The code is provided to clean the data and replace certain columns names and keep the information for the mean and the standard deviation.
At the end of the code, there will be columns describing 
The Subject
Type of Activity
Body Accelerometer Means from our datasets
Body Accelerometer Standard Deviations from our datasets
Gravity Accelerometer Means
Gravity Accelerometer Standard Deviations
Body Accelerometer Jerk Signal Mean
Body Accelerometer Jerk Signal Standard Deviation
Gyroscope Mean
Cyroscope Standard Deviations
Gyroscope Jerk Signal Means
Gyroscope Jerk Signal Standard Deviations
Body Accelerator Euclidean norm Mean
Body Accelerator Euclidian norm standard deviations
Gravity Accelerometer Euclidean Norm Mean
Gravity Accelerometer Euclidean Norm Standard Deviation
Body Accelerometer Jerk Signal Euclidean Norm Mean
Body Accelerometer Jerk Signal Euclidean Norm Standard Deviation
Gyroscope Euclidean Norm Magnitude Mean
Gyroscope Euclidean Norm Standard Deviation
Gyroscope Jerk Signal Euclidean Norm Mean
Gyroscope Jerk Signal Euclidean Norm Standard Deviation
Frequency Body Accelerometer Mean
Frequency Body ACcelerometer Standard Deviation
Frequency Body Accelerometer Jerk Signal Mean
Frequency Body Accelerometer Jerk Signals Standard Deviation
Frequency Body Gyroscope Mean
Frequency Body Gyroscope Standard Deviation
Frequency Body Gyroscope Standard Deviation
Frequency Body Accelerometer Euclidean Norm Standard Deviation
Frequency Body ACcelerometer Euclidean Norm Mean
Frequency Body ACcelerometer Jerk Euclidean Norm Standard Deviation
Frequency Body Accelerometer Gyroscope Euclidean Norm Mean
Frequency Body Accelerometer Gyroscope Euclidean Norm Standard Deviation
Frequency BOdy Accelerometer Gyroscope Jerk Euclidean Norm Mean
Frequency Body Accelerometer Gyroscope Jerk Euclidean Norm Standard Deviation




