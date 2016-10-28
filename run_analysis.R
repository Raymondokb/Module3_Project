#setwd("D:\\Users\\user\\Downloads\\getdata_2Fprojectfiles%2FUCI HAR Dataset\\UCI HAR Dataset\\")
activity_label <- read.table("activity_labels.txt",stringsAsFactors = FALSE)
features <- read.table("features.txt", stringsAsFactors = FALSE)
#D:\\Users\\user\\Downloads\\getdata_2Fprojectfiles%2FUCI HAR Dataset\\UCI HAR Dataset\\
test_subject_test <- read.table("test\\subject_test.txt", stringsAsFactors = FALSE)
test_x_test <- read.table( "test\\X_test.txt", stringsAsFactors = FALSE)
test_y_test <- read.table("test\\y_test.txt", stringsAsFactors = FALSE)


train_subject_train <- read.table("train\\subject_train.txt", stringsAsFactors = FALSE)
train_x_train <- read.table("train\\X_train.txt", stringsAsFactors = FALSE)
train_y_train <- read.table("train\\y_train.txt", stringsAsFactors =FALSE)

grep("mean",features$V2)
grep("std", features$V2)

#Story time
# To get each participant's each activity's mean of the ~79 measurements with gyro having 'mean' or 'sd'



test_x_test <- cbind(test_x_test,test_subject_test)
train_x_train <- cbind(train_x_train, train_subject_train)
colnames(test_x_test)[562] <- "Participants"
colnames(train_x_train)[562] <- "Participants" #get participants

for(i in 1:dim(test_y_test)[1]){
  test_y_test$descript[i] = activity_label[test_y_test[[1]][i],2]
}
for(i in 1:dim(train_y_train)[1]){
  train_y_train$descript[i] = activity_label[train_y_train[[1]][i],2]
}

#Now, time to play

all.data <- rbind(train_x_train, test_x_test)
grep("mean",features$V2)
grep("std", features$V2)
union_index <- union(grep("mean",features$V2),grep("std", features$V2 ) )
# now to append the particpants column
union_index <- append(union_index, 562)

t_all.data <- t(all.data)
subsetted <- t_all.data[union_index,]
all.data2 <- as.data.frame(t(subsetted))

indicators.full <- rbind(train_y_train, test_y_test)
all.data2 <- cbind(all.data2, indicators.full$descript)

colnames(all.data2) <- c(features[union_index,2],"Description")
colnames(all.data2)[80] <- "Participants"
#"WALKING" etc
all.data2Walk <- all.data2[which(all.data2$Description=="WALKING"),]
all.data2walkup <- all.data2[which(all.data2$Description=="WALKING_UPSTAIRS"),]
all.data2walkdown <- all.data2[which(all.data2$Description=="WALKING_DOWNSTAIRS"),]
all.data2sit <- all.data2[which(all.data2$Description=="SITTING"),]
all.data2stand <- all.data2[which(all.data2$Description=="STANDING"),]
all.data2lay <- all.data2[which(all.data2$Description=="LAYING"),]

#now to do just walking. a by participants by activity mean. so a 30 x #features table
#to create an empty matrix and fill in the table from there?
walking.matrix <- matrix(data=NA, nrow = 30, ncol = (length(union_index)+1) )
colnames(walking.matrix) <- c(features[union(grep("mean",features$V2),grep("std", features$V2 ) ),2],"Participants", "Activity")
for(i in 1:dim(walking.matrix)[1]){
  walking.matrix[i,] <- c(colMeans(all.data2Walk[which(all.data2Walk$Participants==i),1:79]),i,"WALKING")
}

#Walkup, oay 4 more of copy pasting
walkup.matrix <- matrix(data=NA, nrow = 30, ncol = (length(union_index)+1) )
colnames(walkup.matrix) <- c(features[union(grep("mean",features$V2),grep("std", features$V2 ) ),2], "Participants", "Activity")
for(i in 1:dim(walkup.matrix)[1]){
  walkup.matrix[i,] <- c(colMeans(all.data2walkup[which(all.data2walkup$Participants==i),1:79]), i, "WALKING_UPSTAIRS")
}

#walkdown
walkdown.matrix <- matrix(data=NA, nrow = 30, ncol = (length(union_index)+1) )
colnames(walkdown.matrix) <- c(features[union(grep("mean",features$V2),grep("std", features$V2 ) ),2], "Participants", "Activity")
for(i in 1:dim(walkdown.matrix)[1]){
  walkdown.matrix[i,] <- c(colMeans(all.data2walkdown[which(all.data2walkdown$Participants==i),1:79]), i, "WALKING_DOWNSTAIRS")
}
#sit
sit.matrix <- matrix(data=NA, nrow = 30, ncol = (length(union_index)+1) )
colnames(sit.matrix) <- c(features[union(grep("mean",features$V2),grep("std", features$V2 ) ),2], "Participants", "Activity")
for(i in 1:dim(sit.matrix)[1]){
  sit.matrix[i,] <- c(colMeans(all.data2sit[which(all.data2sit$Participants==i),1:79]), i, "SITTING")
}
#stand
stand.matrix <- matrix(data=NA, nrow = 30, ncol = (length(union_index)+1) )
colnames(stand.matrix) <- c(features[union(grep("mean",features$V2),grep("std", features$V2 ) ),2], "Participants", "Activity")
for(i in 1:dim(stand.matrix)[1]){
  stand.matrix[i,] <- c(colMeans(all.data2stand[which(all.data2stand$Participants==i),1:79]), i, "STANDING")
}
#lay
lay.matrix <- matrix(data=NA, nrow = 30, ncol = (length(union_index)+1) )
colnames(lay.matrix) <- c(features[union(grep("mean",features$V2),grep("std", features$V2 ) ),2], "Participants", "Activity")
for(i in 1:dim(lay.matrix)[1]){
  lay.matrix[i,] <- c(colMeans(all.data2lay[which(all.data2lay$Participants==i),1:79]), i, "LAYING")
}

#listing
walking.df <- data.frame(walking.matrix, stringsAsFactors = FALSE)
walkup.df <- data.frame(walkup.matrix, stringsAsFactors = FALSE)
walkdown.df <- data.frame(walkdown.matrix, stringsAsFactors = FALSE)
sit.df <- data.frame(sit.matrix, stringsAsFactors = FALSE)
stand.df <- data.frame(stand.matrix, stringsAsFactors = FALSE)
lay.df <- data.frame(lay.matrix, stringsAsFactors = FALSE)

#Ended up not using this
desired.list <- list("WALKING"=walking.df, "WALKING_UPSTAIRS"=walkup.df, "WALKING_DOWNSTAIRS"=walkdown.df, "SITTING"=sit.df, "STANDING"=stand.df, "LAYING"=lay.df)


desired.set <- rbind(walking.df, walkup.df, walkdown.df, sit.df, stand.df, lay.df)
desired.set$Participants <- as.numeric(desired.set$Participants)
output.set <- desired.set[order(desired.set$Participants),]
output.set <- output.set[,c(80,81,1:79)] #Reorder to be easier for marker


setwd("D:\\Users\\user\\Desktop\\DataScienceCoursera\\Module3\\Module3_Project")
write.table(output.set, "Output.txt", row.names=FALSE)
