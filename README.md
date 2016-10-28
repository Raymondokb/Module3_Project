# Module3_Project

1. First row bind the train and test data, as well as their indicator columns on type of activity and which participant. 
2. Then extract the columns/measurements (along with activty and participant) which have 'mean' or 'sd'. I did so by using which() and applying it on a transposed matrix (and then transposing the subsetted matrix back)
3. Then, separate the matrix by each of the six activities
4. Then, for each participant, calculate the column means of the measurements and store in a separate matrix
5. At the end, rbind all the six separate matrices and sort them by participant


In each step, the name of measurement as well as each of the six activities was recorded
