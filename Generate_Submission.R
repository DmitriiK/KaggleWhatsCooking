load(file = "rf.rda")
load(file="submission_data999.rda")


prediction<- predict(rf, submission_data[-1])
submission <- data.frame(submission_data[,1], prediction)
colnames(submission) = c("id", "cuisine")

submission$id = as.character(submission$id)
submission$cuisine = as.character(submission$cuisine)

write.csv(submission, file = "submission.csv",  row.names = FALSE, quote = FALSE)
