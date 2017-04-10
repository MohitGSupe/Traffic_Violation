master_data <- read.csv('/Users/mohitsupe/Desktop/Datasets/TVM.csv')
View(master_data)
runApp("my-app")

model <- glm(master_data$involved_personal_injury~master_data$color_of_vehicle, family = binomial)

pred_tit <- function(color_of_vehicle){
  inputdata <- c(color_of_vehicle)
  pred_data <- as.data.frame(t(inputdata))
  colnames(pred_data) <- c("color")
  surv_prob <- predict(model,pred_data, type = "response")
  return(surv_prob)
}