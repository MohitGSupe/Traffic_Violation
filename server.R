library(shiny)

master_data <- read.csv('/Users/mohitsupe/Desktop/Datasets/TVM_Updated.csv')
master_data <- as.data.frame(master_data)

model1 <- glm(involved_accident ~ gender_of_driver+ race_of_driver + vehicle_type + color_of_vehicle + manufacturer + driver_with_commercial_license + state_issuing_driving_license, family = binomial, master_data)

pred_model1 <- function(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license){
  inputdata1 <- c(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license)
  pred_data1 <- as.data.frame(t(inputdata1))
  colnames(pred_data1) <- c("Gender", "Race", "Vehicle Type","Color","Manufacturer","Commercial License","State License")
  surv_prob1 <- predict(model1,pred_data1, type = "response")
  return(surv_prob1)
}

shinyServer(
  function(input, output){
    output$prob1 <- renderText({pred_data(input$gender, input$race, input$vehicle_type, input$color, input$manufacturer, input$commercial_license, input$state_license)})
  }
)

model2 <- glm( involved_personal_injury~ gender_of_driver+ race_of_driver + vehicle_type + color_of_vehicle + manufacturer + driver_with_commercial_license + state_issuing_driving_license, family = binomial, master_data)

pred_model2 <- function(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license){
  inputdata2 <- c(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license)
  pred_data2 <- as.data.frame(t(inputdata2))
  colnames(pred_data2) <- c("Gender", "Race", "Vehicle Type","Color","Manufacturer","Commercial License","State License")
  surv_prob2 <- predict(model2,pred_data2, type = "response")
  return(surv_prob2)
}

shinyServer(
  function(input, output){
    output$prob2 <- renderText({pred_data(input$gender, input$race, input$vehicle_type, input$color, input$manufacturer, input$commercial_license, input$state_license)})
  }
)

model3 <- glm( involved_property_damage~ gender_of_driver+ race_of_driver + vehicle_type + color_of_vehicle + manufacturer + driver_with_commercial_license + state_issuing_driving_license, family = binomial, master_data)

pred_model3 <- function(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license){
  inputdata3 <- c(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license)
  pred_data3 <- as.data.frame(t(inputdata3))
  colnames(pred_data3) <- c("Gender", "Race", "Vehicle Type","Color","Manufacturer","Commercial License","State License")
  surv_prob3 <- predict(model3,pred_data3, type = "response")
  return(surv_prob3)
}

shinyServer(
  function(input, output){
    output$prob3 <- renderText({pred_data(input$gender, input$race, input$vehicle_type, input$color, input$manufacturer, input$commercial_license, input$state_license)})
  }
)

model4 <- glm( involved_fatality~ gender_of_driver+ race_of_driver + vehicle_type + color_of_vehicle + manufacturer + driver_with_commercial_license + state_issuing_driving_license, family = binomial, master_data)

pred_model4 <- function(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license){
  inputdata4 <- c(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license)
  pred_data4 <- as.data.frame(t(inputdata4))
  colnames(pred_data4) <- c("Gender", "Race", "Vehicle Type","Color","Manufacturer","Commercial License","State License")
  surv_prob4 <- predict(model4,pred_data4, type = "response")
  return(surv_prob4)
}

shinyServer(
  function(input, output){
    output$prob4 <- renderText({pred_data(input$gender, input$race, input$vehicle_type, input$color, input$manufacturer, input$commercial_license, input$state_license)})
  }
)

model5 <- glm( involved_alcohol~ gender_of_driver+ race_of_driver + vehicle_type + color_of_vehicle + manufacturer + driver_with_commercial_license + state_issuing_driving_license, family = binomial, master_data)

pred_model5 <- function(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license){
  inputdata5 <- c(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license)
  pred_data5 <- as.data.frame(t(inputdata5))
  colnames(pred_data5) <- c("Gender", "Race", "Vehicle Type","Color","Manufacturer","Commercial License","State License")
  surv_prob5 <- predict(model5,pred_data5, type = "response")
  return(surv_prob5)
}

shinyServer(
  function(input, output){
    output$prob5 <- renderText({pred_data(input$gender, input$race, input$vehicle_type, input$color, input$manufacturer, input$commercial_license, input$state_license)})
  }
)

model6 <- glm( violation_in_work_zone~ gender_of_driver+ race_of_driver + vehicle_type + color_of_vehicle + manufacturer + driver_with_commercial_license + state_issuing_driving_license, family = binomial, master_data)

pred_model6 <- function(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license){
  inputdata6 <- c(gender, race, vehicle_type, color, manufacturer, commercial_license, state_license)
  pred_data6 <- as.data.frame(t(inputdata6))
  colnames(pred_data6) <- c("Gender", "Race", "Vehicle Type","Color","Manufacturer","Commercial License","State License")
  surv_prob6 <- predict(model6,pred_data6, type = "response")
  return(surv_prob6)
}

shinyServer(
  function(input, output){
    output$prob6 <- renderText({pred_data(input$gender, input$race, input$vehicle_type, input$color, input$manufacturer, input$commercial_license, input$state_license)})
  }
)