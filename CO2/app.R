library(shiny)
library(tidyverse)
library(glue)

### CATEGORIES FOR THE INDIVIDUAL CALCULATOR
gender <- c("Male", "Female")
food <- c("High meat consumption",
          "Normal meat consumption",
          "Low meat consumption",
          "Fish-eater",
          "Vegetarian",
          "Vegan")
flight_class <- c("Economy class", "Business class", "First class")
car_size <- c("Small", "Medium", "Large")
car_fuel <- c("Diesel", "Petrol", "Hybrid", "LPG", 
              "Plug-in Hybrid Electric", "Battery Electric")
motor_size <- c("Small", "Medium", "Large")
fashion <- c("Low", "Medium", "High")
gadgets <- c("Low", "Medium", "High")

### TABLES FOR THE INDIVIDUAL CALCULATOR
# Mean greenhouse gas emissions per 2,000 kcal by diet type and sex
food_table <- data.frame(gender = c(rep("Male", 6), rep("Female", 6)),
                         # Recommended daily calorie intake by gender (in kcal)
                         consumption = c(rep(2500, 6), rep(2000, 6)),
                         type = rep(c("High meat consumption",
                                      "Normal meat consumption",
                                      "Low meat consumption",
                                      "Fish-eater",
                                      "Vegetarian",
                                      "Vegan"), 2),
                         CO2_raw = c(7.26, 5.66, 4.67, 3.94, 3.85, 2.94,
                                     7.17, 5.62, 4.67, 3.90, 3.80, 2.87))

# Correcting for the higher consumption of males
# Multiplying by 365 and dividing by 1000 for the final CO2 emissions from food
food_table <- food_table %>% 
    mutate(CO2 = (CO2_raw * consumption/2000)*365/1000)

# Calculating the emissions from commuting by car
car_table <- crossing(c("Small", "Medium", "Large"), 
                      c("Diesel", "Petrol", "Hybrid", "LPG",
                        "Plug-in Hybrid Electric", "Battery Electric"))
colnames(car_table) <- c("Size", "Type")
car_table <- car_table %>% 
    mutate(CO2_raw = c(0.10698, 0.32863, 0.23304, 0.42817, 0.44752, 0.12183,
                       0.08954, 0.26775, 0.17216, 0.28721, 0.30029, 0.11283,
                       0.07462, 0.22082, 0.16538, 0.20000, 0.23877, 0.03597),
           CO2_mile = CO2_raw/1000/1.609344)

# There are 256 working days in the UK
# Look up the CO2 emissions per mile of the selected car type
working_days <- 256

# Calculating the emissions from commuting by motorbike
motor_table <- data.frame(Size = c("Small", "Medium", "Large"),
                          CO2_raw = c(0.13321, 0.1623, 0.21302))
motor_table <- motor_table %>% 
    mutate(CO2_mile = CO2_raw/1000/1.609344)



### PARAMETERS FOR THE HOUSEHOLD CALCULATOR
# NOT YET AVAILABLE



### SHINY APP: USER INTERFACE
ui <- fluidPage(
    titlePanel("Project Jiga: CO2 emissions Calculator"),
    tabsetPanel(
        tabPanel("Individual Calculator",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("gender", label = "What is your gender?", choices = gender),
                         selectInput("food", label = "How would you describe your food consumption?", choices = food),
                         #################### ENERGY
                         sliderInput("gas", label = "What is your individual gas usage (in kWh/year)?", min = 0, max = 15000, value = 0),
                         sliderInput("electricity", label = "What is your individual electricity usage (in kWh/year)?", min = 0, max = 5000, value = 0),
                         sliderInput("water", label = "What is your individual water usage (in liter/day)?", min = 0, max = 300, value = 0),
                         sliderInput("waste", label = "What is your individual waste usage (in kg/year)?", min = 0, max = 1000, value = 0),
                         #################### AIR TRAVEL
                         sliderInput("flight_number", label = "How many round-trip flights do you typically take in a year?", min = 0, max = 15, value = 0),
                         sliderInput("flight_distance", label = "What is the typical one-way distance of your flights (in miles)?", min = 0, max = 5000, value = 0),
                         selectInput("flight_class", label = "In which class do you generally fly?", choices = flight_class),
                         #################### CAR
                         sliderInput("car_dist", label = "What is the distance of your typical daily commute by car (in miles)?", min = 0, max = 250, value = 0),
                         selectInput("car_size", label = "What is the size of your car?", choices = car_size),
                         selectInput("car_fuel", label = "What type of fuel does your car take?", choices = car_fuel),
                         #################### Motorbike
                         sliderInput("motor_dist", label = "What is the distance of your typical daily commute by motorbike (in miles)?", min = 0, max = 250, value = 0),
                         selectInput("motor_size", label = "What is the size of your motorbike?", choices = motor_size),
                         #################### BUS, TRAIN, TAXI
                         sliderInput("bus_dist", label = "What is the distance of your typical daily commute by bus (in miles)?", min = 0, max = 250, value = 0),
                         sliderInput("train_dist", label = "What is the distance of your typical daily commute by train (in miles)?", min = 0, max = 250, value = 0),
                         sliderInput("taxi_dist", label = "What is the distance of your typical daily commute by taxi (in miles)?", min = 0, max = 250, value = 0),
                         #################### FASHION, GADGETS
                         selectInput("fashion", label = "How would you describe your fashion sense?", choices = fashion),
                         selectInput("gadgets", label = "How up-to-date are you with the latest electronic gadgets?", choices = gadgets),
                         width = 6
                     ) ,
                     mainPanel(
                         tableOutput("calculations"),
                         width = 6
                        
                     )    
                 )
        ),       
        tabPanel("Household Calculator",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("income", label = "What is your household income (in 1,000Â£)?", min = 1, max = 150, value = 0),
                         sliderInput("adults", label = "What is the number of adults in your household?", min = 1, max = 5, value = 1),
                         sliderInput("children", label = "What is the number of children in your household?", min = 0, max = 5, value = 0),
                         selectInput("gender_household", label = "What is the gender of the household reference person?", choices = c("Male", "Female")),
                         sliderInput("age_household", label = "What is the age of the household reference person?", min = 18, max = 80, value = 18),
                         selectInput("ethnicity_household", label = "What is the ethnicity of the household reference person?", choices = c("White", "Non-White")),
                         selectInput("education_household", label = "How many years of eduction does the household reference person have?", choices = c("Less than 12", "12 to 15", "16 or more")),
                         selectInput("rural_location", label = "Does the household live in a rural location?", choices = c("Yes", "No")),
                         selectInput("employment", label = "Is any of the household member employed with a living wage?", choices = c("Yes", "No")),
                         width = 6
                     ) ,
                     mainPanel(
                         h4("The estimated total yearly CO2 emissions from your household are:"),
                         h4(textOutput("household_calc")),
                         width = 6
                         
                     )    
                 )
        ),
    )
)

### SHINY APP: SERVER
server <- function(input, output) {
    
    # Calculating the total CO2 emissions from food consumption
    food_score <- reactive(
        food_table$CO2[food_table$gender==input$gender & 
                           food_table$type==input$food]
    )
    
    # Calculating the total CO2 emissions from gas usage
    gas_score <- reactive(
        input$gas*0.22/1000
    )
    
    # Calculating the total CO2 emissions from electricity usage
    electricity_score <- reactive(
        input$electricity*0.6/1000
    )
    
    # Calculating the total CO2 emissions from water usage
    water_score <- reactive(
        input$water*365/1000000*0.344
    )
    
    # Calculating the total CO2 emissions from waste usage
    waste_score <- reactive(
        input$waste*21.317/1000/1000
    )
    
    # Calculating the total CO2 emissions from air travel
    flight_score <- reactive(
        input$flight_number*2*(input$flight_distance/1.609344)*
            ifelse(input$flight_class=="Business class", 3*0.15,
                   ifelse(input$flight_class=="First class", 5*0.15
                          , 0.15))/1000
    )
    
    # Calculating the total CO2 emissions from commuting by car
    car_score <- reactive(
        input$car_dist*working_days*
            car_table$CO2_mile[car_table$Size==input$car_size & 
                                   car_table$Type==input$car_fuel]
    )
    
    # Calculating the total CO2 emissions from commuting by motor
    motor_score <- reactive(
        input$motor_dist*working_days* 
            motor_table$CO2_mile[motor_table$Size==input$motor_size]
    )
    
    # Calculating the total CO2 emissions from commuting by bus
    bus_score <- reactive(
        input$bus_dist*working_days*0.10312/1000/1.609344
    )
    
    # Calculating the total CO2 emissions from commuting by train
    train_score <- reactive(
        input$train_dist*working_days*0.03694/1000/1.609344
    )
    
    # Calculating the total CO2 emissions from commuting by taxi
    taxi_score <- reactive(
        input$taxi_dist*working_days*0.14549/1000/1.609344
    )
    
    # Calculating the total CO2 emissions from fashion
    fashion_score <- reactive(
        0.03*5.4*ifelse(input$fashion=="Low", 0.5, ifelse(input$fashion=="Medium", 1, 1.5))
    )
    
    # Calculating the total CO2 emissions from electronic gadgets
    gadget_score <- reactive(
        0.03*5.4*ifelse(input$gadgets=="Low", 0.5, ifelse(input$gadgets=="Medium", 1, 1.5))
    )
    
    # Calculating the sum of emissions
    total_emissions <- reactive({
        food_score() + gas_score() + electricity_score() + water_score() + waste_score() +
            flight_score() + car_score() + motor_score() + 
            bus_score() + train_score() + taxi_score() + fashion_score() + gadget_score()
    })
    
    # Individual CO2 emissions table for output
    output$calculations <- renderTable({
        emission_table <- data.frame(Category = c("Food", "Gas", "Electricity",
                                                    "Water", "Waste",
                                                    "Air Travel", "Car", 
                                                    "Motor", "Bus", "Train", "Taxi",
                                                    "Fashion", "Gadgets", "Total"),
                                     Emissions = c(food_score(), gas_score(),
                                               electricity_score(), water_score(),
                                               waste_score(), flight_score(),
                                               car_score(), motor_score(),
                                               bus_score(), train_score(),
                                               taxi_score(), fashion_score(),
                                               gadget_score(), total_emissions()))
    })
    
    # Calculating the raw results from the regression analysis
    regression_score <- reactive(0.432*log(input$income) + 0.267*(input$adults>=2) + 0.111*(input$adults>=3) + 
                                     0.0736*(input$adults>=4) + 0.110*(input$adults>=5) +
                                     0.09866*(input$children>=1) + 0.0727*(input$children>=2) + 0.0605*(input$children>=3) + 0.0605*(input$children>=4) + 0.0605*(input$children>=5) +
                                     0.0203*(input$age_household) - 0.0188*(input$age_household)^2/100 - 0.0877*(input$age_household==80) +
                                     0.0256*(input$gender_household=="Female") + 0.0734*(input$education_household=="12 to 15") + 0.0996*(input$education_household=="16 or more") +
                                     -0.00918*(input$employment=="No") - 0.0701*(input$ethnicity_household=="Non-White") + 0.0880*(input$rural_location=="Yes"))
    
    # Household CO2 emissions table for output
    output$household_calc <- renderText({
        round(exp(as.numeric(regression_score())), 2)
    })
    
}

### RUN THE APPLICATION
shinyApp(ui = ui, server = server)
