library(shiny)
library(caret)
library(factoextra)
library(dplyr)
library(cluster)
library(data.table)

fulldata = fread("full_data.csv")

map = fread("map.csv")
fulldata$Vas_Subscription
fulldata = merge(fulldata , map , by.x = "Vas_Subscription" , by.y = "VAS" , all.x = T)

train = fulldata %>% filter(train == "train")
train$train = NULL
train$V20 = NULL
#train_new$V20 = NULL

unique(train$AGE_SLAB)
unique(train$SECTOR)
train_new = train %>% mutate(SHOPPING = ifelse(WEBAPP_FLAG %in% c("APP","WEB") & AGE_SLAB %in% c("AGE_18_TO_23_SLAB","AGE_24_TO_28_SLAB","AGE_29_TO_33_SLAB") & International_Usage == "No",1,0 ))

train_new = train_new %>% mutate(WORK = ifelse(AGE_SLAB  %in% c("AGE_24_TO_28_SLAB","AGE_29_TO_33_SLAB","AGE_34_TO_38_SLAB","AGE_39_TO_43_SLAB","AGE_44_TO_48_SLAB") & !data_usage %in% c("NO DATA USAGE") | SECTOR == "WORK", 1,0))

train_new = train_new %>% mutate(PERSONAL = ifelse(!AGE_SLAB %in% c("UNKNOWN","AGE_54_TO_58_SLAB","AGE_GT_58" ) | SECTOR == "PERSONAL",1,0))

train_new = train_new %>% mutate(TRAVEL = ifelse(AGE_SLAB %in% c("AGE_29_TO_33_SLAB","AGE_34_TO_38_SLAB","AGE_39_TO_43_SLAB") & !data_usage %in% c("NO DATA USAGE")  | International_Usage == "Yes" | SECTOR == "TRAVEL",1,0) )

train_new = train_new %>% mutate(HEALTH = ifelse(AGE_SLAB %in% c("AGE_44_TO_48_SLAB","AGE_49_TO_53_SLAB","AGE_54_TO_58_SLAB","AGE_GT_58") | SECTOR == "HEALTH",1,0))

train_new = train_new %>% mutate(ENTERTAINMENT = ifelse(AGE_SLAB %in% c("AGE_LT_18_SLAB","AGE_18_TO_23_SLAB","AGE_24_TO_28_SLAB","AGE_29_TO_33_SLAB") | SECTOR == "NEWS/ENTERTAINMENT", 1,0) )

train_new = train_new %>% mutate(FINANCE = ifelse(AGE_SLAB %in% c("AGE_34_TO_38_SLAB","AGE_39_TO_43_SLAB","AGE_44_TO_48_SLAB","AGE_49_TO_53_SLAB","AGE_54_TO_58_SLAB","AGE_GT_58") | SECTOR == "BANKING",1,0))

train_new = train_new %>% mutate(FOOD = ifelse( SECTOR == "FOOD",1,0))

#SHOPPING
train_shopping = train_new %>% select(SHOPPING,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

sh_log_model <- glm(SHOPPING ~ . , data = train_shopping ,family = binomial(link="logit"))


#WORK
train_work = train_new %>% select(WORK,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

wo_log_model <- glm(WORK ~ . , data = train_work ,family = binomial(link="logit"))

#PERSONAL
train_per = train_new %>% select(PERSONAL,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

pe_log_model <- glm(PERSONAL ~ . , data = train_per ,family = binomial(link="logit"))

#TRAVEL
train_travel = train_new %>% select(TRAVEL,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

tr_log_model <- glm(TRAVEL ~ . , data = train_travel ,family = binomial(link="logit"))

#HEALTH
train_health = train_new %>% select(HEALTH,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

he_log_model <- glm(HEALTH ~ . , data = train_health ,family = binomial(link="logit"))

#ENTERTAINMENT
train_ent = train_new %>% select(ENTERTAINMENT,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

en_log_model <- glm(ENTERTAINMENT ~ . , data = train_ent ,family = binomial(link="logit"))

#FINANCE
train_fin = train_new %>% select(FINANCE,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

fi_log_model <- glm(FINANCE ~ . , data = train_fin ,family = binomial(link="logit"))

#FOOD
train_food = train_new %>% select(FOOD,AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)

fo_log_model <- glm(FOOD ~ . , data = train_food ,family = binomial(link="logit"))



# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

options(shiny.maxRequestSize=30*1024^2) 
# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      #return(head(df))
      
      
      

      
      
      test = df
      
      
      #colnames(train_new)
      
 
      
      #SHOPPING
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(sh_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$SHOPPING = log_predict
      unique(test$SHOPPING)
      NROW(test %>% filter(SHOPPING == 0.2))
      NROW(test %>% filter(SHOPPING == 1))
      
      #WORK
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(wo_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$WORK = log_predict
      unique(test$WORK)
      NROW(test %>% filter(WORK == 0.2))
      NROW(test %>% filter(WORK == 1))
      
      #PERSONAL
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(pe_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$PERSONAL = log_predict
      unique(test$PERSONAL)
      NROW(test %>% filter(PERSONAL == 0.2))
      NROW(test %>% filter(PERSONAL == 1))
      
      #TRAVEL
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(tr_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$TRAVEL = log_predict
      unique(test$TRAVEL)
      NROW(test %>% filter(TRAVEL == 0.2))
      NROW(test %>% filter(TRAVEL == 1))
      
      #HEALTH
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(he_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$HEALTH = log_predict
      unique(test$HEALTH)
      NROW(test %>% filter(HEALTH == 0.2))
      NROW(test %>% filter(HEALTH == 1))
      
      #ENTERTAINMENT
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(en_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$ENTERTAINMENT = log_predict
      unique(test$ENTERTAINMENT)
      NROW(test %>% filter(ENTERTAINMENT == 0.2))
      NROW(test %>% filter(HEALTH == 1))
      
      #FINANCE
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(fi_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$FINANCE = log_predict
      unique(test$FINANCE)
      NROW(test %>% filter(FINANCE == 0.2))
      NROW(test %>% filter(FINANCE == 1))
      
      #FOOD
      
      test_new = test %>% select(AGE_SLAB,ARPU_SLAB,Age_On_Network_SLAB,Brand_Identifier,Circle_Name,connection_type,DND_STATUS,data_usage , GENDER , Genre , International_Usage , Pincode , Recharge, sms_usage ,Voice_Usage_SLAB ,WEBAPP_FLAG)
      log_predict <- predict(fo_log_model,newdata = test_new,type = "response")
      log_predict <- ifelse(log_predict <0.8,0,1)
      test$FOOD = log_predict
      unique(test$FOOD)
      NROW(test %>% filter(FOOD == 0.2))
      NROW(test %>% filter(FOOD == 1))
      
      test$cnt = 1
      
      
      
      shop = test %>% group_by(SHOPPING) %>% summarise(Shopping = n() , `% Cust (Shopping)` = paste0(round(n()/sum(test$cnt) * 100,2),"%") ) %>% rename(Probability = SHOPPING)
      work = test %>% group_by(WORK) %>% summarise(Work = n(),  `% Cust (Work)` = paste0(round(n()/sum(test$cnt) * 100,2),"%")) %>% rename(Probability = WORK)
      
      per = test %>% group_by(PERSONAL) %>% summarise(Personal = n(),  `% Cust (Personal)`  = paste0(round(n()/sum(test$cnt) * 100,2),"%"))%>% rename(Probability = PERSONAL)
      trav = test %>% group_by(TRAVEL) %>% summarise(Travel = n(),  `% Cust (Travel)`  = paste0(round(n()/sum(test$cnt) * 100,2),"%"))%>% rename(Probability = TRAVEL)
      
      
      heal = test %>% group_by(HEALTH) %>% summarise(Health = n(),  `% Cust (Health)`  = paste0(round(n()/sum(test$cnt) * 100,2),"%"))%>% rename(Probability = HEALTH)
      enter = test %>% group_by(ENTERTAINMENT) %>% summarise(Entertainment = n(),  `% Cust (Entertainment)`  = paste0(round(n()/sum(test$cnt) * 100,2),"%"))%>% rename(Probability = ENTERTAINMENT)
      
      
      fin = test %>% group_by(FINANCE) %>% summarise(Finance = n(),  `% Cust (Finance)`  = paste0(round(n()/sum(test$cnt) * 100,2),"%"))%>% rename(Probability = FINANCE)
      food = test %>% group_by(FOOD) %>% summarise(Food = n(),  `% Cust (Food)`  = paste0(round(n()/sum(test$cnt) * 100,2),"%"))%>% rename(Probability = FOOD)
      
      
      final = shop
      final = merge(final , work , by = "Probability" , all.x = T)
      final = merge(final , per , by = "Probability" , all.x = T)
      final = merge(final , trav , by = "Probability" , all.x = T)
      final = merge(final , heal , by = "Probability" , all.x = T)
      final = merge(final , enter , by = "Probability" , all.x = T)
      final = merge(final , fin , by = "Probability" , all.x = T)
      final = merge(final , food , by = "Probability" , all.x = T)

      
      return(final)
      

      
      library(radarchart)
      
      skillsByLabel = fread("C:/Users/ADMIN/Documents/VIL Codefest/VIL Confidential Information Dataset/Book3.csv")
      return(chartJSRadar(scores = skillsByLabel, maxScale = 1))
      

      
      
    }
    else {
      return(df)
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
