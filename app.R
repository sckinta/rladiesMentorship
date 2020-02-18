# remote::install_github("sckinta/aws.s3")
library(shinyWidgets)
library(shinyjs)
library(shiny)
library(shinythemes)
library(tidyverse)
library(aws.s3)

`%then%` <- shiny:::`%OR%`
################### aws.s3 setting #####################
s3BucketName <- "rladiesmentor"
Sys.setenv("AWS_ACCESS_KEY_ID" = "[SECRET]",
           "AWS_SECRET_ACCESS_KEY" = "[SECRET]",
           "AWS_DEFAULT_REGION" = "us-east-2")

################ aws.s3 save and load data function ####################
saveData <- function(input) {
        data <- data.frame(matrix(nrow=1,ncol=0))
        for (x in fields) {
                var <- input[[x]]
                if (x == "photo_wig" & length(var)!=0){
                        img_file=var$datapath
                        if (grepl("\\.jpeg|\\.JPEG",img_file)){
                                img_format=".jpeg"
                        }
                        if (grepl("\\.png|\\.PNG",img_file)){
                                img_format=".png"
                        }
                }else if (x == "photo_wig" & length(var)==0){
                        img_file="unknown.jpg"
                }
                else{
                        if (length(var)==0){
                                data[[x]] <- " "
                        }
                        else if (length(var) > 1 ) {
                                # handles lists from checkboxGroup and multiple Select
                                data[[x]] <- list(var)
                        } else {
                                # all other data types
                                data[[x]] <- var
                        }
                }
        }
        data$submit_time <- date()
        # Create a unique file name
        name1=as.integer(Sys.time())
        name2=digest::digest(data)
        fileName <- sprintf(
                "%s_%s.rds", 
                name1, 
                name2
        )
        # rename imagefilename
        if (img_file!="unknown.jpg"){
                img_newName <-sprintf(
                        paste0("%s_%s",img_format), 
                        name1, 
                        name2
                )
                file.rename(from=img_file, to=file.path(tempdir(),img_newName))
                # Upload the file to S3
                put_object(file = file.path(tempdir(),img_newName), object = img_newName, bucket = s3BucketName, check_region = F, acl = "public-read")
        }else{
                img_newName = "unknown.jpg"
        }
        
        # save tmp file here
        
        # Write the file to the local system
        file_path <- file.path(tempdir(), fileName)
        # saveRDS(
        #         object = data,
        #         file = file_path
        # )
        data["photo_wig"]=paste0("https://rladiesmentor.s3.us-east-2.amazonaws.com/",img_newName)
        s3save(data, bucket = s3BucketName, object = fileName)
        
        
}

loadData <- function() {
        # read all the files into a list
        # files <- list.files(outputDir, full.names = TRUE, pattern="\\.rds")
        files <- sapply(get_bucket(s3BucketName), function(x){x[["Key"]]})
        files <- files[grepl("\\.rds",files)]
        if (length(files) == 0) {
                # create empty data frame with correct columns
                field_list <- c(fields, "submit_time")
                data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
                names(data) <- field_list
        } else {
                data <- lapply(files, function(x) {
                        # object <- get_object(x, s3BucketName)
                        # readRDS(object)
                        s3load(x, bucket = s3BucketName)
                        data
                })
                
                # Concatenate all data together into one data.frame
                data <- do.call(rbind, data)
        }
        colnames(data) = c("name","pronoun","linkedin", "signUp.type","expertises","primary.employment","preferred.mentor.method","submit.timestamp","photo.link")
        # data
        out = tibble(
                photo=sapply(data$photo.link,function(pic){paste0('<img src=',pic,' height=52></img>')})
        )
        out = out %>%
                mutate(name=mapply(function(url,text){paste0("<a href='",url,"'>",text,"</a>")}, data$linkedin, data$name))
        out = bind_cols(
                out %>% as.data.frame(),
                data[,c("pronoun","signUp.type","expertises","primary.employment","preferred.mentor.method")]
        )
        out
}

# outputDir="/Users/suc1/ShareFile/Personal Folders/Rladies/mentorShiny/mentorShiny/data"

#################### user variables #########################
types=c("Speaker","Mentor")
expertises=c("Academia to industry transition","Transition to new field/industry","Project/team management","Making data science more accessible","Working with big datasets","Language research","Data cleaning","Capacity building","Global health","Data visualization","Package creation","Geospatial science","Ecological modeling","Mental health","Building scalable tools","Reproducible research","App development")
employment=c("Academic","Pharmaceutical","Financial","Business","Research","Quality assurance","Government/public sector")
meets=c("In-person","Remote (e.g. by phone or online)")
genders=c("She/her", "He/him", "They/them","Other")

#################### define wigget (variable_name/field_name and inputId must be the same) #################
fields <- c("name_wig", "gender_wig", "linkedin_wig", "photo_wig",
            "type_wig", "expertise_wig", "employment_wig", "meet_wig")

name_wig <- textInput("name_wig", "Name:", "")
gender_wig  <- radioButtons(
        "gender_wig", 
        "Pronouns:",
        genders, 
        inline = TRUE,
        selected = "none"
)
linkedin_wig <- textInput("linkedin_wig","LinkedIn Profile Link:","")
photo_wig <- fileInput("photo_wig", "Your photo (eg. .jpeg, .png)", accept = c("jpeg","png"))
type_wig <- checkboxGroupInput(
        "type_wig",
        "Available as mentor and/or speaker?", 
        types
)
expertise_wig <- selectizeInput(
        inputId = "expertise_wig",
        label = "Areas of expertise", 
        choices =  expertises,
        multiple = T,
        options = list(create = TRUE)
)
employment_wig <- selectizeInput(
        inputId = "employment_wig",
        label = "Primary type of employment", 
        choices =  employment,
        multiple = F,
        options = list(create = TRUE)
)
meet_wig <- checkboxGroupInput(
        "meet_wig",
        "If you are willing to serve as a mentor, \nwhat is your preferred method of communication with your mentees?", 
        meets
)

clear_wig <- actionButton("clear", "Clear Form")
submit_wig <- actionButton("submit", "Submit")

##################### resetForm function #######################
resetForm <- function(session) {
        updateTextInput(session, "name_wig", value = "")
        updateRadioButtons(session, "gender_wig", selected = "none")
        updateTextInput(session, "linkedin_wig", value = "")
        updateCheckboxGroupInput(session, "type_wig", selected=character(0))
        updateSelectizeInput(session, "expertise_wig", selected=character(0))
        updateSelectizeInput(session, "employment_wig", selected=character(0))
        updateCheckboxGroupInput(session, "meet_wig", selected=character(0))
}


######################### ui ####################################
ui <- navbarPage(
        title = "Mentor/Speaker", theme = shinytheme("flatly"),id="tab",
        tabPanel(
                title="Sign-Up",
                tags$head(
                        tags$style(
                                HTML(".shiny-notification {position:fixed; top: calc(50%);left: calc(30%);}")
                                )
                        ),
                fluidRow(
                        column(width=12,
                               name_wig,
                               offset=3
                        )
                        
                ),
                fluidRow(
                        column(12,
                               gender_wig,
                               offset=3
                        )
                ),
                fluidRow(
                        column(12,
                               linkedin_wig,
                               offset=3
                        )
                ),
                fluidRow(
                        column(12,
                               photo_wig,
                               offset=3
                        )
                ),
                fluidRow(
                        column(12,
                               type_wig,
                               offset=3
                        )
                ),
                fluidRow(
                        column(12,
                               expertise_wig,
                               offset=3
                        )
                ),
                fluidRow(
                        column(width=12,
                                employment_wig,
                                offset=3
                        )
                ),
                fluidRow(
                        column(
                                12,
                                meet_wig,
                                offset=3
                        )
                ),
                fluidRow(
                        column(
                                3,
                                clear_wig,
                                offset=2
                        ),
                        column(
                                3,
                                submit_wig,
                                offset=1
                        )
                )
        ),
        tabPanel(
                title="Search",
                sidebarLayout(
                        sidebarPanel(
                                width = 3,
                                checkboxGroupInput(
                                        inputId = "search_type",
                                        label = "Mentor/Speaker",
                                        choices = types,
                                        selected = types
                                ),
                                checkboxGroupInput(
                                        inputId = "search_gender",
                                        label = "Pronoun",
                                        choices = genders,
                                        selected = genders
                                ),
                                checkboxGroupInput(
                                        inputId = "search_meet",
                                        label = "Preferred Mentorship Meeting Format",
                                        choices = meets,
                                        selected = meets
                                ),
                                pickerInput(
                                        inputId = "search_employment",
                                        label = "Primary Employment",
                                        choices = employment,
                                        selected = employment,
                                        multiple = T, 
                                        options = list(`actions-box` = TRUE)
                                ),
                                pickerInput(
                                        inputId = "search_expertise",
                                        label = "Area(s) of Expertise",
                                        choices = expertises,
                                        selected = expertises,
                                        multiple = T, 
                                        options = list(`actions-box` = TRUE)
                                ),
                                actionButton("search_submit", "Search")
                        ),
                        mainPanel(
                                width = 9,
                                dataTableOutput("responses")
                        )
                )
        )
        
)

########################### server ####################################
server <- function(input, output, session) {
        ### input tab
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
                # validate linkedInde
                validate(
                        need(input$name_wig!="", 
                             showNotification("Please signup with your name", duration = 0, type = "error", id="name_error")
                        )
                )
                removeNotification("name_error", session = getDefaultReactiveDomain())
                validate(
                        need(grepl("linkedin\\.com",input$linkedin_wig), 
                                showNotification("Please add a valid linkedin url", duration = 0, type = "error", id="linkedin_error")
                        )
                )
                removeNotification("linkedin_error", session = getDefaultReactiveDomain())
                validate(
                        need(input$type_wig!="", 
                             showNotification("Please select mentor or speaker", duration = 0, type = "error", id="type_error")
                        )
                )
                removeNotification("type_error", session = getDefaultReactiveDomain())
                # thank the user
                response <- paste0("Thank you for signing up for Rladies menter/speaker program!")
                showNotification(response, duration = 0, type = "message")
                saveData(input)
                resetForm(session)
        })
        # clear the fields
        observeEvent(input$clear, {
                resetForm(session)
        })
        
        # output$responses <- renderDataTable({
        #         # update with current response when Submit or Delete are clicked
        #         input$submit
        #         loadData()
        # },escape = FALSE)
        # 
        # #### search tab
        v <- reactiveValues(data = loadData())
        
        observeEvent(input$search_submit, {
                search_type_bl = unlist(lapply(loadData()$signUp.type, function(x){any(x %in% input$search_type)}))
                search_gender_bl = unlist(lapply(loadData()$pronoun, function(x){any(x %in% input$search_gender)}))
                search_meet_bl = unlist(lapply(loadData()$preferred.mentor.method, function(x){any(x %in% input$search_meet)}))
                search_employment_bl = unlist(lapply(loadData()$primary.employment, function(x){any(x %in% input$search_employment)}))
                search_expertise_bl = unlist(lapply(loadData()$expertises, function(x){any(x %in% input$search_expertise)}))
                bl = as.logical(search_type_bl*search_gender_bl*search_meet_bl*search_employment_bl*search_expertise_bl)
                v$data = loadData()[bl,]
        })
        
        output$responses <- renderDataTable({
                # update with current response when Submit or Delete are clicked
                input$submit
                v$data
        },escape = FALSE)
        
}


####### run
shinyApp(ui = ui, server = server)
