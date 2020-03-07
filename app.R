# https://sckinta.shinyapps.io/mentorShiny/
library(shinyWidgets)
library(shinyjs)
library(shiny)
library(shinythemes)
library(tidyverse)
library(googledrive)
library(googlesheets4)

# # designate project-specific cache
options(
        gargle_oauth_cache = ".secret",
        gargle_oauth_email = TRUE
)

################ googlesheet save and load data function ####################
saveData <- function(input) {
        # download previous spreadsheet to tempfile
        tmpDir=file.path(tempdir(),"mentors.csv")
        mentors=drive_find(pattern = "mentors", type = "spreadsheet")
        drive_download(as_id(mentors), type="csv", path=tmpDir, overwrite=T)
        # drive_download("mentors", type="csv", path=tmpDir, overwrite=T)
        
        # read spreadsheet to df
        df = read_csv(tmpDir)
        
        # read input to data
        data <- data.frame(matrix(nrow=1,ncol=0))
        for (x in fields) {
                var <- input[[x]]
                if (length(var)==0){
                        data[[x]] <- " "
                }
                else if (length(var) > 1 ) {
                        # handles lists from checkboxGroup and multiple Select
                        data[[x]] <- paste(var,collapse = ", ")
                } else {
                        # all other data types
                        data[[x]] <- var
                }
        }
        
        data$submit_time <- Sys.time()
        colnames(data) = c("name","pronoun","linkedin", "email","signUp.type","expertises","primary.employment","preferred.mentor.method","submit.timestamp")
        # append new data
        df = bind_rows(df, data)
        # write into tempfile
        write_csv(df, path=tmpDir, na=" ")
        # update mentors spreadsheet
        mentors <- mentors %>% drive_update(
                tmpDir,
                name="mentors"
        )
        # drive_rm(mentors)
}

loadData <- function() {
        # read spreadsheet
        sheet_id=drive_find(pattern = "mentors", type = "spreadsheet")$id
        data=read_sheet(sheet_id)
        # data
        names = tibble(
                name=mapply(
                        function(url,text){paste0("<a href=mailto:",url,">",text,"</a>")}, 
                        data$email, data$name
                        )
        )
        links = tibble(
                links=sapply(data$linkedin,function(url){
                        ifelse(
                                url!=" ",
                                paste0("<a href='",url,"'>","linkedIn","</a>"),
                                "linkedIn"
                        )
                })
        )
        out = bind_cols(
                names %>% as.data.frame(),
                data[,c("pronoun","signUp.type","expertises","primary.employment","preferred.mentor.method")],
                links %>% as.data.frame()
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
fields <- c("name_wig", "gender_wig", "linkedin_wig","email_wig",
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
email_wig <- textInput("email_wig","Email:","")

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
        updateTextInput(session, "email_wig", value = "")
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
                               email_wig,
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
                # validate(
                #         need(grepl("linkedin\\.com",input$linkedin_wig), 
                #                 showNotification("Please add a valid linkedin url", duration = 0, type = "error", id="linkedin_error")
                #         )
                # )
                # removeNotification("linkedin_error", session = getDefaultReactiveDomain())
                validate(
                        need(grepl("\\@",input$email_wig), 
                             showNotification("Please add a valid email", duration = 0, type = "error", id="email_error")
                        )
                )
                removeNotification("email_error", session = getDefaultReactiveDomain())
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
                # updateNavbarPage(session, "tab", selected = NULL)
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
        df = loadData()
        v <- reactiveValues(data = df)
        # observeEvent(input$submit, {
        #         updateNavbarPage(session, "tab", selected = NULL)
        # })
        
        
        observeEvent(input$search_submit, {
                search_type_bl = unlist(lapply(df$signUp.type, function(x){any(strsplit(x,", ")[[1]] %in% input$search_type)}))
                search_gender_bl = unlist(lapply(df$pronoun, function(x){any(strsplit(x,", ")[[1]]  %in% input$search_gender)}))
                search_meet_bl = unlist(lapply(df$preferred.mentor.method, function(x){any(strsplit(x,", ")[[1]]  %in% input$search_meet)}))
                search_employment_bl = unlist(lapply(df$primary.employment, function(x){any(strsplit(x,", ")[[1]]  %in% input$search_employment)}))
                search_expertise_bl = unlist(lapply(df$expertises, function(x){any(strsplit(x,", ")[[1]]  %in% input$search_expertise)}))
                bl = as.logical(search_type_bl*search_gender_bl*search_meet_bl*search_employment_bl*search_expertise_bl)
                v$data = df[bl,]
        })
        
        output$responses <- renderDataTable({
                # update with current response when Submit
                input$submit
                v$data
        },escape = FALSE)
        
        # updateNavbarPage(session, "tab", selected = NULL)
        
}


####### run
shinyApp(ui = ui, server = server)
