library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(DBI)
library(RSQLite)
library(shinyauthr)
library(shinyjs)

# User base with passwords
user_base <- data.frame(
  user = c("admin", "visitor"),
  password = c("adm1n", "v1s1tor"),
  permissions = c("admin", "visitor"),
  stringsAsFactors = FALSE
)

# Connect to your DB and fetch tables
dir <- "MyBooks.db"
database <- DBI::dbConnect(RSQLite::SQLite(), dbname = dir)

# Function to fetch data and recalculate the scores table
update_scores_data <- function() {
  DimBooks <- dbGetQuery(database, "SELECT * FROM DimBooks;")
  DimUsers <- dbGetQuery(database, "SELECT * FROM DimUser;")
  FactBooks_read_by_Users <- dbGetQuery(database, "SELECT * FROM FactBooks_read_by_Users;")
  
  Total_books <- DimBooks %>% summarise(Total = n())
  
  Books_read_and_Users <- FactBooks_read_by_Users %>%
    left_join(DimUsers, by = c("User_ID" = "ID"))
  
  Genres_Books_read_and_Users <- Books_read_and_Users %>%
    left_join(DimBooks, by = c("Book_ID" = "ID"))
  
  Show_Books_read_and_Users <- Genres_Books_read_and_Users %>%
    group_by(User_name) %>%
    summarise(
      Total = n(),
      Genres_read = n_distinct(Genre)
    ) %>%
    mutate(
      Completed = paste0(round((Total / Total_books$Total) * 100, 2), "%")
    ) %>%
    arrange(desc(Total))
  
  Pretty_Show_Books_read_and_Users <- Show_Books_read_and_Users
  colnames(Pretty_Show_Books_read_and_Users) <- c("Member", "Total books read", "Genres read", "% read over total books")
  return(Pretty_Show_Books_read_and_Users)
}

ui <- fluidPage(
  useShinyjs(),
  shinyauthr::loginUI("login"),
  uiOutput("app_ui"),
  title = "Welcome to MyBooks!",
  setBackgroundColor(color = c("#F7F2E6")),
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #F7F2E6;
        border: none;
        box-shadow: none;
      }
      #score-wrapper table {
        margin-left: auto;
        margin-right: auto;
      }
      #score-wrapper th, #score-wrapper td {
        text-align: center !important;
      }
    "))
  )
)

server <- function(input, output, session) {
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(input$logout)
  )
  
  # Initial data load and reactive value definition (moved inside server)
  scores_data <- reactiveVal(update_scores_data())
  
  output$app_ui <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        column(12,
               div(style = "text-align: right; padding: 10px;",
                   actionButton("logout", "Logout", icon = icon("sign-out-alt"), class = "btn-danger btn-sm")
               )
        )
      ),
      titlePanel(
        div(class = "header", style = "text-align: center;",
            img(src = "MyBooks.png", height = 300, width = 300),
            div(class = "title", "Welcome!")
        )
      ),
      div(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",
        br(),
        actionBttn(inputId = "click1", "See books and locations", size = "sm", style = "jelly", color = "success"),
        br(),
        actionBttn(inputId = "click2", "See readers' scores", size = "sm", style = "jelly", color = "success"),
        br(),
        actionBttn(inputId = "click3", "FAQ", size = "sm", style = "jelly", color = "success"),
        br(),
        if (credentials()$info$permissions == "admin") {
          tagList(
            actionBttn(inputId = "edit_db", "Edit database", size = "sm", style = "jelly", color = "danger"),
            br()
          )
        },
        br()
      ),
      uiOutput("mainUI")
    )
  })
  
  observeEvent(input$click1, {
    # Fetch DimBooks inside the observer
    DimBooks_local <- dbGetQuery(database, "SELECT * FROM DimBooks;")
    
    output$mainUI <- renderUI({
      sidebarLayout(
        sidebarPanel(
          pickerInput("GenreDropdown", "Genre",
                      choices = unique(DimBooks_local$Genre),
                      selected = unique(DimBooks_local$Genre),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("AuthorDropdown", "Author",
                      choices = unique(DimBooks_local$Author),
                      selected = unique(DimBooks_local$Author),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("TitleDropdown", "Title",
                      choices = unique(DimBooks_local$Title),
                      selected = unique(DimBooks_local$Title),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("RoomDropdown", "Room",
                      choices = unique(DimBooks_local$Room),
                      selected = unique(DimBooks_local$Room),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          width = 3
        ),
        mainPanel(
          DTOutput("BooksTable")
        )
      )
    })
    
    output$BooksTable <- renderDT({
      req(input$GenreDropdown, input$AuthorDropdown, input$TitleDropdown, input$RoomDropdown)
      DimBooks_local %>%
        filter(
          Genre %in% input$GenreDropdown,
          Author %in% input$AuthorDropdown,
          Title %in% input$TitleDropdown,
          Room %in% input$RoomDropdown
        ) %>%
        select(ID, Title, Author, Genre, Room, Furniture, Shelf_or_drawer_number)
    })
  })
  
  observeEvent(input$click2, {
    output$mainUI <- renderUI({
      sidebarLayout(
        sidebarPanel(width = 2, p(" ")),
        mainPanel(
          div(id = "score-wrapper", tableOutput("scoreTable"))
        )
      )
    })
    output$scoreTable <- renderTable({
      scores_data()
    })
  })
  
  observeEvent(input$click3, {
    output$mainUI <- renderUI(
      div(
        style = "display: flex; justify-content: center; align-items: center; padding:30px;",
        div(
          style = "background-color: #FFFFFF; padding: 30px; border-radius: 10px; width: 725px; text-align: left; box-shadow: 0px 0px 10px #ccc;",
          h2("Frequently Asked Questions"),
          br(),
          h4("1. How can I search for a book?"),
          p("Use the filters by Genre, Author, Title, and Room to find books quickly."),
          br(),
          h4("2. How do I see users' reading scores?"),
          p("Click on 'See readers' scores' to check how many books each member has read."),
          br(),
          h4("3. What is 'Completed %' in the scores section?"),
          p("It shows the percentage of books each user has read compared to the total available."),
          br(),
          h4("4. Who should I contact for help?"),
          p("Please reach out to the administrator if you have any issues."),
          h2("For Admin only:"),
          h4("5. How do I add a new book?"),
          p("Once you're logged in as an Admin, go to 'Edit database'. You will see the button 'Add book'. Complete the fields and click 'Add'."),
          h4("6. How do I add a new reader?"),
          p("Once you're logged in as an Admin, go to 'Edit database'. You will see the button 'Add reader'. Complete the fields and click 'Add'."),
          h4("7. How do I add a new reading?"),
          p("First of all, the reader and the book have to exist already in database. Once you're logged in as an Admin and both book and reader exist, go to 'Edit database'. You will see the button 'Add reading'. Complete the fields and click 'Add'.")
        )
      )
    )
  })
  
  observeEvent(input$edit_db, {
    output$mainUI <- renderUI({
      if (credentials()$info$permissions == "admin") {
        tagList(
          div(
            style = "display: flex; flex-direction: column; align-items: center; gap: 15px;",
            actionBttn(inputId = "add_reader", label = "Add reader", size = "sm", style = "jelly", color = "danger"),
            actionBttn(inputId = "add_book", label = "Add book", size = "sm", style = "jelly", color = "danger"),
            actionBttn(inputId = "add_reading", label = "Add reading", size = "sm", style = "jelly", color = "danger")
          )
        )
      } else {
        NULL
      }
    })
  })
  
  observeEvent(input$add_reader, {
    showModal(modalDialog(
      title = "Add New Reader",
      textInput("new_reader_name", "Reader Name:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_reader", "Add")
      )
    ))
  })
  
  observeEvent(input$confirm_add_reader, {
    req(input$new_reader_name)
    new_name <- trimws(input$new_reader_name)
    if (new_name == "") {
      showNotification("Reader name cannot be empty.", type = "error")
      return()
    }
    # Fetch DimUsers inside the observer
    DimUsers_current <- dbGetQuery(database, "SELECT * FROM DimUser;")
    existing_names <- DimUsers_current$User_name
    if (new_name %in% existing_names) {
      showNotification("This reader already exists.", type = "error")
      return()
    }
    tryCatch({
      # Fetch the maximum existing ID
      max_user_id <- dbGetQuery(database, "SELECT MAX(ID) FROM DimUser;")[[1]]
      new_user_id <- ifelse(is.na(max_user_id), 1, max_user_id + 1)
      
      dbExecute(database, "INSERT INTO DimUser (ID, User_name) VALUES (?, ?)", params = list(new_user_id, new_name))
      # Re-fetch DimUsers after insertion
      DimUsers <<- dbGetQuery(database, "SELECT * FROM DimUser;")
      # Recalculate scores data
      scores_data(update_scores_data())
      removeModal()
      showNotification(paste("Reader", new_name, "added!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error adding reader:", e$message), type = "error")
    })
  })
  
  observeEvent(input$add_book, {
    # Fetch DimBooks inside the observer
    DimBooks_local <- dbGetQuery(database, "SELECT * FROM DimBooks;")
    max_id <- max(DimBooks_local$ID)
    
    showModal(modalDialog(
      title = "Add New Book",
      numericInput("new_book_id", "ID:", value = max_id + 1, min = max_id + 1),
      textInput("new_book_title", "Title:", ""),
      textInput("new_book_author", "Author:", ""),
      selectizeInput(
        "new_book_genre",
        "Genre:",
        choices = c("", unique(DimBooks_local$Genre)),
        options = list(create = TRUE),
        selected = ""
      ),
      selectInput("new_book_room", "Room:", choices = unique(DimBooks_local$Room)),
      selectInput("new_book_furniture", "Furniture:", choices = unique(DimBooks_local$Furniture)),
      selectInput("new_book_shelf", "Shelf/Drawer Number:", choices = unique(DimBooks_local$Shelf_or_drawer_number)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_book", "Add Book")
      )
    ))
  })
  
  observeEvent(input$confirm_add_book, {
    req(input$new_book_id, input$new_book_title, input$new_book_author,
        input$new_book_genre, input$new_book_room, input$new_book_furniture,
        input$new_book_shelf)
    
    new_id <- input$new_book_id
    new_title <- trimws(input$new_book_title)
    new_author <- trimws(input$new_book_author)
    new_genre <- trimws(input$new_book_genre)
    new_room <- input$new_book_room
    new_furniture <- input$new_book_furniture
    new_shelf <- input$new_book_shelf
    
    if (new_title == "" || new_author == "") {
      showNotification("Title and Author cannot be empty.", type = "error")
      return()
    }
    
    # Fetch DimBooks inside the observer
    DimBooks_local <- dbGetQuery(database, "SELECT * FROM DimBooks;")
    existing_ids <- DimBooks_local$ID
    if (new_id %in% existing_ids) {
      showNotification("This Book ID already exists.", type = "error")
      return()
    }
    
    tryCatch({
      dbExecute(database,
                "INSERT INTO DimBooks (ID, Title, Author, Genre, Room, Furniture, Shelf_or_drawer_number)
                VALUES (?, ?, ?, ?, ?, ?, ?)",
                params = list(new_id, new_title, new_author, new_genre, new_room, new_furniture, new_shelf))
      # Re-fetch DimBooks after insertion to update the global DimBooks
      DimBooks <<- dbGetQuery(database, "SELECT * FROM DimBooks;")
      # Recalculate scores data
      scores_data(update_scores_data())
      removeModal()
      showNotification(paste("Book '", new_title, "' added!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error adding book:", e$message), type = "error")
    })
  })
  
  observeEvent(input$add_reading, {
    user_choices <- dbGetQuery(database, "SELECT User_name FROM DimUser;")[, 1]
    book_choices <- dbGetQuery(database, "SELECT Title FROM DimBooks;")[, 1]
    
    showModal(modalDialog(
      title = "Add Reading Record",
      selectInput("new_reading_user", "User:", choices = user_choices),
      selectizeInput(
        "new_reading_book",
        "Book:",
        choices = book_choices,
        options = list(create = TRUE), # Enable creating new entries
        selected = character(0) # Start with no selection
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_reading", "Add Reading")
      )
    ))
  })
  
  observeEvent(input$confirm_add_reading, {
    req(input$new_reading_user, input$new_reading_book)
    
    selected_user_name <- input$new_reading_user
    selected_book_title <- input$new_reading_book
    
    user_id_result <- dbGetQuery(database, "SELECT ID FROM DimUser WHERE User_name = ?;", params = selected_user_name)
    book_id_result <- dbGetQuery(database, "SELECT ID FROM DimBooks WHERE Title = ?;", params = selected_book_title)
    
    if (nrow(user_id_result) != 1) {
      showNotification(paste("Error: Could not find User ID for '", selected_user_name, "'."), type = "error")
      return()
    }
    if (nrow(book_id_result) != 1) {
      showNotification(paste("Error: Could not find Book ID for '", selected_book_title, "'."), type = "error")
      return()
    }
    
    user_id <- user_id_result[[1, 1]]
    book_id <- book_id_result[[1, 1]]
    
    # Check if the reading record already exists
    existing_reading <- dbGetQuery(database,
                                   "SELECT * FROM FactBooks_read_by_Users WHERE User_ID = ? AND Book_ID = ?;",
                                   params = list(user_id, book_id))
    
    if (nrow(existing_reading) > 0) {
      showNotification("This reading record already exists.", type = "warning")
      return()
    }
    
    tryCatch({
      dbExecute(database,
                "INSERT INTO FactBooks_read_by_Users (User_ID, Book_ID) VALUES (?, ?)",
                params = list(user_id, book_id))
      # FactBooks_read_by_Users <<- dbGetQuery(database, "SELECT * FROM FactBooks_read_by_Users;")
      # Recalculate scores data after adding a reading record
      scores_data(update_scores_data())
      removeModal()
      showNotification(paste("Reading record added for User '", selected_user_name, "' and Book '", selected_book_title, "'!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error adding reading record:", e$message), type = "error")
    })
  })
  
  observeEvent(input$logout, {
    session$reload()
  })
}

shinyApp(ui, server)