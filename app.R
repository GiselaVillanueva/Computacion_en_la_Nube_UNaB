# Load all necessary libraries

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

# Connect to DB and fetch tables
# Ensure 'MyBooks.db' is in the same directory as your app.R file
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
  
  # Corrected return statement: used 'Pretty_Show_Books_read_and_Users'
  return(Pretty_Show_Books_read_and_Users)
}

ui <- fluidPage(
  useShinyjs(),
  shinyauthr::loginUI("login"), # Login (as admin or visitor)
  uiOutput("app_ui"),
  title = "Welcome to MyBooks!", # This appears once you're logged in
  setBackgroundColor(color = c("#F7F2E6")),
  tags$head(
    tags$style(HTML(" #styling
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
  credentials <- shinyauthr::loginServer( #Log in view
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(input$logout)
  )
  
  # Initial data load and reactive value definition
  scores_data <- reactiveVal(update_scores_data())
  
  # Shows all books existing on the table
  DimBooks_data <- reactiveVal(dbGetQuery(database, "SELECT * FROM DimBooks;"))
  
  # Stores the book ID to be deleted
  rv <- reactiveValues(book_id_to_delete = NULL)
  
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
      div( #Shows all initial buttons; at line 123, shows 'Edit database' if the user is admin
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
  
  observeEvent(input$click1, { #It picks up if you clicked Button 1, 'See books and locations'
    DimBooks_current <- DimBooks_data()
    
    output$mainUI <- renderUI({ #If you did click that button, it shows the table and all the dropdowns
      sidebarLayout(
        sidebarPanel(
          pickerInput("GenreDropdown", "Genre",
                      choices = unique(DimBooks_current$Genre),
                      selected = unique(DimBooks_current$Genre),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("AuthorDropdown", "Author",
                      choices = unique(DimBooks_current$Author),
                      selected = unique(DimBooks_current$Author),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("TitleDropdown", "Title",
                      choices = unique(DimBooks_current$Title),
                      selected = unique(DimBooks_current$Title),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("RoomDropdown", "Room",
                      choices = unique(DimBooks_current$Room),
                      selected = unique(DimBooks_current$Room),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          width = 2 # Adjusted sidebarPanel width to make more room for the mainPanel
        ),
        mainPanel(
          DTOutput("BooksTable"),
          width = 10 # Adjusted mainPanel width to fill the remaining space
        )
      )
    })
    
    output$BooksTable <- renderDT({ #This is the backend of Books Table: it does the selection we did with the dropdowns and the text box.
      req(input$GenreDropdown, input$AuthorDropdown, input$TitleDropdown, input$RoomDropdown)
      
      filtered_books <- DimBooks_data() %>%
        filter(
          Genre %in% input$GenreDropdown,
          Author %in% input$AuthorDropdown,
          Title %in% input$TitleDropdown,
          Room %in% input$RoomDropdown
        ) %>%
        select(ID, Title, Author, Genre, Room, Furniture, Shelf_or_drawer_number, Comments)
      
      if (credentials()$info$permissions == "admin") { #Shows buttons 'Edit' and 'Delete' if the user is admin
        # Add the action buttons to the table first
        filtered_books <- filtered_books %>%
          mutate(
            Edit = paste0('<button id="edit_', ID, '" class="btn btn-sm btn-info edit_btn" data-id="', ID, '"><i class="fa fa-pencil-alt"></i></button>'),
            Delete = paste0('<button id="delete_', ID, '" class="btn btn-sm btn-danger delete_btn" data-id="', ID, '"><i class="fa fa-trash-alt"></i></button>')
          )
        
        datatable(filtered_books, escape = FALSE, options = list( #Shows those buttons in a single column
          dom = 'Bfrtip',
          scrollX = TRUE,
          columnDefs = list(
            list(targets = grep("Edit|Delete", colnames(filtered_books)), orderable = FALSE, searchable = FALSE)
          )
        ),
        callback = JS( #Button styling with javascript
          "table.on('click', '.edit_btn', function() {",
          "  Shiny.setInputValue('edit_book_id', this.id, {priority: 'event'});",
          "});",
          "table.on('click', '.delete_btn', function() {",
          "  Shiny.setInputValue('delete_book_id', this.id, {priority: 'event'});",
          "});"
        )) %>%
          formatStyle(
            columns = c('Edit', 'Delete'),
            cursor = 'pointer'
          )
      } else {
        datatable(filtered_books, options = list(scrollX = TRUE))
      }
    })
  })
  
  # 'Edit book' functionality
  observeEvent(input$edit_book_id, {
    book_id_to_edit <- gsub("edit_", "", input$edit_book_id)
    book_data <- dbGetQuery(database, "SELECT * FROM DimBooks WHERE ID = ?", params = list(book_id_to_edit))
    
    if (nrow(book_data) > 0) {
      current_books_data <- DimBooks_data() # Get the current full data
      
      showModal(modalDialog(
        title = "Edit Book", #This shows all fields we can edit
        numericInput("edit_id", "ID:", value = book_data$ID, min = 1),
        textInput("edit_title", "Title:", value = book_data$Title),
        textInput("edit_author", "Author:", value = book_data$Author),
        selectizeInput(
          "edit_genre",
          "Genre:",
          # Ensure current genre is included as a choice
          choices = unique(c(current_books_data$Genre, book_data$Genre)),
          selected = book_data$Genre,
          options = list(create = TRUE)
        ),
        selectInput(
          "edit_room",
          "Room:",
          # Ensure current room is included as a choice
          choices = unique(c(current_books_data$Room, book_data$Room)),
          selected = book_data$Room
        ),
        selectInput(
          "edit_furniture",
          "Furniture:",
          # Ensure current furniture is included as a choice
          choices = unique(c(current_books_data$Furniture, book_data$Furniture)),
          selected = book_data$Furniture
        ),
        selectInput(
          "edit_shelf",
          "Shelf/Drawer Number:",
          # Ensure current shelf is included as a choice
          choices = unique(c(current_books_data$Shelf_or_drawer_number, book_data$Shelf_or_drawer_number)),
          selected = book_data$Shelf_or_drawer_number
        ),
        textInput("edit_comments", "Comments:", value = book_data$Comments),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_edit_book", "Save Changes", class = "btn-primary")
        )
      ))
    }
  })
  
  observeEvent(input$confirm_edit_book, { #The backend of editing a record
    req(input$edit_id, input$edit_title, input$edit_author,
        input$edit_genre, input$edit_room, input$edit_furniture,
        input$edit_shelf)
    
    edited_id <- input$edit_id
    edited_title <- trimws(input$edit_title)
    edited_author <- trimws(input$edit_author)
    edited_genre <- trimws(input$edit_genre)
    edited_room <- input$edit_room
    edited_furniture <- input$edit_furniture
    edited_shelf <- input$edit_shelf
    edited_comments <- trimws(input$edit_comments)
    
    if (edited_title == "" || edited_author == "") {
      showNotification("Title and Author cannot be empty.", type = "error")
      return()
    }
    
    tryCatch({
      dbExecute(database,
                "UPDATE DimBooks SET Title = ?, Author = ?, Genre = ?, Room = ?, Furniture = ?, Shelf_or_drawer_number = ?, Comments = ? WHERE ID = ?",
                params = list(edited_title, edited_author, edited_genre, edited_room, edited_furniture, edited_shelf, edited_comments, edited_id))
      
      # Re-fetch data to update the value
      DimBooks_data(dbGetQuery(database, "SELECT * FROM DimBooks;"))
      scores_data(update_scores_data()) # Update scores after book edit
      removeModal() # This closes the modal after successful save
      showNotification(paste("Book '", edited_title, "' updated!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating book:", e$message), type = "error")
    })
  })
  
  # Delete book functionality
  observeEvent(input$delete_book_id, {
    book_id_to_delete <- gsub("delete_", "", input$delete_book_id)
    book_title <- dbGetQuery(database, "SELECT Title FROM DimBooks WHERE ID = ?", params = list(book_id_to_delete))[[1]]
    
    # Store the ID in a reactive value to be accessible in confirm_delete_book
    rv$book_id_to_delete <- book_id_to_delete
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste0("Are you sure you want to delete the book '", book_title, "'? This will also delete all associated reading records."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_book", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_book, { #Frontend of deleting a record
    req(rv$book_id_to_delete)
    book_id <- rv$book_id_to_delete
    
    tryCatch({
      # Delete related entries in FactBooks_read_by_Users first
      dbExecute(database, "DELETE FROM FactBooks_read_by_Users WHERE Book_ID = ?", params = list(book_id))
      
      # Then delete the book from DimBooks
      dbExecute(database, "DELETE FROM DimBooks WHERE ID = ?", params = list(book_id))
      
      # Re-fetch data to update the reactive value
      DimBooks_data(dbGetQuery(database, "SELECT * FROM DimBooks;"))
      scores_data(update_scores_data()) # Update scores after book deletion
      removeModal()
      showNotification("Book and associated reading records deleted!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error deleting book:", e$message), type = "error")
    })
  })
  
  observeEvent(input$click2, { #This observes if I clicked button 3 (see readers' scores)
    output$mainUI <- renderUI({
      # Removed sidebarLayout for this view to hide the left white rectangle
      div(id = "score-wrapper", # Wrap the table in a div for styling
          tableOutput("scoreTable")
      )
    })
    output$scoreTable <- renderTable({
      scores_data()
    })
  })
  
  observeEvent(input$click3, { #This observes if I clicked button 3 (FAQ)
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
          br(),
          h3("For Admin only:"),
          br(),
          h4("5. How do I add a new book?"),
          p("Once you're logged in as an Admin, go to 'Edit database'. You will see the button 'Add book'. Complete the fields and click 'Add'."),
          br(),
          h4("6. How do I add a new reader?"),
          p("Once you're logged in as an Admin, go to 'Edit database'. You will see the button 'Add reader'. Complete the fields and click 'Add'."),
          br(),
          h4("7. How do I add a new reading?"),
          p("First of all, the reader and the book have to exist already in database. Once you're logged in as an Admin and both book and reader exist, go to 'Edit database'. You will see the button 'Add reading'. Complete the fields and click 'Add'."),
          br(),
          h4("8. How do I edit a book?"),
          p("Go to 'See books and locations'; at the right of each record, you will see the buttons 'Edit' (a pencil icon) and 'Delete' (a trash can icon). Hit 'Edit' and you will be able to change any data regarding the book."),
          br(),
          h4("9. What happens if I delete a book?"),
          p("Since the book database and the readings database are linked, if you delete a book that a user has read, that reading will be deleted as well. Keep this in mind if you want to delete a record. To avoid deleting the reading, you can add a comment by using the 'Edit' button and indicating whatever happened in the 'Comments' section (for example: 'Lost by Mario on April 6th').")
        )
      )
    )
  })
  
  observeEvent(input$edit_db, { #It observes if I click 'Edit database' button
    output$mainUI <- renderUI({
      if (credentials()$info$permissions == "admin") { #It makes this button available for admin only
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
  
  observeEvent(input$add_reader, { #Add reader
    showModal(modalDialog(
      title = "Add New Reader",
      textInput("new_reader_name", "Reader Name:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_reader", "Add", class = "btn-primary")
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
    DimUsers_current <- dbGetQuery(database, "SELECT * FROM DimUser;")
    existing_names <- DimUsers_current$User_name
    if (new_name %in% existing_names) {
      showNotification("This reader already exists.", type = "error")
      return()
    }
    tryCatch({
      max_user_id <- dbGetQuery(database, "SELECT MAX(ID) FROM DimUser;")[[1]]
      new_user_id <- ifelse(is.na(max_user_id), 1, max_user_id + 1)
      
      dbExecute(database, "INSERT INTO DimUser (ID, User_name) VALUES (?, ?)", params = list(new_user_id, new_name))
      scores_data(update_scores_data()) # Update scores after adding a reader
      removeModal()
      showNotification(paste("Reader", new_name, "added!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error adding reader:", e$message), type = "error")
    })
  })
  
  observeEvent(input$add_book, { #Add book
    DimBooks_current <- DimBooks_data()
    max_id <- max(DimBooks_current$ID, 0)
    
    showModal(modalDialog(
      title = "Add New Book",
      numericInput("new_book_id", "ID:", value = max_id + 1, min = 1),
      textInput("new_book_title", "Title:", ""),
      textInput("new_book_author", "Author:", ""),
      selectizeInput(
        "new_book_genre",
        "Genre:",
        choices = c("", unique(DimBooks_current$Genre)),
        options = list(create = TRUE),
        selected = ""
      ),
      selectInput("new_book_room", "Room:", choices = unique(DimBooks_current$Room)),
      selectInput("new_book_furniture", "Furniture:", choices = unique(DimBooks_current$Furniture)),
      selectInput("new_book_shelf", "Shelf/Drawer Number:", choices = unique(DimBooks_current$Shelf_or_drawer_number)),
      textInput("new_book_comments", "Comments:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_book", "Add Book", class = "btn-primary")
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
    new_comments <- trimws(input$new_book_comments)
    
    if (new_title == "" || new_author == "") {
      showNotification("Title and Author cannot be empty.", type = "error")
      return()
    }
    
    DimBooks_current <- DimBooks_data()
    existing_ids <- DimBooks_current$ID
    existing_titles <- DimBooks_current$Title
    
    if (new_id %in% existing_ids) {
      showNotification("This Book ID already exists.", type = "error")
      return()
    }
    
    # Check if the title already exists (case-insensitive)
    if (tolower(new_title) %in% tolower(existing_titles)) {
      showNotification("A book with this title already exists. Please change the title.", type = "error")
      return()
    }
    
    tryCatch({
      dbExecute(database,
                "INSERT INTO DimBooks (ID, Title, Author, Genre, Room, Furniture, Shelf_or_drawer_number, Comments)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(new_id, new_title, new_author, new_genre, new_room, new_furniture, new_shelf, new_comments))
      
      DimBooks_data(dbGetQuery(database, "SELECT * FROM DimBooks;"))
      scores_data(update_scores_data()) # Update scores after adding a book
      removeModal()
      showNotification(paste("Book '", new_title, "' added!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error adding book:", e$message), type = "error")
    })
  })
  
  observeEvent(input$add_reading, { #Add reading
    user_choices <- dbGetQuery(database, "SELECT User_name FROM DimUser;")[, 1]
    book_choices <- dbGetQuery(database, "SELECT Title FROM DimBooks;")[, 1]
    
    showModal(modalDialog(
      title = "Add Reading Record",
      selectInput("new_reading_user", "User:", choices = user_choices),
      selectizeInput(
        "new_reading_book",
        "Book:",
        choices = book_choices,
        options = list(create = TRUE),
        selected = character(0)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_reading", "Add Reading", class = "btn-primary")
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
      scores_data(update_scores_data()) # Update scores after adding a reading record
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