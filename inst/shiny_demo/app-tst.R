# app-tst.R - For Testing

library(shiny)
library(RSQLite)
library(dtedit2)

##### Load books data.frame as a SQLite database
conn <- dbConnect(RSQLite::SQLite(), "books.sqlite")

message("Dtedit2 TESTE REACTIVE - V.", dtedit2::version())

browser()

# set controlador to default
# dtedit2::start()

if (!'books' %in% dbListTables(conn)) {
	books <- read.csv('books.csv', stringsAsFactors = FALSE)
	books$Authors <- strsplit(books$Authors, ';')
	books$Authors <- lapply(books$Authors, trimws) # Strip white space
	books$Authors <- unlist(lapply(books$Authors, paste0, collapse = ';'))
	books$id <- 1:nrow(books)
	books$Date <- paste0(books$Date, '-01-01')
	dbWriteTable(conn, "books", books, overwrite = TRUE)
}

getBooks <- function() {
	res <- dbSendQuery(conn, "SELECT * FROM books")
	books <- dbFetch(res)
	dbClearResult(res)
	books$Authors <- strsplit(books$Authors, ';')
	books$Date <- as.Date(books$Date)
	books$Publisher <- as.factor(books$Publisher)
	return(books)
}

##### Callback functions.
books.insert.callback <- function(data, row) {
	query <- paste0("INSERT INTO books (id, Authors, Date, Title, Publisher) VALUES (",
					"", max(getBooks()$id) + 1, ", ",
					"'", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
					"'", as.character(data[row,]$Date), "', ",
					"'", data[row,]$Title, "', ",
					"'", as.character(data[row,]$Publisher), "' ",
					")")
	print(query) # For debugging
	dbSendQuery(conn, query)
	return(getBooks())
}

books.update.callback <- function(data, olddata, row) {
	query <- paste0("UPDATE books SET ",
					"Authors = '", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
					"Date = '", as.character(data[row,]$Date), "', ",
					"Title = '", data[row,]$Title, "', ",
					"Publisher = '", as.character(data[row,]$Publisher), "' ",
					"WHERE id = ", data[row,]$id)
	print(query) # For debugging
	dbSendQuery(conn, query)
	return(getBooks())
}

books.delete.callback <- function(data, row) {
	query <- paste0('DELETE FROM books WHERE id = ', data[row,]$id)
	dbSendQuery(conn, query)
	return(getBooks())
}

##### Create the Shiny server
#server <- function(input, output, session) {
server <- function(input, output) {
	books <- reactiveVal()
	books(getBooks())
	
	#token <- session$token
	#message('session$token: ', token )
	token <- 'abc'
	
	browser()

	
	#output$info <- shiny::renderUI({
	#  shiny::br()
	#  paste0('token: ', print(token))
	#  shiny::br()
	#  paste0('time: ', Sys.time(), ' token: ', token )
	#})
	
	
	#shiny::observeEvent(input$btn, {
	  
	  browser()
	  
	  # get books not reactive
	  nr.books <- if(shiny::is.reactive(shiny::isolate(books))) {
	    shiny::isolate(books()) 
	  } else {books}
	  
	  p.input.choices <- list(Authors = unique(unlist(nr.books$Authors)))
	  p.view.cols <- names(nr.books[c(5,1,3)])

	  result_DT <- callModule (module = dtedit2::dtedit2, id = 'dataspace', input = input, output = output,
	                   #session = session,
	                   token = token,
	                   #name = 'dataspace',
	                   thedataf = books,
	                   view.cols = p.view.cols,
	                   edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
	                   edit.label.cols = c('Book Title', 'Authors', 'Publication Date', 'Publisher'),
	                   input.types = c(Title='textAreaInput'),
	                   input.choices = p.input.choices,
	                   callback.update = books.update.callback,
	                   callback.insert = books.insert.callback,
	                   callback.delete = books.delete.callback)
	  
	  
	#})
	
	#names <- data.frame(Name=character(), Email=character(), Date=numeric(),
	#					Type = factor(levels=c('Admin', 'User')),
	#					stringsAsFactors=FALSE)
	#names$Date <- as.Date(names$Date, origin='1970-01-01')
	#namesdt <- 	dtedit2::dtedit2(input = input,
	#                             output = output, 
	#                             session = session,
	#                             token = token,
	#                             name = 'names',
	#                             thedata = names, input.types =  c(Name='textInput'))
	
	shiny::observeEvent(input$btn2, {
	  #message('outs: ', print(shiny::outputOptions(output)))
	  browser()
	  message('observe event click btn2')
	  #shiny::isolate(print(shiny::reactiveValuesToList(input)))
	  dtedit2::updateDados(dados = head(books,3),
	                       ui_name = 'books',
	                       token = token)
	})
	
	shiny::observeEvent(input$btn3, {
	  browser()
	  message('observe event click btn3')
	  #shiny::isolate(print(shiny::reactiveValuesToList(input)))
	  dtedit2::updateDados(dados = head(names,2),
	                       ui_name = 'names',
	                       token = token)
	})
	
		
}

##### Create the shiny UI
ui <- shiny::fluidPage(shiny::uiOutput('info'),
                       shiny::br(''),
                       shiny::actionButton('btn', 'Show Table'),
                       shiny::actionButton('btn2', 'Update Table Books'),
                       shiny::actionButton('btn3', 'Update Table Names'),
                       shiny::actionButton('btn4', 'use Reactive'),
                       shiny::br(''),
  shiny::h3('Books'),
  dtedit2::dteditUI("dataspace"),
  shiny::hr(), shiny::h3('Email Addresses')
)

shiny::shinyApp(ui = ui, server = server)
