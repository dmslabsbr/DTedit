#TODO make it works like a normal DTedit,too, but no reactive.

#' Show dtedit version
#'
#' @return version
#'
#' @export
version <- function() {
  res <- '0.0.29r'
  return(res)
  # 0.0.26 - Version with field size check. (addJsInput)
  # 0.0.27 - Correct data input / Include ESCAPE function in DT.
  # 0.0.28 - Add Easy Close parameter
  # 0.0.29 - Bug fix - Required parameters and data insert/update
}


insert.click <- NA
update.click <- NA



#' Create a DataTable with external update using reactive data.
#'
#' dteditUI - user-interface function
#'
#' Use in conjunction with \code{callModule} and \code{dtedit} to create
#' editable datatables. \code{dteditUI} is used in the 'user interface' component
#' of the shiny app.
#'
#' @param id the namespace of the module
#' @family Datatable Edit functions
#' @example inst/shiny_demo/app-tst2.R
#' @export
dteditUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("editdt"))
  )
}

# Add maxlength to inputbox
addJsInput <- function(session, name, typeName, edit.cols, edit.cols.size) {
  if (length(edit.cols.size) == 0) {return ()}
  cmd <- ''
  ns <- session$ns
  for(i in seq_along(edit.cols.size)) {
    col_size <- edit.cols.size[[edit.cols[i]]]
    if (!is.null(col_size)) {
      ui_name <- ns(paste0(name, typeName, edit.cols[i]))
      jsAdd <- paste0("$('#", ui_name, "').attr('maxlength',",col_size,"); ")
      cmd <- paste0(cmd, jsAdd)
    }
  }
  shinyjs::runjs(cmd)
}

# check dtedit2 parameters
dataCheck <- function(thedata, edit.cols, edit.label.cols, view.cols, view.label.cols, edit.require.cols) {
  if (!is.data.frame(thedata) | ncol(thedata) < 1) {
    stop('Must provide a data frame with at least one column.')
  } else if (length(edit.cols) != length(edit.label.cols)) {
    stop('edit.cols and edit.label.cols must be the same length.')
  } else if (length(view.cols) != length(view.label.cols)) {
    stop('view.cols and view.label.cols must be the same length.')
  } else if (!all(view.cols %in% names(thedata))) {
    stop('Not all view.cols are in the data.')
  } else if (!all(edit.cols %in% names(thedata))) {
    stop('Not all edit.cols are in the data. \n edit.cols: ', edit.cols,
         '\n names-thedata: ', names(thedata))
  }
  if (!all(edit.require.cols %in% names(thedata))) {
    stop('a edit.require.cols not in the data')
  }
}


# remove colunas de uma dataframe
# remove_vt = FALSE, mantem as que estão listadas em vt_colunas e
# remove os que não estão.
# remove_vt = TRUE, remove as que estão listadas em vt_Colunas e 
# mantem o restante.
f_remove_columns <- function (df_dados, vt_colunas, remove_vt = FALSE) {
  if (class(df_dados) != "data.frame") { warning("df_dados must be a data.frame")} 
  for (i in names(df_dados)) {
    achou <- i %in% vt_colunas
    if (remove_vt) {
      if (achou) { df_dados[i] <- NULL}
    } else {
      if (!achou) {df_dados[i] <- NULL}
    }
  }
  return (df_dados)
}

# Convert any list columns to characters before displaying
list2char <- function(thedata) {
  for (i in 1:ncol(thedata)) {
    if (nrow(thedata) == 0) {
      thedata[,i] <- character()
    } else if (is.list(thedata[,i])) {
      thedata[,i] <- sapply(thedata[,i], FUN = function(x) { paste0(x, collapse = ', ') })
    }
  }
  return(thedata)
}

# Get date coluns number
getDateCols <- function(thedata, type = 'name') {
  datatypes <- c('Date', "POSIXct", "POSIXlt" )
  tmp <- c()
  if (is.null(ncol(thedata))) {
    stop ('thedata must be a multidimensional data.')
  }
  for (i in 1:ncol(thedata)) {
    day <- thedata[1,i]
    #message('day: ', day, ' - Class:', class(day))
    if (class(day) %in% datatypes) {
      if (type == 'name') {
        column <- names(thedata[i])  
      } else {
        column <- i
      }
      
      tmp <- c(tmp, column)
    }
  }
  return(tmp)
}



valid.input.types <- c('dateInput', 'selectInput', 'numericInput',
                       'textInput', 'textAreaInput', 'passwordInput', 'selectInputMultiple')


# correct inputTypes
inputTypes.go <- function(thedata, edit.cols, input.types ) {
  
  inputTypes <- sapply(thedata[,edit.cols], FUN=function(x) {
    switch(class(x)[1],
           list = 'selectInputMultiple',
           character = 'textInput',
           Date = 'dateInput',
           factor = 'selectInput',
           integer = 'numericInput',
           numeric = 'numericInput',
           POSIXct = 'dateInput',
           POSIXt = 'dateInput')
  })
  
  if(!missing(input.types)) {
    if(!all(names(input.types) %in% edit.cols)) {
      stop('input.types column not a valid editting column: ',
           paste0(names(input.types)[!names(input.types) %in% edit.cols]))
    }
    if(!all(input.types %in% valid.input.types)) {
      stop(paste0('input.types must only contain values of: ',
                  paste0(valid.input.types, collapse = ', ')))
    }
    inputTypes[names(input.types)] <- input.types
  }
  return (inputTypes)
  
}

selectInputMultiple <- function(selectize, ...) {
  shiny::selectInput(multiple = TRUE, selectize = selectize, ...)
}


# get fields data
getFields <- function(session, typeName, values,
                      edit.cols, edit.cols.size, edit.require.cols,
                      edit.label.cols, inputTypes, name, 
                      date.format, date.format.db, date.width, input.choices,
                      select.width, result, numeric.width,
                      textarea.width, textarea.height, text.width,
                      selectize) {
  ns <- session$ns
  fields <- list()
  for(i in seq_along(edit.cols)) {
    if (all(edit.cols[i] %in% edit.require.cols)) {
      # TODO - show * in red.
      edit.label.cols[i] <- paste0(edit.label.cols[i],'(*)')
      #edit.label.cols[i] <- paste0(edit.label.cols[i], tags$strong(style='color:FF0000','(*)'))
      message('n*: ', edit.label.cols[i])
    }
    if(inputTypes[i] == 'dateInput') {
      dt.sys <- as.Date(Sys.Date(),  date.format.db)
      value <- ifelse(missing(values),
                      as.character(dt.sys),
                      as.character(values[,edit.cols[i]]))
      value <- as.Date(value, date.format.db)
      fields[[i]] <- shiny::dateInput(ns(paste0(name, typeName, edit.cols[i])),
                                      label=edit.label.cols[i],
                                      value=value,
                                      format=date.format,
                                      width=date.width)
    } else if(inputTypes[i] == 'selectInputMultiple') {
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      if(is.list(value)) {
        value <- value[[1]]
      }
      choices <- ''
      if(!missing(values)) {
        choices <- unique(unlist(values[,edit.cols[i]]))
      }
      if(!is.null(input.choices)) {
        if(edit.cols[i] %in% names(input.choices)) {
          choices <- input.choices[[edit.cols[i]]]
        }
      }
      if(length(choices) == 1 & choices == '') {
        warning(paste0('No choices available for ', edit.cols[i],
                       '. Specify them using the input.choices parameter'))
      }
      fields[[i]] <- shiny::selectInputMultiple(selectize, 
                                                ns(paste0(name, typeName, edit.cols[i])),
                                                label=edit.label.cols[i],
                                                choices=choices,
                                                selected=value,
                                                width=select.width)
      
    } else if(inputTypes[i] == 'selectInput') {
      value <- ifelse(missing(values), '', as.character(values[,edit.cols[i]]))
      shiny::isolate({
        fields[[i]] <- shiny::selectInput(ns(paste0(name, typeName, edit.cols[i])),
                                          label=edit.label.cols[i],
                                          choices=levels(result$thedata[,edit.cols[i]]),
                                          selected=value,
                                          width=select.width)
      })
    } else if(inputTypes[i] == 'numericInput') {
      value <- ifelse(missing(values), 0, values[,edit.cols[i]])
      fields[[i]] <- shiny::numericInput(ns(paste0(name, typeName, edit.cols[i])),
                                         label=edit.label.cols[i],
                                         value=value,
                                         width=numeric.width)
    } else if(inputTypes[i] == 'textAreaInput') {
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      fields[[i]] <- shiny::textAreaInput(ns(paste0(name, typeName, edit.cols[i])),
                                          label=edit.label.cols[i],
                                          value=value,
                                          width=textarea.width, height=textarea.height)
    } else if(inputTypes[i] == 'textInput') {
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      fields[[i]] <- shiny::textInput(ns(paste0(name, typeName, edit.cols[i])),
                                      label=edit.label.cols[i],
                                      value=value,
                                      width=text.width)
    } else if(inputTypes[i] == 'passwordInput') {
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      fields[[i]] <- shiny::passwordInput(ns(paste0(name, typeName, edit.cols[i])),
                                          label=edit.label.cols[i],
                                          value=value,
                                          width=text.width)
    } else {
      stop('Invalid input type!')
    }
  }
  return(fields)
}

# check required fields
checkReq <- function(input, tag, 
                     name, edit.require.cols, edit.label.cols, edit.cols) {
  lReq <- list() # TODO future use for color require fields
  lack <- c()
  browser()
  for(i in edit.cols) {
    input_add <- shiny::isolate(input[[paste0(name, tag, i)]])
    lReq[i] <- TRUE
    dt_ok <- TRUE
    if (all(i %in% edit.require.cols)) {
      if (class(input_add) == "Date" ) {
        dt_ok <- FALSE
        if (any(nchar(as.character(input_add)) >= 8)) {
          dt_ok = TRUE
        }
      }
      if (is.null(input_add) || identical(input_add, '') || !dt_ok ) {
        lReq[i] <- FALSE
        elem <- edit.label.cols[grep(paste0(i), edit.cols)]
        lack <- c(lack,paste0(elem))
      }
    }
  }
  return(lack)
}	





#' Function to create a DataTable with Add, Edit, and Delete buttons.
#'
#' This object will maintain data state. However, in order of the data to persist
#' between Shiny instances, data needs to be saved to some external format (e.g.
#' database or R data file). The callback functions provide a mechanism for this
#' function to interact with a permanent data storage scheme. The callback
#' functions are called when the user adds, updates, or deletes a row from the
#' data table. The callback must accept two parameters: \code{data} and \code{row}.
#' For inserting and updating, the \code{data} object is the current state of
#' data table including any additions or updates. The \code{row} parameter indicates
#' which row from \code{data} was modified (or added). For deletions, however,
#' the \code{data} represents the data table just before deleting the specified
#' row. That is, if \code{callback.delete} returns a \code{data.frame}, that will
#' be the new data table; otherwise this function will remove row \code{row} from
#' \code{data} and that will become the current data table.
#'
#' The callback functions may throw errors (see e.g. \code{stop}) if there are
#' problems with data. That is, if data validation checks indicate data problems
#' before inserting or updating a row the function may throw an error. Note that
#' the error message will be presented to the user so providing messages
#' meaningful to the user is recommended. Moreover, if an error is thrown, the
#' modal dialog is not dismissed and the user can further edit the data and
#' retry the insertion or update.
#'
#' Callback functions may return a \code{data.frame}. When a \code{data.frame} is
#' returned that will become the current state of the data table. If anything
#' else is returned then the internal \code{data.frame} will be used.
#'
#' @return Returns a list of reactive values. \code{return_values$data()} contains
#'  the current state of DTedit's copy of the data. \code{return_values$edit.count()}
#'  contains the number of edits done within DTedit (does not include changes to DTedit's
#'  copy of the data secondary to changes in \code{thedataframe}, if \code{thedataframe} is a reactive)
#'
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param session Shiny output object passed from the server.
#' @param name the name of the UI output. That is, put \code{uiOutput(name)} where
#'        you want the DataTable in \code{ui.R}. When using more that one \code{dtedit}
#'        within a Shiny application the name must be unique.
#' @param thedataf a data frame to view and edit and update (Can be a reactive)
#' @param view.cols character vector with the column names to show in the DataTable.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols character vector with the column names the user can edit/add.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols.size list with max fields sizes. Only for textAreaInput and textInput.
#' @param edit.require.cols character vector with the column names required the user must edit/add.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.require.label the label of require message.
#' @param edit.label.cols character vector with the labels to use on the edit
#'        and add dialogs. The length and order of \code{code.cols.labels} must
#'        correspond to \code{edit.cols}.
#' @param view.label.cols character vector with the labels to use on the view
#'        The length and order of \code{code.cols.labels} must
#'        correspond to \code{view.cols}.
#' @param input.types a character vector where the name corresponds to a column
#'        in \code{edit.cols} and the value is the input type. Possible values
#'        are \code{dateInput}, \code{selectInput}, \code{numericInput},
#'        \code{textInput}, \code{textAreaInput}, or \code{passwordInput}.
#'        The most common case where this parameter is desirable is when a text
#'        area is required instead of a simple text input.
#' @param input.choices a list of character vectors. The names of each element in the list must
#'        correpsond to a column name in the data. The value, a character vector, are the options
#'        presented to the user for data entry.
#' @param selectize Whether to use selectize.js or not. See \code{\link{selectInput}} for more info.
#' @param defaultPageLength number of rows to show in the data table by default.
#' @param modal.size the size of the modal dialog. See \code{\link{modalDialog}}.
#' @param text.width width of text inputs.
#' @param textarea.width the width of text area inputs.
#' @param textarea.height the height of text area inputs.
#' @param date.width the width of data inputs
#' @param date.format the default for data format inputs. data.format.db - Defautl: 'yyyy-mm-dd'
#' @param date.format.db the default for data format inputs from database - Default: 'Y-m-d'
#' @param date.method the default methods for DT::formatDate.
#' @param numeric.width the width of numeric inputs.
#' @param select.width the width of drop down inputs.
#' @param title.delete the title of the dialog box for deleting a row.
#' @param title.edit the title of the dialog box for editing a row.
#' @param title.add the title of the dialog box for inserting a new row.
#' @param label.delete the label of the delete button.
#' @param title.delete.confirmation deletion confirmation message.
#' @param label.edit the label of the edit button.
#' @param label.add the label of the add button.
#' @param label.copy the label of the copy button.
#' @param show.delete whether to show/enable the delete button.
#' @param show.update whether to show/enable the update button.
#' @param show.insert whether to show/enable the insert button.
#' @param show.copy whether to show/enablre the copy button.
#' @param easyClose If TRUE, the modal dialog can be dismissed by clicking outside the dialog box, or be pressing the Escape key. 
#'        If FALSE (the default), the modal dialog can't be dismissed in those ways; instead it must be dismissed by clicking on the dismiss button
#' @param callback.delete a function called when the user deletes a row. This function should
#'        return an updated data.frame.
#' @param callback.update a function called when the user updates a row. This function should
#'        return an updated data.frame.
#' @param callback.insert a function called when the user inserts a new row. This function should
#'        return an updated data.frame.
#' @param click.time.threshold This is to prevent duplicate entries usually by double clicking the
#'        save or update buttons. If the user clicks the save button again within this amount of
#'        time (in seconds), the subsequent click will be ignored. Set to zero to disable this
#'        feature. For developers, a message is printed using the warning function.
#' @param datatable.options options passed to \code{\link{DT::renderDataTable}}.
#'        See \link{https://rstudio.github.io/DT/options.html} for more information.
#' @param escape Escaping Table Content - The argument escape determines whether the HTML entities in the table are escaped or not.
#'        See \link{https://rstudio.github.io/DT} for more information.
#' @family Datatable Edit functions
#' @export
dtedit2 <- function(input, output,
                    session,
                    thedataf,
                    view.cols = names(shiny::isolate(if(shiny::is.reactive(thedataf)) {thedataf()} else {thedataf})),
                    view.label.cols = shiny::isolate(view.cols),
                    edit.cols = names(shiny::isolate(if(shiny::is.reactive(thedataf)) {thedataf()} else {thedataf})),
                    edit.cols.size = list(), 
                    edit.label.cols = shiny::isolate(edit.cols),
                    edit.require.cols = NULL,
                    edit.require.label = 'The following fields are required: ',
                    input.types = c(),  # test
                    input.choices = NULL,
                    selectize = TRUE,
                    modal.size = 'm',
                    text.width = '100%',
                    textarea.width = '570px',
                    textarea.height = '200px',
                    date.width = '100px',
                    date.format = 'yyyy-mm-dd',
                    date.format.db = '%Y-%m-%d',
                    date.method = NULL,  #'toLocaleDateString'
                    numeric.width = '100px',
                    select.width = '100%',
                    defaultPageLength = 10,
                    title.delete = 'Delete',
                    title.delete.confirmation = 'Are you sure you want to delete this record?',
                    title.edit = 'Edit',
                    title.add = 'New',
                    label.delete = 'Delete',
                    label.edit = 'Edit',
                    label.add = 'New',
                    label.copy = 'Copy',
                    label.cancel = 'Cancel',
                    label.save = 'Save',
                    show.delete = TRUE,
                    show.update = TRUE,
                    show.insert = TRUE,
                    show.copy = TRUE,
                    easyClose = FALSE,
                    callback.delete = function(data, row) { },
                    callback.update = function(data, olddata, row) { },
                    callback.insert = function(data, row) { },
                    click.time.threshold = 2, # in seconds
                    datatable.options = list(pageLength = defaultPageLength),
                    escape = FALSE
) {
  
  message("* DtEdit2 Version  : ", version())
  message('- data - format    : ', date.format)
  message('- Current namespace: ', getAnywhere('input')$where)
  message('- session (token)  : ', format(session$token))
  
  
  # if a reactive has been passed, obtain the value
  thedata <- if(shiny::is.reactive(shiny::isolate(thedataf)))
  {shiny::isolate(thedataf())} else {thedataf}
  
  message("- the Data (1): ", print(head(thedata,1)))
  
  # Some basic parameter checking
  dataCheck(thedata, edit.cols, edit.label.cols, view.cols, view.label.cols, edit.require.cols)
  
  name <- "editdt"
  ui_output = name
  
  #name <- paste0(name, '_', token)
  
  DataTableName <- paste0(name, 'dt')
  message('DataTableName: ', DataTableName)
  message('- New Name: ', name)
  
  result <- shiny::reactiveValues()
  result$thedata <- thedata
  result$view.cols <- view.cols
  result$view.label.cols <- view.label.cols
  result$edit.cols <- edit.cols
  result$edit.count <- 0 # number of edits (Add/Delete/Edit/Copy) through dtedit
  
  dt.proxy <- DT::dataTableProxy(DataTableName)
  
  # internal functions
  
  updateData <- function(proxy, pdata, ...) {
    #browser()
    # Convert any list columns to characters before displaying
    for(i in 1:ncol(pdata)) {
      if(is.list(pdata[,i])) {
        pdata[,i] <- sapply(pdata[,i], FUN = function(x) { paste0(x, collapse = ', ') })
      }
    }
    DT::replaceData(proxy, pdata, ...)
  }
  
  
  #put correct inputTypes
  inputTypes <- inputTypes.go(thedata, edit.cols, input.types)
  
  # correct POSIXlt.Date for show
  datacols <- getDateCols(thedata)
  
  # Convert any list columns to characters before displaying
  thedata <- list2char(thedata)
  
  thedata <- thedata[,view.cols]
  
  #removes mismatched columns
  datacols <- datacols[datacols %in% names(thedata)]
  
  if (is.null(date.method)) {
    renderedDT <- DT::renderDataTable({
      thedata
    }, escape = escape, options = datatable.options, server=TRUE, selection='single', rownames=FALSE, colnames = view.label.cols )
  } else {
    # thedataDT <- DT::datatable(thedata) %>% DT::formatDate(columns = datacols,
    #                                    method = date.method) # 'toLocaleDateString'
    # renderedDT <- DT::renderDataTable({
    #   thedataDT # thedata[,view.cols]
    # }, options = datatable.options, server=TRUE, selection='single', rownames=FALSE, colnames = view.label.cols )
    
    thedataDT <- DT::datatable(thedata,
                               options = datatable.options,
                               selection='single',
                               escape = escape,
                               rownames=FALSE,
                               colnames = view.label.cols) %>% 
      DT::formatDate(columns = datacols,
                     method = date.method, # 'toLocaleDateString'
      ) 
    renderedDT <- DT::renderDataTable({
      thedataDT # thedata[,view.cols]
    }, escape = escape, server=TRUE)
  }
  
  output[[DataTableName]] <- renderedDT
  shiny::outputOptions(output, DataTableName, suspendWhenHidden = FALSE)
  
  output[[paste0(name, '_message')]] <- shiny::renderText('')
  
  
  
  ##### Build the UI for the DataTable and buttons ###########################
  build.ui <- function(name, DataTableName, 
                       show.insert, show.update, show.delete, show.copy,
                       label.add, label.edit, label.delete, label.copy ) {
    #message("output[[name]]: ", name)
    #message('DataTableName: ',DataTableName)
    #message('name: ', name)
    
    tmpT <- shiny::renderUI({
      ns <- session$ns # namespace for module
      shiny::div(
        if(show.insert) { shiny::actionButton(ns(paste0(name, '_add')), label.add) },
        if(show.update) { shiny::actionButton(ns(paste0(name, '_edit')), label.edit) },
        if(show.delete) { shiny::actionButton(ns(paste0(name, '_remove')), label.delete) },
        if(show.copy) { shiny::actionButton(ns(paste0(name, '_copy')), label.copy) },
        shiny::br(), shiny::br(), DT::dataTableOutput(ns(DataTableName))
      )
    })
    return (tmpT)
  }
  
  
  
  
  
  ##### Build the UI for the DataTable and buttons ###########################
  output[[ui_output]] <- build.ui(name, DataTableName, 
                                  show.insert, show.update, show.delete, show.copy,
                                  label.add, label.edit, label.delete, label.copy )
  shiny::outputOptions(output, ui_output, suspendWhenHidden = FALSE)
  
  
  ###### ADD MODAL #####################################################
  
  addModal <- function(row, values) {
    ns <- session$ns
    output[[paste0(name, '_message')]] <- shiny::renderText('')
    fields <- getFields(session, '_add_', values,
                        edit.cols, edit.cols.size, edit.require.cols,
                        edit.label.cols, inputTypes, name, 
                        date.format, date.format.db, date.width, input.choices,
                        select.width, result, numeric.width,
                        textarea.width, textarea.height, text.width,
                        selectize)
    shiny::modalDialog(title = title.add,
                       shiny::div(shiny::textOutput(ns(paste0(name, '_message'))), style='color:red'),
                       fields,
                       footer = shiny::column(shiny::modalButton(label.cancel), #'Cancel'
                                              shiny::actionButton(ns(paste0(name, '_insert')),
                                                                  label.save), #'Save'
                                              width=12),
                       size = modal.size, 
                       easyClose = easyClose
    )
  }
  
  
  ##### Insert functions #####################################################
  
  shiny::observeEvent(input[[paste0(name, '_add')]], {
    if(!is.null(row)) {
      shiny::showModal(addModal())
      addJsInput(session, name, '_add_', edit.cols, edit.cols.size)
    }
  })
  
  
  shiny::observeEvent(input[[paste0(name, '_insert')]], {
    if(!is.na(insert.click)) {
      lastclick <- as.numeric(Sys.time() - insert.click, units = 'secs')
      message('last_click_i: ', lastclick)
      if(lastclick < click.time.threshold) {
        warning(paste0('Double click detected. Ignoring insert call for ', name, '.'))
        return()
      }
    }
    insert.click <- Sys.time()
    browser()
    newdata <- shiny::isolate(result$thedata)
    row <- nrow(newdata) + 1
    newdata[row,] <- NA
    lReq <- list() # TODO future use
    lack <- c()
    lack <- checkReq(input,'_add_',
                     name, edit.require.cols, edit.label.cols, edit.cols)
    lReq <- (length(lack)==0)
    if (!all(unlist(lReq))) {
      # need field
      msg <- edit.require.label
      msg <- paste0(msg,toString(lack), '.')
      output[[paste0(name, '_message')]] <<- shiny::renderText(msg)
      return(FALSE)
    }
    for(i in edit.cols) {
      input_add <- shiny::isolate(input[[paste0(name, '_add_', i)]])
      if (input.types[i] %in% c('dateInput')) {
        # input_add <- ''  # I don't remember.
      }
      # message(paste0("input_add: ", input_add, ' i: ', i))
      if(inputTypes[i] %in% c('selectInputMultiple')) {
        newdata[[i]][row] <- list(input_add)
      } else {
        newdata[row,i] <- input_add
      }
    }
    
    tryCatch({
      callback.data <- callback.insert(data = newdata, row = row)
      if(!is.null(callback.data) & is.data.frame(callback.data)) {
        result$thedata <- callback.data
      } else {
        result$thedata <- newdata
      }
      shiny::isolate({
        updateData(dt.proxy,
                   result$thedata[,view.cols],
                   rownames = FALSE)
        result$edit.count <- result$edit.count + 1
      })
      shiny::removeModal()
      return(TRUE)
    }, error = function(e) {
      output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
      return(FALSE)
    })
  })
  
  ##### Copy functions #######################################################
  
  shiny::observeEvent(input[[paste0(name, '_copy')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::isolate({
          shiny::showModal(addModal(values=result$thedata[row,]))
        })
        addJsInput(session, name, '_add_', edit.cols, edit.cols.size)
      }
    }
  })
  
  ##### React to changes in 'thedataf' if that variable is a reactive ######
  
  if (shiny::is.reactive(thedataf)) {
    shiny::observeEvent(thedataf(), {
      #browser()
      result$thedata <- as.data.frame(shiny::isolate(thedataf()))
      if (nrow(result$thedata)==0) {
        res_data <- f_remove_columns(result$thedata,view.cols, FALSE)
      } else {
        res_data <- result$thedata[,view.cols, drop=FALSE]
      }
      updateData(dt.proxy, res_data,
                 # was "result$thedata[,view.cols]",
                 # but that returns vector (not dataframe)
                 # if view.cols is only a single column
                 rownames = FALSE)
    })
  }
  
  
  ##### Update functions #####################################################
  
  editModal <- function(row) {
    #browser()
    ns <- session$ns
    output[[paste0(name, '_message')]] <- shiny::renderText('')
    shiny::isolate({
      fields <- getFields(session, '_edit_', values=result$thedata[row,],
                          edit.cols, edit.cols.size, edit.require.cols,
                          edit.label.cols, inputTypes, name, 
                          date.format, date.format.db, date.width, input.choices,
                          select.width, result, numeric.width,
                          textarea.width, textarea.height, text.width,
                          selectize)
    })
    shiny::modalDialog(title = title.edit,
                       shiny::div(shiny::textOutput(ns(paste0(name, '_message'))), style='color:red'),
                       fields,
                       footer = shiny::column(shiny::modalButton(label.cancel), # CANCEL
                                              shiny::actionButton(ns(paste0(name, '_update')), label.save), #Save
                                              width=12),
                       size = modal.size,
                       easyClose = easyClose
    )
  }
  
  shiny::observeEvent(input[[paste0(name, '_edit')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(editModal(row))
        addJsInput(session, name, '_edit_', edit.cols, edit.cols.size)
      }
    }
  })
  
  
  shiny::observeEvent(input[[paste0(name, '_update')]], {
    if(!is.na(update.click)) {
      lastclick <- as.numeric(Sys.time() - update.click, units = 'secs')
      if(lastclick < click.time.threshold) {
        warning(paste0('Double click detected. Ignoring update call for ', name, '.'))
        return()
      }
    }
    update.click <- Sys.time()
    xtag <- '_edit_'
    browser()
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        newdata <- shiny::isolate(result$thedata)
        # by dms
        lReq <- list() # TODO future use
        lack <- c()
        lack <- checkReq(input, xtag, # '_edit_'
                         name, edit.require.cols, edit.label.cols, edit.cols)
        lReq <- (length(lack)==0)
        if (!all(unlist(lReq))) {
          msg <- edit.require.label
          msg <- paste0(msg,toString(lack),'.')
          output[[paste0(name, '_message')]] <<- shiny::renderText(msg)
          return(FALSE)
        }
        for(i in edit.cols) {
          input_edit <- shiny::isolate(input[[paste0(name, xtag, i)]])
          if (input.types[i] %in% c('dateInput')) {
            # input_edit<- ''  - Can't remember why.
          }
          if(inputTypes[i] %in% c('selectInputMultiple')) {
            newdata[[i]][row] <- list(input_edit)
          } else {
            newdata[row,i] <- input_edit
          }
        }
        
        tryCatch({
          callback.data <- callback.update(data = newdata,
                                           olddata = shiny::isolate(result$thedata),
                                           row = row)
          shiny::isolate({
            if(!is.null(callback.data) & is.data.frame(callback.data)) {
              result$thedata <- callback.data
            } else {
              result$thedata <- newdata
            }
            updateData(dt.proxy,
                       result$thedata[,view.cols],
                       rownames = FALSE)
            result$edit.count <- result$edit.count + 1
          })
          shiny::removeModal()
          return(TRUE)
        }, error = function(e) {
          output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
          return(FALSE)
        })
      }
    }
    return(FALSE)
  })
  
  
  ##### Delete functions #####################################################
  
  
  deleteModal <- function(row) {
    ns <- session$ns 
    fields <- list()
    # TODO - Put Description, no field names.
    # TODO - Format Text
    shiny::isolate({
      for(i in view.cols) {
        fields[[i]] <- shiny::div(paste0(i, ' = ', result$thedata[row,i]))
      }
    })
    output[[paste0(name, '_message')]] <- shiny::renderText('')
    shiny::modalDialog(title = title.delete,
                       shiny::div(shiny::textOutput(ns(paste0(name, '_message'))), style='color:red'),
                       shiny::p(title.delete.confirmation), # 'Are you sure you want to delete this record?'
                       fields,
                       footer = shiny::column(shiny::modalButton(label.cancel), #CANCEL
                                              shiny::actionButton(ns(paste0(name, '_delete')),
                                                                  label.delete), # 'Delete'
                                              width=12),
                       size = modal.size,
                       easyClose = FALSE
    )
  }
  
  shiny::observeEvent(input[[paste0(name, '_remove')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(deleteModal(row))
      }
    }
  })
  
  shiny::observeEvent(input[[paste0(name, '_delete')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    shiny::isolate({
      if(!is.null(row)) {
        if(row > 0) {
          newdata <- callback.delete(data = result$thedata, row = row)
          if(!is.null(newdata) & is.data.frame(newdata)) {
            result$thedata <- newdata
          } else {
            result$thedata <- result$thedata[-row,]
          }
          updateData(dt.proxy,
                     result$thedata[,view.cols],
                     rownames = FALSE)
          result$edit.count <- result$edit.count + 1
          shiny::removeModal()
          return(TRUE)
        }
      }
    })
    return(FALSE)
  })
  
  # return token to know the actual session
  return(list(thedata = reactive({result$thedata}),
              edit.count = reactive({result$edit.count}),
              token = session$token))
}

# internal functions

nothing <- function() {
  # cmd clear and rebuild
  devtools::document()
  usethis::use_package_doc()
  devtools::load_all('.')
  devtools::install()
  shiny::runApp('inst/shiny_demo/app-tst.R')
  devtools::build()
  devtools::build(binary = TRUE, args = c('--preclean'))
}

