#' Show dtedit version
#'
#' @return version
#'
#' @export
version <- function() {
  res <- '0.0.20x5'
  return(res)
}


insert.click <- NA
update.click <- NA

pkg.env <- new.env(parent = emptyenv())
pkg.env$books <- F
pkg.env$names <- F


#' Get current environment
#'
#' @return version
#'
#' @export
getEnv <- function() {
  return(pkg.env)
}


#' Return if \code{uiOutput(name)} is running
#'
#' @param name the name of the UI output.
#' @return version
#'
#' @export
running <- function(name = NULL) {
  if (is.null(name)) return(ls(pkg.env))
  dtname <- paste0(name, '_dt')
  d.1 <- name
  d.2 <- get0(name, pkg.env)
  message('Datatablename (d1): ', d.1)
  message('pkg.env (d2): ', d.2)
  message('dtname: ', dtname)
  if (is.null(d.2)) {
    res <- FALSE
  } else {
    res <- (d.2 == TRUE)  
  }
  return(res)
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
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param name the name of the UI output. That is, put \code{uiOutput(name)} where
#'        you want the DataTable in \code{ui.R}. When using more that one \code{dtedit}
#'        within a Shiny application the name must be unique.
#' @param thedata a data frame to view and edit.
#' @param view.cols character vector with the column names to show in the DataTable.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols character vector with the column names the user can edit/add.
#'        This can be a subset of the full \code{data.frame}.
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
#' @param date.format the default for data format inputs.
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
#' @export
dtedit2 <- function(input, output, name, thedata,
				   view.cols = names(thedata),
				   view.label.cols = view.cols,
				   edit.cols = names(thedata),
				   edit.label.cols = edit.cols,
				   edit.require.cols = NULL,
				   edit.require.label = 'The following fields are required: ',
				   input.types,
				   input.choices = NULL,
				   selectize = TRUE,
				   modal.size = 'm',
				   text.width = '100%',
				   textarea.width = '570px',
				   textarea.height = '200px',
				   date.width = '100px',
				   date.format = 'yyyy-mm-dd',
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
				   callback.delete = function(data, row) { },
				   callback.update = function(data, olddata, row) { },
				   callback.insert = function(data, row) { },
				   click.time.threshold = 2, # in seconds
				   datatable.options = list(pageLength=defaultPageLength)
) {
  message("DtEdit2 Version:",version())
  message('data - format: ', date.format)
  message('Current namespace: ', getAnywhere('input')$where)
  message('- env: ',format(pkg.env))
  message("the Data", thedata)
  
	# Some basic parameter checking
	if(!is.data.frame(thedata) | ncol(thedata) < 1) {
		stop('Must provide a data frame with at least one column.')
	} else if(length(edit.cols) != length(edit.label.cols)) {
		stop('edit.cols and edit.label.cols must be the same length.')
	} else if(length(view.cols) != length(view.label.cols)) {
	    stop('view.cols and view.label.cols must be the same length.')
	} else if(!all(view.cols %in% names(thedata))) {
		stop('Not all view.cols are in the data.')
	} else if(!all(edit.cols %in% names(thedata))) {
		stop('Not all edit.cols are in the data.')
	}

  if(!all(edit.require.cols %in% names(thedata))) {
    stop('a edit.require.cols not in the data')
  }
  
	DataTableName <- paste0(name, 'dt')
	
	message('name: ', DataTableName)

	result <- shiny::reactiveValues()
	result$thedata <- thedata
	result$view.cols <- view.cols
	result$view.label.cols <- view.label.cols
	result$edit.cols <- edit.cols

	dt.proxy <- DT::dataTableProxy(DataTableName)

	selectInputMultiple <- function(...) {
		shiny::selectInput(multiple = TRUE, selectize = selectize, ...)
	}

	valid.input.types <- c('dateInput', 'selectInput', 'numericInput',
						   'textInput', 'textAreaInput', 'passwordInput', 'selectInputMultiple')
	inputTypes <- sapply(thedata[,edit.cols], FUN=function(x) {
		switch(class(x),
			   list = 'selectInputMultiple',
			   character = 'textInput',
			   Date = 'dateInput',
			   factor = 'selectInput',
			   integer = 'numericInput',
			   numeric = 'numericInput')
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

	# Convert any list columns to characters before displaying
	for(i in 1:ncol(thedata)) {
		if(nrow(thedata) == 0) {
			thedata[,i] <- character()
		} else if(is.list(thedata[,i])) {
			thedata[,i] <- sapply(thedata[,i], FUN = function(x) { paste0(x, collapse = ', ') })
		}
	}

	output[[DataTableName]] <- DT::renderDataTable({
		thedata[,view.cols]
	}, options = datatable.options, server=TRUE, selection='single', rownames=FALSE, colnames = view.label.cols )

	getFields <- function(typeName, values) {
		fields <- list()
		for(i in seq_along(edit.cols)) {
		  if (all(edit.cols[i] %in% edit.require.cols)) {
		    # TODO - show * in red.
		    edit.label.cols[i] <- paste0(edit.label.cols[i],'(*)')
		    #edit.label.cols[i] <- paste0(edit.label.cols[i], tags$strong(style='color:FF0000','(*)'))
		    message('n*: ', edit.label.cols[i])
		  }
			if(inputTypes[i] == 'dateInput') {
				value <- ifelse(missing(values),
								as.character(Sys.Date()),
								as.character(values[,edit.cols[i]]))
				fields[[i]] <- dateInput(paste0(name, typeName, edit.cols[i]),
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
				fields[[i]] <- selectInputMultiple(paste0(name, typeName, edit.cols[i]),
										   label=edit.label.cols[i],
										   choices=choices,
										   selected=value,
										   width=select.width)

			} else if(inputTypes[i] == 'selectInput') {
				value <- ifelse(missing(values), '', as.character(values[,edit.cols[i]]))
				fields[[i]] <- shiny::selectInput(paste0(name, typeName, edit.cols[i]),
										   label=edit.label.cols[i],
										   choices=levels(result$thedata[,edit.cols[i]]),
										   selected=value,
										   width=select.width)
			} else if(inputTypes[i] == 'numericInput') {
				value <- ifelse(missing(values), 0, values[,edit.cols[i]])
				fields[[i]] <- shiny::numericInput(paste0(name, typeName, edit.cols[i]),
											label=edit.label.cols[i],
											value=value,
											width=numeric.width)
			} else if(inputTypes[i] == 'textAreaInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- shiny::textAreaInput(paste0(name, typeName, edit.cols[i]),
											 label=edit.label.cols[i],
											 value=value,
											 width=textarea.width, height=textarea.height)
			} else if(inputTypes[i] == 'textInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- shiny::textInput(paste0(name, typeName, edit.cols[i]),
										 label=edit.label.cols[i],
										 value=value,
										 width=text.width)
			} else if(inputTypes[i] == 'passwordInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- shiny::passwordInput(paste0(name, typeName, edit.cols[i]),
										 label=edit.label.cols[i],
										 value=value,
										 width=text.width)
			} else {
				stop('Invalid input type!')
			}
		}
		return(fields)
	}

	output[[paste0(name, '_message')]] <- shiny::renderText('')

	updateData <- function(proxy, data, ...) {
		# Convert any list columns to characters before displaying
		for(i in 1:ncol(data)) {
			if(is.list(data[,i])) {
				data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
			}
		}
		DT::replaceData(proxy, data, ...)
	}

	
	# check required fields
  checkReq <- function(input, tag) {
    lReq <- list() # TODO future use for color require fields
    lack <- c()
    for(i in edit.cols) {
      input_add <- input[[paste0(name, tag, i)]]
      lReq[i] <- TRUE
      if (all(i %in% edit.require.cols)) {
        if (is.null(input_add) || identical(input_add,'') ) {
          lReq[i] <- FALSE
          elem <- edit.label.cols[grep(paste0(i),edit.cols)]
          lack <- c(lack,paste0(elem))
        }
      }
    }
    return (lack)
  }	
	
	##### Insert functions #####################################################

  shiny::observeEvent(input[[paste0(name, '_add')]], {
		if(!is.null(row)) {
			shiny::showModal(addModal())
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

		newdata <- result$thedata
		row <- nrow(newdata) + 1
		newdata[row,] <- NA
		lReq <- list() # TODO future use
		lack <- c()
		lack <- checkReq(input,'_add_')
		lReq <- (length(lack)==0)
		for(i in edit.cols) {
		  input_add <- input[[paste0(name, '_add_', i)]]
		  #message('inputTypes[i]: ',inputTypes[i])
			if(inputTypes[i] %in% c('selectInputMultiple')) {
				newdata[[i]][row] <- list(input[[paste0(name, '_add_', i)]])
			} else {
				newdata[row,i] <- input[[paste0(name, '_add_', i)]]
			}
		}

		if (!all(unlist(lReq))) {
		  # need field
		  msg <- edit.require.label
		  msg <- paste0(msg,toString(lack), '.')
		  output[[paste0(name, '_message')]] <<- shiny::renderText(msg)
		  return(FALSE)
		}
		tryCatch({
			callback.data <- callback.insert(data = newdata, row = row)
			if(!is.null(callback.data) & is.data.frame(callback.data)) {
				result$thedata <- callback.data
			} else {
				result$thedata <- newdata
			}
			updateData(dt.proxy,
						result$thedata[,view.cols],
						rownames = FALSE)
			shiny::removeModal()
			return(TRUE)
		}, error = function(e) {
		 	output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
		 	return(FALSE)
		})
	})

	addModal <- function(row, values) {
		output[[paste0(name, '_message')]] <- shiny::renderText('')
		fields <- getFields('_add_', values)
		shiny::modalDialog(title = title.add,
					shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
					fields,
					footer = shiny::column(shiny::modalButton(label.cancel), #'Cancel'
									shiny::actionButton(paste0(name, '_insert'), label.save), #'Save'
									width=12),
					size = modal.size
		)
	}

	##### Copy functions #######################################################

	shiny::observeEvent(input[[paste0(name, '_copy')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				shiny::showModal(addModal(values=result$thedata[row,]))
			}
		}
	})

	##### Update functions #####################################################

	shiny::observeEvent(input[[paste0(name, '_edit')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				shiny::showModal(editModal(row))
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

		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				newdata <- result$thedata
				# by dms
				lReq <- list() # TODO future use
				lack <- c()
				lack <- checkReq(input, '_edit_')
				lReq <- (length(lack)==0)
				for(i in edit.cols) {
					if(inputTypes[i] %in% c('selectInputMultiple')) {
						newdata[[i]][row] <- list(input[[paste0(name, '_edit_', i)]])
					} else {
						newdata[row,i] <- input[[paste0(name, '_edit_', i)]]
					}
				}
				if (!all(unlist(lReq))) {
				  msg <- edit.require.label
				  msg <- paste0(msg,toString(lack),'.')
				  output[[paste0(name, '_message')]] <<- shiny::renderText(msg)
				  return(FALSE)
				}
				
				tryCatch({
					callback.data <- callback.update(data = newdata,
													 olddata = result$thedata,
													 row = row)
					if(!is.null(callback.data) & is.data.frame(callback.data)) {
						result$thedata <- callback.data
					} else {
						result$thedata <- newdata
					}
					updateData(dt.proxy,
								result$thedata[,view.cols],
								rownames = FALSE)
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

	editModal <- function(row) {
		output[[paste0(name, '_message')]] <- shiny::renderText('')
		fields <- getFields('_edit_', values=result$thedata[row,])
		shiny::modalDialog(title = title.edit,
			shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
			fields,
			footer = shiny::column(shiny::modalButton(label.cancel), # CANCEL
							shiny::actionButton(paste0(name, '_update'), label.save), #Save
							width=12),
			size = modal.size
		)
	}

	##### Delete functions #####################################################

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
				shiny::removeModal()
				return(TRUE)
			}
		}
		return(FALSE)
	})

	deleteModal <- function(row) {
		fields <- list()
		# TODO - Put Description, no field names.
		# TODO - Format Text
		for(i in view.cols) {
			fields[[i]] <- shiny::div(paste0(i, ' = ', result$thedata[row,i]))
		}
		shiny::modalDialog(title = title.delete,
					shiny::p(title.delete.confirmation), # 'Are you sure you want to delete this record?'
					fields,
					footer = shiny::column(shiny::modalButton(label.cancel), #CANCEL
									shiny::actionButton(paste0(name, '_delete'), label.delete), # 'Delete'
									width=12),
					size = modal.size
		)
	}

	##### Build the UI for the DataTable and buttons ###########################
  message("output[[name]]: ", name)
	message('DataTableName',DataTableName)
	
	assign(name, TRUE, pkg.env)
	message('name: ', name)
	
	tmpT <- shiny::renderUI({
	  shiny::div(
	    if(show.insert) { shiny::actionButton(paste0(name, '_add'), label.add) },
	    if(show.update) { shiny::actionButton(paste0(name, '_edit'), label.edit) },
	    if(show.delete) { shiny::actionButton(paste0(name, '_remove'), label.delete) },
	    if(show.copy) { shiny::actionButton(paste0(name, '_copy'), label.copy) },
	    shiny::br(), shiny::br(), DT::dataTableOutput(DataTableName)
	  )
	})

	output[[name]] <- tmpT
	
	# for create
	if (1 == 2) {
	  # cmd clear and rebuild
	  devtools::document()
	  devtools::install()
	  usethis::use_package_doc()
	  devtools::document()
	  shiny::runApp('inst/shiny_demo')
	  devtools::build()
	  devtools::build(binary = TRUE, args = c('--preclean'))
	  
	}

	return(result)
}



