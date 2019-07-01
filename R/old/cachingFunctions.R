# require(filehash)

# library("R6")
# 
# CacheDb <- R6Class("R6CacheDb",
#                    ## a wrapper class for RData objects
#                    public = list(
#                      initialize = function(name=NA, path=NA) {
#                        self$db.name = name
#                        self$db.path = path
#                        self$db.path.name = file.path(path, name)
#                        variable.names = character()
#                      }, ## end init
# 
#                      AddObj = function(variable) {
#                        if(length(self$variable.names)==0) {
#                          save(file=self$db.path.name, list=variable)
#                        } else {
#                          var.name <- deparse(substitute(variable))
#                          previous  <- load(self$db.path.name)
#                          
#                          if( length(self$variable.names %in% var.name) ==0 ) {
#                            ## append variable 
#                            self$variable.names <- c(self$variable.names, var.name)
#                            save(list = c(previous, var.name), file = self$db.path.name)
#                            # save(list=var.name, file=self$db.path.name)
#                          } else {
#                            ## substitute variable
#                            assign(variable, get(var.name, envir = parent.frame()))
#                            save(list = unique(c(previous, variable)), file = self$db.path.name)
#                          }
#                        }
#                      }, ## end add
#                      
#                      LoadObj = function(variable.name) {
#                        
#                       
#                      }
#                      
#                    ), ## end public
# 
#                    private = list(
#                      db.name = NULL,
#                      db.path = NULL,
#                      db.path.name = NULL,
#                      variable.names = NULL,
#                       
#                      InitCachingRData = function() {
#                        ## this function creates a new database or select an existing one
#                        if(!file.exists(private$db.path)) {
#                          dir.create(private$db.path, recursive=T)
#                        }
# 
#                        if(!file.exists(db.path.name)) {
#                          #self$add(NULL)
#                        }
# 
#                        return(db)
#                      } ## end init.db
# 
# 
#                    ) ## end private
# ) ## end class

InitCachingDb <- function(db.name, db.path='') {
  ## this function creates a new database or select an existing one
  ## @name: the name of the database file
  ## @path: if not null, creates 
  ## returns: the database object
  if(db.path=='') {
    db.path.name <- db.name
  } else {
    db.path.name <- file.path(db.path, db.name)
    if(!file.exists(db.path)) {
      dir.create(db.path, recursive=T)
    }
  }
  if(!file.exists(db.path.name)) {
    dbCreate(db.path.name)
  }
  db <- dbInit(db.path.name)
  return(db)
}

SaveInCache <- function(db, object, key) {
  ## this function save an object in the database
  ## @db: the database object within to save the object
  ## @object: the object to save
  ## @key: the key value to associate to the object
  ## returns: none
  dbInsert(db, key, object)
}

LoadCachedObject <- function(db, key) {
  ## this function retrieve an object from a database
  ## @db: the database object within the object is saved
  ## @key: the key value associated to the object
  ## returns: the object retrieved from the database
  object <- dbFetch(db, key)
  return(object)
}

