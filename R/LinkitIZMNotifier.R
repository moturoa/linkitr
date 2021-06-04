#' Implementation of a notification system between IZM and LinkIt
#' @export
LinkitIZMNotifier <- R6::R6Class(inherit = LinkItEngine,
  public = list(
    
    initialize = function(config_name, schema, pool, config_file = "conf/config.yml"){
      
      flog.info("DB Connection", name = "DBR6")
      flog.info("Using Postgres - devpostgres02", name = "DBR6")
      
      self$pool <- pool
      
      self$schema <- schema
      response <- try({
        shintobag::shinto_db_connection(what = config_name, 
                                        pool = pool, 
                                        file = config_file)
      })
      
      if(!inherits(response, "try-error")){
        self$con <- response
      }
      
    },
    
    #' @description Retrieve IZM notifications for all dossiers of which the 
    #' current user is the coordinator, and the dossier is active,
    #' and the notification has not been removed yet.
    #' @param userid
    get_notificaties = function(userid){
      
      my_bsns <- self$get_personen_active_dossiers(userid)
      
      self$read_table("notificaties", lazy = TRUE) %>%
        filter(pseudo_bsn %in% !!my_bsns, removed == 0) %>%
        collect
      
    },
    
    #' Make a notification invisible
    #' @param id Id of the notificatie
    remove_notificatie = function(id){
      
      ids <- private$to_sql_string(id)
      DBIdbExecute(self$con, glue("update {self$schema}.notificaties set",
                               " removed = 1 where id in {ids}"))
      
    },
    
    
    person_is_in_linkit_dossier = function(pseudo_bsn){
      
      linkit_bsns <- self$get_personen_active_dossiers(method = "all")
      pseudo_bsn %in% linkit_bsns
    },

    append_notificatie = function(pseudo_bsn, userid){
      
      self$append_data("notificaties", 
                       tibble::tibble(
                         id = uuid::UUIDgenerate(),
                         timestamp = as.character(Sys.time()),
                         userid = userid,
                         pseudo_bsn = pseudo_bsn, 
                         removed = 0,
                         comment = ""
                       ))
      
    }
    
  ),
  
  private = list(
    to_sql_string = function(x){
      
      paste0(
        "('",
        paste(x, collapse="','"),
        "')"
      )
      
    }
    
  )
  
)
