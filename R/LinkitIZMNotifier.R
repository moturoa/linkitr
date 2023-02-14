#' Implementation of a notification system between IZM and LinkIt
#' @export
LinkitIZMNotifier <- R6::R6Class(
  lock_objects = FALSE,
  inherit = LinkItEngine,
  
  public = list(
    
    initialize = function(what, schema, pool, config_file = "conf/config.yml"){
      
      super$initialize(what = what, schema = schema, pool = pool, config_file = config_file)
      
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
      DBI::dbExecute(self$con, glue("update {self$schema}.notificaties set",
                               " removed = 1 where id in {ids}"))
      
    },
    
    
    person_is_in_linkit_dossier = function(pseudo_bsn){
      
      linkit_bsns <- self$get_personen_active_dossiers(method = "all")
      pseudo_bsn %in% linkit_bsns
    },
    
    person_has_recent_notificatie = function(pseudo_bsn, recent_days = 30){
      
      recent <- Sys.Date() - recent_days
      
      person_msg <- self$read_table("notificaties", lazy = TRUE) %>%
        filter(pseudo_bsn == !!pseudo_bsn) %>%
        collect %>%
        mutate(date = as.Date(ymd_hms(timestamp))) %>%
        filter(date >= recent)
      
      nrow(person_msg > 0)
      
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
