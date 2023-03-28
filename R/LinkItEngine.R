#' Business logic of the LinkIt application
#' @description An R6 object with methods for use in the Shiny application `ede_linkt_app`, 
#' and `ede-izm_frontend`. 
#' @importsFrom shintobag shinto_db_connection
#' @importsFrom pool dbPool poolClose
#' @importsFrom R6 R6Class
#' @importsFrom safer encrypt_string decrypt_string
#' @export
LinkItEngine <- R6::R6Class(
  lock_objects = FALSE,
  inherit = shintodb::databaseClass,
  
  public = list(
    
    sel = NULL,
    gemeente = NULL,
    gebruikers = NULL,
    
    initialize = function(gemeente = NULL, schema, pool, config_file = "conf/config.yml", what = NULL){
      
      self$gemeente <- gemeente
      
      # LinkIt default: config entry = 'ede'
      if(is.null(what)){
        what <- tolower(gemeente)  
      }
      
      
      super$initialize(what = what, config_file = config_file, pool = pool, schema = schema)
      
      self$gebruikers <- self$read_table("gebruikers")
      dossierkenmerktype <- self$read_table("dossierkenmerktype")
      objectkenmerktype <- self$read_table("objectkenmerktype")
      
      self$sel <- list(
        coordinatoren = self$make_choices(values_from = "userid",
                                          names_from = "username",
                                          data = self$gebruikers),
        dossierkenmerktypes =  self$make_choices(values_from = "typeid",
                                                 names_from = "naam",
                                                 data = dossierkenmerktype),
        objectkenmerktypes =  self$make_choices(values_from = "typeid",
                                                names_from = "naam",
                                                data = objectkenmerktype)
      )
      
    },
    
    
    
    delete_rows_where = function(table, col_compare, val_compare){
      
      query <- glue("delete from {self$schema}.{table} where {col_compare}= ?val")
      query <- sqlInterpolate(DBI::ANSI(), query, val = val_compare)
      flog.info(query, name = "DBR6")
      
      tm <- try(
        dbExecute(self$con, query)   
      )
      
      return(invisible(!inherits(tm, "try-error")))
    },
    
    delete_rows_where2 = function(table, col_compare, val_compare, col_compare2, val_compare2){
      
      query <- glue("delete from {self$schema}.{table} where {col_compare}= ?val and {col_compare2}= ?val2")
      query <- sqlInterpolate(DBI::ANSI(), query, val = val_compare, val2 = val_compare2)
      flog.info(query, name = "DBR6")
      
      tm <- try(
        dbExecute(self$con, query)   
      )
      
      return(invisible(!inherits(tm, "try-error")))
    },
    
    
    
    get_con = function(){
      if(!is.null(self$con) && dbIsValid(self$con)){
        self$con
      }
    },
    
    has_value = function(table, column, value){
      
      query <- glue("select {column} from {self$schema}.{table} where {column}= ?val")
      query <- sqlInterpolate(DBI::ANSI(), query, val = value)
      
      out <- self$query(query)
      
      nrow(out) > 0
      
    },
    
    
    #---- Algemene niet DB methodes -----
    make_choices = function(values_from, names_from = values_from, data = NULL){
      
      data <- data %>%
        distinct(!!sym(values_from), !!sym(names_from))
      
      out <- data[[values_from]] %>% 
        setNames(data[[names_from]])
      
      # Sorteer op labels, niet op waardes
      out[order(names(out))]
      
    },
    
    
    #------- LinkIt methodes -----
    
    add_user = function(userid, role = 1, username){
      
      self$append_data("gebruikers", 
                       tibble(
                         userid = as.character(userid),
                         userrole = as.integer(role),
                         username = as.character(username),
                         lastlogin = Sys.time(),
                         meta = ""
                       ))
      
    },
    
    username_from_userid = function(userid){
      
      users <- self$read_table("gebruikers", lazy = TRUE) %>%
        filter(userid %in% !!userid) %>%
        collect
      
      ii <- match(userid, users$userid)
      if(length(ii) == 0 || all(is.na(ii))){
        userid
      } else {
        users$username[ii]  
      }
      
      
    },
    

    get_favorites = function(userid, lazy = FALSE){
      
      fav <- self$read_table("favorieten", lazy = TRUE) %>%
        filter(userid == !!userid) %>%
        collect
      
      out <- self$read_table("dossiers", lazy = TRUE) %>% 
        filter(dossierid %in% !!fav$dossierid)
      
      if(lazy){
        return(out)
      } else {
        return(collect(out))
      }
      
    },
    
    is_favorite = function(userid, dossierid){
      
      fav_dos <- self$read_table("favorieten", lazy = TRUE) %>%
        filter(userid == !!userid, dossierid == !!dossierid) %>%
        collect
      
      nrow(fav_dos) > 0
    },
    
    format_dossierid = function(id){
      
      stringr::str_pad(as.character(id), 4, "left", "0")
      
    },
    
    
    add_favorite = function(userid, dossierid){
      
      if(!self$is_favorite(userid, dossierid)){
        
        self$log_audit(userid, dossierid, actie = glue("Dossier {dossierid} gevolgd"))
        
        self$append_data("favorieten",
                         tibble(
                           id = uuid::UUIDgenerate(),
                           dossierid = dossierid,
                           userid = userid
                         ))
      } else {
        return(FALSE)
      }
      
      
    },
    
    delete_favorite = function(userid, dossierid){
      
      self$log_audit(userid, dossierid, actie = glue("Dossier {dossierid} ontvolgd"))
      
      self$delete_rows_where2("favorieten", 
                              col_compare = "dossierid", 
                              val_compare = dossierid, 
                              col_compare2 = "userid", 
                              val_compare2 = userid
      )
      
      
    },
    
    #' @description Toggle favorie dossier for a user
    #' @details Delete the favorite if the dossier is a favorite, otherwise add it.
    #' @return TRUE if successful.
    toggle_favorite = function(userid, dossierid){
      
      if(self$is_favorite(userid, dossierid)){
        self$delete_favorite(userid, dossierid)
      } else {
        self$add_favorite(userid, dossierid)
      }
      
    },
    
    
    laatste_login = function(userid){
      
      last <- self$query(glue("SELECT lastlogin FROM linkit.gebruikers WHERE userid = '{userid}'"))$lastlogin
      
      if(length(last) == 0){
        return(format(Sys.Date()))
      }
      
      if(last != as.character((Sys.Date()))){
        self$execute_query(glue("UPDATE linkit.gebruikers SET lastlogin = '{as.character(Sys.Date())}' WHERE userid = '{userid}'"))
      }
      
      return(last)
    },
    
    next_dossier_id = function(){
      
      maxid <- self$query("select max(dossierid) from linkit.dossiers")
      
      out <- maxid$max + 1
      if(is.na(out))out <- 1
      
      out
      
    },
    
    #' @description Make a new dossier, write to DB
    #' @param dossiername Name of the dossier (char)
    #' @param userid Current user (char)
    #' @param aanmaakdatum Creation date (posixct)
    #' @param verloopdatum Expiry date (posixct)
    #' @param betrokkeninstanties (char)
    #' @return The (usually new) ID of the dossier.
    add_dossier = function(dossiername, 
                           userid, 
                           aanmaakdatum, 
                           verloopdatum, 
                           betrokkeninstanties,
                           aanmelder,
                           id = NULL){
      
      if(is.null(id)){
        id <- self$next_dossier_id()
      }
      
      self$append_data("dossiers", 
                       tibble(
                         dossierid = id,
                         dossiername = dossiername,
                         aanmaker = userid,
                         aanmaakdatum = aanmaakdatum,
                         verloopdatum = verloopdatum,
                         #archiefdatum = 
                         #hoofdadresid =
                         actuele_status = "Actief",
                         betrokkeninstanties = paste(betrokkeninstanties, collapse=";"),
                         aanmelder = aanmelder
                       ))
      
      return(id)
      
    },
    
    #' @description Get a dossier based on its ID
    #' @param dossierid Dossier ID's (can be a vector)
    get_dossier = function(dossierid){
      
      self$read_table("dossiers", lazy = TRUE) %>%
        filter(dossierid %in% !!dossierid) %>%
        collect
      
    },
    
    #' @description Retrieves dossiers for a user 'Actief' or 'Volgen' status for 
    get_my_active_dossiers = function(userid){
      
      dossierids <- self$get_dossierid_coordinatoren(userid)
      
      self$read_table("dossiers", lazy = TRUE) %>%
        filter(dossierid %in% !!dossierids, actuele_status %in%  c("Actief","Volgen")) %>%
        collect
      
    },

    #' @description Find all dossiers with 'Actief' or 'Volgen' status
    get_all_active_dossiers = function(){
      
      self$read_table("dossiers", lazy = TRUE) %>%
        filter(actuele_status %in%  c("Actief","Volgen")) %>%
        collect
      
    },
    
    #' @description Find all dossiers with 'Verwijderd' status
    get_verwijderde_dossiers = function(){
      
      self$read_table("dossiers", lazy = TRUE) %>%
        filter(actuele_status == "Verwijderd") %>%
        collect
      
    },
    
    #' Retrieve all pseudo-BSNs of all persons in active dossiers for the current user.
    #' @return Vector of pseudo-BSNs
    get_personen_active_dossiers = function(userid = NULL, method = c("current_user", "all")){
      
      method <- match.arg(method)
      
      if(method == "current_user"){
        dosid <- self$get_my_active_dossiers(userid) %>%
          pull(dossierid)
      } else {
        dosid <- self$get_all_active_dossiers() %>%
          pull(dossierid)
      }
      
      if(length(dosid) == 0)return(NULL)
      
      self$get_objects(dosid) %>%
        filter(objecttype == "bsn_persoon") %>%
        pull(objectinstance)
      
    },
    
    get_objecten_objectinstance = function(ids){
      
      self$read_table("objecten", lazy = TRUE) %>% 
        filter(objectinstance %in% !!ids) %>%
        collect
      
    },
    
    get_dossierid_personen = function(pseudoid){
      
      doss_ids <- self$get_objecten_objectinstance(pseudoid) %>% 
        pull(dossierid) %>%
        unique
      
      doss_nms <-self$dossier_naam_from_id(doss_ids)
      
      tibble(dossierid = doss_ids, dossiername = doss_nms)
      
    },
    
    
    
    #' @description Find the name of a dossier based on its ID
    dossier_naam_from_id = function(dossierid){
      
      self$get_dossier(dossierid) %>% pull(dossiername)
      
    },
    
    #' @description Really delete a dossier.
    delete_dossier = function(dossierid){
      
      self$delete_rows_where("dossiers", "dossierid", dossierid)
      self$delete_rows_where("favorieten", "dossierid", dossierid)
      self$delete_rows_where("dossierkenmerk", "dossierid", dossierid)
      self$delete_rows_where("coordinatoren", "dossierid", dossierid)
      self$delete_rows_where("objecten", "dossierid", dossierid)
      self$delete_rows_where("persoon_kenmerken", "dossierid", dossierid)
      
    },
    
    #' @description Set status dossier to 'Verwijderd'
    verwijder_dossier = function(dossierid){
      
      tm_now <- Sys.time()
      tm_10yr <- tm_now + lubridate::years(10)
      
      dbExecute(self$con, glue("update {self$schema}.dossiers set",
                               " actuele_status = 'Verwijderd' where dossierid = {dossierid}"))
      dbExecute(self$con, glue("update {self$schema}.dossiers set",
                               " archiefdatum = '{format(tm_now)}' where dossierid = {dossierid}"))
      dbExecute(self$con, glue("update {self$schema}.dossiers set",
                               " verloopdatum = '{format(tm_10yr)}' where dossierid = {dossierid}"))
      
    },
    
    #' @description Undo verwijder_dossier
    undo_verwijder_dossier = function(dossierid){
      
      dbExecute(self$con, glue("update {self$schema}.dossiers set",
                               " actuele_status = 'Actief' where dossierid = {dossierid}"))
      dbExecute(self$con, glue("update {self$schema}.dossiers set",
                               " archiefdatum = NULL where dossierid = {dossierid}"))
      dbExecute(self$con, glue("update {self$schema}.dossiers set",
                               " verloopdatum = NULL where dossierid = {dossierid}"))
    },
    
    
    #' @description Does the given Dossier ID actually exist?
    dossier_exists = function(dossierid){
      
      self$has_value("dossiers", "dossierid", dossierid)  
      
    },
    
    
    #' @description Betrokken instantie toevoegen
    add_instantie = function(instantie, properties = ""){
      
      
      data_have <- self$read_table("instanties", lazy = TRUE) %>% 
        filter(instantie == !!instantie) %>%
        collect
      
      id_max <- self$query("select max(id) from linkit.instanties")[[1]]
      
      n_have <- nrow(data_have)
      
      if(n_have == 0){
        self$append_data(
          "instanties",
          tibble(
            id = id_max + 1,
            instantie = instantie,
            properties = properties
          )
        ) 
        
        return(1)
        
      } else {
        return(-1)
      }
      
      
    },
    
    list_instanties = function(){
      
      self$read_table("instanties") %>% 
        pull(instantie) %>%
        stringr::str_sort()
      
    },
    
    
    #' @description Log an audit action
    log_audit = function(userid, dossierid, actie, pseudo_bsn = ""){
      
      self$append_data("audittrail", 
                       tibble(
                         auditid = uuid::UUIDgenerate(),
                         userid = userid,
                         dossierid = dossierid,
                         actie = actie,
                         tijdstip = Sys.time(),
                         pseudo_bsn = pseudo_bsn
                       ))
      
      
    },
    
    #' @description Get dossier kenmerken for a dossierid
    get_audit = function(dossierid){
      
      self$read_table("audittrail", lazy = TRUE) %>%
        filter(dossierid %in% !!dossierid) %>%
        collect
      
    },
    
    
    #' @description Is this objectinstance a member of dossier?
    #' @return TRUE if object is in the dossier, FALSE otherwise.
    is_object_in_dossier = function(objectinstance, dossierid){
      
      out <- self$read_table("objecten", lazy = TRUE) %>%
        filter(dossierid == !!dossierid, objectinstance == !!objectinstance) %>%
        collect
      
      nrow(out) > 0
    },
    
    
    #' @description Add person(s) (who has a BSN) to a dossier
    #' @param ids Vector of pseudo-BSNs, or persoon-IDs (in case of unknown persons)
    #' @param dossierid Dossier ID (char)
    #' @param userid The current user (for audit trail)
    #' @return TRUE if successful, FALSE if object already added to dossier.
    add_object_to_dossier = function(ids, dossierid, userid,
                                     type = c("bsn_persoon","onbekend_persoon",
                                              "bag_locatie","onbekend_locatie"),
                                     dossierid_check = TRUE){  
      
      
      type <- match.arg(type)
      
      if(dossierid_check && !self$dossier_exists(dossierid)){
        warning(glue("Trying to add object to non-existent Dossier ID: {dossierid}"))
        return(FALSE)
      }
      
      
      # current objects in dossier
      cur_ids <- self$get_objects(dossierid) %>% pull(objectinstance)
      
      ids_exist <- intersect(ids, cur_ids)
      if(length(ids_exist)){
        
        if(length(ids_exist) == length(ids)){
          
          # object already in dossier
          return(FALSE)
          
        } else {
          
          # some objects already in dossier, add only the ones that are not.
          ids <- setdiff(ids, cur_ids)
          
        }
        
      }
      
      self$append_data("objecten", tibble(
        objectid = uuid::UUIDgenerate(n = length(ids)),
        dossierid = dossierid,
        objecttype = type,
        objectinstance = as.character(ids),  # pseudo_bsn's, adresseerbaarobject, etc.
        toevoegdatum = Sys.time(),
        verwijderdatum = as.POSIXct(NA)
      ))
      
      if(type == "bsn_persoon"){
        pids <- paste(ids, collapse=';')
        self$log_audit(userid, dossierid, actie = glue("Object type {type} toegevoegd aan Dossier {dossierid},",
                                                       " pseudo-BSN: {pids}"),
                       pseudo_bsn = pids)
      } else {
        self$log_audit(userid, dossierid, actie = glue("Object type {type} toegevoegd aan Dossier {dossierid}."))
      }
      
    },
    
    #' @description Remove an object from a dossier
    remove_object_from_dossier = function(ids, dossierid, userid){
      
      for(id in ids){
        self$log_audit(userid, dossierid, actie = glue("Verwijder object: {id} uit dossier: {dossierid}"))  
        self$delete_rows_where2("objecten", "objectinstance", id, "dossierid", dossierid)
      }
      
    },
    
    #' @description Add kenmerken to a persoon
    add_persoon_kenmerken = function(dossierid,
                                     id,
                                     geboortedatum = "",
                                     geslacht = ""){
      
      ts <- format(Sys.time())
      
      leeftijd <- floor(as.numeric(difftime(Sys.Date(), as.Date(geboortedatum),
                           units = "weeks")) / 52)
      
      self$append_data("persoon_kenmerken", tibble(
        timestamp = ts,
        dossierid = dossierid,
        pseudo_bsn = id,
        leeftijd = leeftijd,
        geslacht = geslacht
      ))
      
    },
    
    
    #' @description Register a new 'unknown' person
    #' @param name
    #' @param adresseerbaarobject
    #' @param woonadresomschrijving
    #' @return The new ID of the person
    add_unknown_person = function(name, 
                                  adresseerbaarobject = NULL, 
                                  locatieid = "",
                                  woonadresomschrijving = ""){
      
      persoon_id <- uuid::UUIDgenerate()
      
      if(is.null(adresseerbaarobject)){
        woonadres_type <- "locatie"
        woonadres_id <- locatieid
      } else {
        woonadres_type <- "adresseerbaarobject"
        woonadres_id <- adresseerbaarobject
      }
      
      # Onbekend persoon op onbekend adres
      self$append_data("persoon", 
                       tibble(
                         persoonid = persoon_id,
                         persoonname = name,
                         woonadrestype = woonadres_type,
                         woonadresid = woonadres_id,
                         woonadresomschrijving = woonadresomschrijving,  # 'extra' veld (?)
                         gekoppeldbsn = ""  # kan later ingevuld worden
                       ))
      
      
      return(persoon_id) 
    },
    
    #' @description Get a person (an unknown person) by ID
    get_persoon = function(id){
      
      self$read_table("persoon", lazy = TRUE) %>%
        filter(persoonid %in% !!id) %>%
        collect
      
    },
    
    #' @description Update name, BSN of an unknown person
    edit_unknown_persoon = function(id, naam, bsn){
      
      if(!is.null(naam)){
        self$replace_value_where("persoon", col_replace = "persoonname", val_replace = naam,
                                 col_compare = "persoonid", val_compare = id)  
      }
      if(!is.null(bsn)){
        self$replace_value_where("persoon", col_replace = "gekoppeldbsn", val_replace = bsn,
                                 col_compare = "persoonid", val_compare = id)  
      }
      
      
    },
    
    #' @description Register an 'unknown' location
    #' @param locatieomschrijving Description of the location - mandatory
    #' @param wijk Optional
    #' @param buurt Optional
    #' @param postcode Optional
    #' @param straatnaam Optional
    #' @return The new ID of the location
    add_unknown_location = function(locatieomschrijving, 
                                    wijk = "", 
                                    buurt = "", 
                                    postcode = "", 
                                    straatnaam = "",
                                    huisnummer = "",
                                    huisnummertoevoeging = "",
                                    plaatsnaam = ""
    ){
      
      
      location_id <- uuid::UUIDgenerate()
      
      self$append_data("locatie", 
                       tibble(
                         locatieid = location_id,
                         locatieomschrijving = locatieomschrijving, 
                         wijk = wijk, 
                         buurt = buurt, 
                         postcode = postcode, 
                         straatnaam = straatnaam,
                         huisnummer = huisnummer,
                         huisnummertoevoeging = huisnummertoevoeging,
                         plaatsnaam = plaatsnaam
                       ))
      
      return(location_id)
    },
    
    delete_unknown_location = function(id){
      
      self$delete_rows_where("locatie", "locatieid", id)
      
    },
    
    get_unknown_location = function(id){
      
      self$read_table("locatie", lazy = TRUE) %>%
        filter(locatieid %in% !!id) %>%
        collect
      
    },
    
    
    #' @description Make an adres the hoofdadres of the dossier
    #' @param dossierid
    #' @param objectinstance
    set_adres_as_hoofdadres = function(dossierid, objectinstance){
      
      query <- glue("update {self$schema}.dossiers set hoofdadresid = '{objectinstance}' where ",
                    "dossierid = {dossierid}") %>% as.character()
      
      dbExecute(self$con, query)
      
    },
    

    
    #'@description Fill 'relation' and 'related_to' column for an object (typically, person) 
    #'in a dossier.
    #'@param dossierid
    #'@param objectinstance
    #'@param relation
    #'@param related_to
    set_object_relations = function(dossierid, objectinstance, relation, related_to){
      
      query <- glue("update {self$schema}.objecten set relation = '{relation}', related_to = '{related_to}' ",
                    "where dossierid = {dossierid} and objectinstance = '{objectinstance}'") %>% 
        as.character()
      
      dbExecute(self$con, query)
      
    },
    
    #' @description Get kenmerk column for an object in a dossier. 
    #' @param dossierid The dossier ID (integer)
    #' @param objectinstance The object ID instance.
    get_object_kenmerk = function(dossierid, objectinstance){
      
      self$read_table("objecten", lazy = TRUE) %>%
        filter(dossierid == !!dossierid, objectinstance == !!objectinstance) %>%
        collect %>%
        pull(kenmerk)
      
    },
    
    #'@description Fill 'kenmerk' column for an object in a dossier.
    #'@param dossierid
    #'@param objectinstance
    #'@param kenmerk Text field (no validation)
    set_object_kenmerk = function(dossierid, objectinstance, kenmerk){
      
      if(is.null(kenmerk))kenmerk <- " "
      query <- glue("update {self$schema}.objecten set kenmerk = '{kenmerk}' where ",
                    "dossierid = {dossierid} and objectinstance = '{objectinstance}'") %>% 
        as.character()
      
      dbExecute(self$con, query)
      
    },
    
    #' @description Get all objects in a dossier
    #' @param dossierid The dossier ID (integer)
    get_objects = function(dossierid, type = NULL){
      
      query <- self$read_table("objecten", lazy = TRUE) %>%
        filter(dossierid %in% !!dossierid)
      
      if(!is.null(type)){
        query <- filter(query, objecttype %in% !!type)
      }
      
      collect(query)
      
    },
    
    n_personen_dossier = function(dossierid){
      
      out <- self$read_table("objecten", lazy = TRUE) %>%
        filter(dossierid %in% !!dossierid,
               objecttype %in% c("bsn_persoon","onbekend_persoon")) %>%
        summarize(n()) %>%
        collect
      
      as.integer(out[[1]])
      
    },
    
    #' @description Save kenmerktypes for a dossier
    #' @param dossierid The dossier ID
    #' @param A vector of kenmerktypes
    add_dossier_kenmerken = function(dossierid, kenmerktypeid, userid){
      
      n_kenm <- length(kenmerktypeid)
      
      self$append_data("dossierkenmerk", tibble(
        
        dossierid = dossierid,
        kenmerktypeid = kenmerktypeid,
        creation_time = Sys.time(),
        userid = userid
      ))
      
      
    },
    
    #' @description Get dossier kenmerken for a dossierid
    get_dossier_kenmerken = function(dossierid){
      
      self$read_table("dossierkenmerk", lazy = TRUE) %>%
        filter(dossierid %in% !!dossierid) %>%
        collect
      
    },
    
    #' @description Replace kenmerken for a dossier
    replace_dossier_kenmerken = function(dossierid, kenmerktypeid, userid){
      
      kenm <- paste(kenmerktypeid, collapse =", ")
      self$log_audit(userid, dossierid, actie = glue("Kenmerken dossier toegewezen: {kenm} voor dossier: {dossierid}"))
      
      self$delete_rows_where("dossierkenmerk", "dossierid", dossierid)
      
      if(length(kenmerktypeid) > 0){
        self$add_dossier_kenmerken(dossierid, kenmerktypeid, userid)  
      }
      
      
    },
    
    
    #' @description Get dossierid's for given dossierkenmerken
    #' @param coordinatoren A vector of coordinatoren userid's.
    #' @return A vector of dossierid's
    get_dossierid_dossier_kenmerken = function(kenmerken){
      
      self$read_table("dossierkenmerk", lazy = TRUE) %>%
        filter(kenmerktypeid %in% !!kenmerken) %>%
        collect %>%
        pull(dossierid) %>%
        unique
      
    },
    
    
    #' @description Write coordinatoren for a dossier.
    add_coordinatoren = function(dossierid, userid){
      
      n_coor <- length(userid)
      
      self$append_data("coordinatoren", tibble(
        id = uuid::UUIDgenerate(n = n_coor),
        dossierid = dossierid,
        userid = userid
      ))
      
    },
    
    delete_dossier_coordinatoren = function(dossierid){
      
      self$delete_rows_where("coordinatoren",col_compare = "dossierid",val_compare = dossierid)
      
    },
    
    replace_dossier_coordinatoren = function(dossierid, userid){
      
      self$delete_dossier_coordinatoren(dossierid)
      self$add_coordinatoren(dossierid, userid)
      
    },
    
    #' @description Get coordinatoren for a dossierid
    get_coordinatoren = function(dossierid){
      
      self$read_table("coordinatoren", lazy = TRUE) %>%
        filter(dossierid %in% !!dossierid) %>%
        collect
      
    },
    
    #' @description Get dossierid's for given coordinatoren
    #' @param coordinatoren A vector of coordinatoren userid's.
    #' @return A vector of dossierid's
    get_dossierid_coordinatoren = function(coordinatoren){
      
      self$read_table("coordinatoren", lazy = TRUE) %>%
        filter(userid %in% !!coordinatoren) %>%
        collect %>%
        pull(dossierid) %>%
        unique
      
    },
    
    #' @description Encrypt a value using a secret
    #' @details A vectorized version of `safer::encrypt_string`
    #' @param x A vector of values to encrypt
    #' @param secret The secret to use, set options(linkit_secret = "abc")
    encrypt = function(x, secret = getOption("linkit_secret")){
    
    vapply(x, safer::encrypt_string, key = secret,
           USE.NAMES = FALSE, FUN.VALUE = character(1)
    )
    },
    
    #' @description Decrypt a value using a secret
    #' @details A vectorized version of `safer::decrypt_string`.
    #' Missing values and empty strings are returned unchanged.
    #' @param x A vector of values to decrypt
    #' @param secret The secret to use, set options(linkit_secret = "abc")
    decrypt = function(x, secret = getOption("linkit_secret")){
      
      ii <- which(!is.na(x) & stringr::str_trim(x) != "")
      jj <- setdiff(1:length(x),ii)
      
      d <- vapply(x[ii], 
                  safer::decrypt_string, 
                  key = secret,
                  USE.NAMES = FALSE, FUN.VALUE = character(1)
      )
      
      out <- vector("character", length = length(x))
      out[ii] <- d
      out[jj] <- x[jj]
      
      out
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


