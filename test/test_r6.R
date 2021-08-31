


# Test het system warehouse object.

library(microbenchmark)
library(tinytest)
library(here)

options(config_file = "c:/repos/ede/ede_linkit_app/conf/config.yml")

source(file.path(here(), "preload/load_packages.R"))
source(file.path(here(), "R/SystemWarehouseR6.R"))

library(linkitr)

.sys <- LinkItEngine$new(gemeente = "Ede", 
                            schema = "linkit", 
                            config_file = getOption("config_file"),
                            pool = TRUE)


# .sys$add_user("gert.jan.kloppenburg", 1, "Gert-Jan Kloppenburg")
# .sys$add_user("frank.keeris", 1, "Frank Keeris")
# .sys$add_user("sanae.smeets", 1, "Sanae Smeets")
# .sys$add_user("timo.otten", 1, "Timo Otten")


# $is_favorite
expect_true(.sys$is_favorite("martijnheijstek", "006"))
expect_false(.sys$is_favorite("martijnheijstek", "006x4980347"))



# $add_favorite
expect_true(
  .sys$add_favorite("testuser","testdossier")
)


expect_true(
  .sys$delete_favorite("testuser","testdossier")
)

expect_false(
  .sys$is_favorite2("testuser", "testdossier")
)


# $user
.sys$username_from_userid("remkoduursma")



# $add_dossier
expect_inherits(
  new_id <- .sys$add_dossier(
    dossiername = "remko test", 
    userid = "remkoduursma", 
    aanmaakdatum = Sys.time(), 
    verloopdatum = Sys.time() + weeks(6), 
    betrokkeninstanties = "Allegro"
  ),
  "character"
)


# $delete_dossier
expect_true(
  .sys$delete_dossier(new_id)
)

# $log_audit
expect_true(
  .sys$log_audit(
    userid = "remkoduursma",
    dossierid = "testid",
    actie = "auditing getest"
  )
)

# cleanup
.sys$delete_rows_where("audittrail", "dossierid", "testid")


# $add_person_to_dossier
expect_true(
  .sys$add_object_to_dossier(ids = "testid123", 
                             dossierid = "othertestid", 
                             userid = current_user,
                             type = "bsn_persoon", 
                             dossierid_check = FALSE)  
)

expect_warning(
  .sys$add_persons_to_dossier(ids = "testid123", dossierid = "thisiddoesnotexist")  
)


# cleanup
.sys$remove_object_from_dossier("testid123", dossierid = "othertestid",userid = "remkoduursma")




# $add_unknown_person
if(FALSE){
  # voorlopig handmatig testen
  
  
  locatie_id <- .sys$add_unknown_location("Onder de grote brug", wijk = "'t Hofke")
  
  .sys$add_unknown_person("Meneer onder de Brug", locatieid = locatie_id)
  
  .sys$read_table("persoon")
  
}





# $add_dossier_kenmerken
expect_true(
  .sys$add_dossier_kenmerken(dossierid = "XXX", 
                             kenmerktypeid = c("fraude","drugsprobleem"), 
                             userid = "remkoduursma")
)


.sys$replace_dossier_kenmerken("27c64072-0641-4d1d-9cc3-14f9f8ff8311", 
                               c("crimineel","fraude"), "remkoduursma")






# cleanup
.sys$delete_rows_where("dossierkenmerk", "dossierid", "XXX")


# $add_coordinatoren
expect_true(
  .sys$add_coordinatoren(dossierid = "XXX", 
                         userid = c("peter","remkoduursma"))
)




# cleanup
.sys$delete_rows_where("coordinatoren", "dossierid", "XXX")



# $get_dossier
.sys$get_dossier(c("005","001"))

# $get_dossier_coordinatoren
.sys$get_dossierid_coordinatoren("remkoduursma")




# $get_notificaties
if(FALSE){
  # some fake notifications
  .sys$append_data("notificaties", 
                   tibble(
                     id = uuid::UUIDgenerate(n = 3),
                     timestamp = as.character(rep(Sys.time(),3)),
                     userid = c("izmuser1","izmuser2","izmuser1"),
                     pseudo_bsn = c("v5nmIjwS1","iWaq6cVRE","3Dzf0BUdY"), # dossiers 15 en 10
                     removed = 0,
                     comment = ""
                   ))
}


.sys$get_notificaties("remkoduursma")


# object kenmerko$
.sys$set_object_kenmerk(dossierid = "002", objectinstance = "21itZVlNE", kenmerk = "test")










