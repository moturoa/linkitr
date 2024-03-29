% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinkItEngine.R
\name{LinkItEngine}
\alias{LinkItEngine}
\title{Business logic of the LinkIt application}
\description{
An R6 object with methods for use in the Shiny application `ede_linkt_app`, 
and `ede-izm_frontend`.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{LinkItEngine$new()}}
\item \href{#method-list_tables}{\code{LinkItEngine$list_tables()}}
\item \href{#method-close}{\code{LinkItEngine$close()}}
\item \href{#method-read_table}{\code{LinkItEngine$read_table()}}
\item \href{#method-append_data}{\code{LinkItEngine$append_data()}}
\item \href{#method-query}{\code{LinkItEngine$query()}}
\item \href{#method-execute_query}{\code{LinkItEngine$execute_query()}}
\item \href{#method-delete_rows_where}{\code{LinkItEngine$delete_rows_where()}}
\item \href{#method-delete_rows_where2}{\code{LinkItEngine$delete_rows_where2()}}
\item \href{#method-replace_value_where}{\code{LinkItEngine$replace_value_where()}}
\item \href{#method-get_con}{\code{LinkItEngine$get_con()}}
\item \href{#method-has_value}{\code{LinkItEngine$has_value()}}
\item \href{#method-make_choices}{\code{LinkItEngine$make_choices()}}
\item \href{#method-add_user}{\code{LinkItEngine$add_user()}}
\item \href{#method-username_from_userid}{\code{LinkItEngine$username_from_userid()}}
\item \href{#method-is_admin}{\code{LinkItEngine$is_admin()}}
\item \href{#method-get_favorites}{\code{LinkItEngine$get_favorites()}}
\item \href{#method-is_favorite}{\code{LinkItEngine$is_favorite()}}
\item \href{#method-format_dossierid}{\code{LinkItEngine$format_dossierid()}}
\item \href{#method-add_favorite}{\code{LinkItEngine$add_favorite()}}
\item \href{#method-delete_favorite}{\code{LinkItEngine$delete_favorite()}}
\item \href{#method-toggle_favorite}{\code{LinkItEngine$toggle_favorite()}}
\item \href{#method-laatste_login}{\code{LinkItEngine$laatste_login()}}
\item \href{#method-next_dossier_id}{\code{LinkItEngine$next_dossier_id()}}
\item \href{#method-add_dossier}{\code{LinkItEngine$add_dossier()}}
\item \href{#method-get_dossier}{\code{LinkItEngine$get_dossier()}}
\item \href{#method-get_my_active_dossiers}{\code{LinkItEngine$get_my_active_dossiers()}}
\item \href{#method-get_all_active_dossiers}{\code{LinkItEngine$get_all_active_dossiers()}}
\item \href{#method-get_verwijderde_dossiers}{\code{LinkItEngine$get_verwijderde_dossiers()}}
\item \href{#method-get_personen_active_dossiers}{\code{LinkItEngine$get_personen_active_dossiers()}}
\item \href{#method-dossier_naam_from_id}{\code{LinkItEngine$dossier_naam_from_id()}}
\item \href{#method-delete_dossier}{\code{LinkItEngine$delete_dossier()}}
\item \href{#method-verwijder_dossier}{\code{LinkItEngine$verwijder_dossier()}}
\item \href{#method-undo_verwijder_dossier}{\code{LinkItEngine$undo_verwijder_dossier()}}
\item \href{#method-dossier_exists}{\code{LinkItEngine$dossier_exists()}}
\item \href{#method-log_audit}{\code{LinkItEngine$log_audit()}}
\item \href{#method-get_audit}{\code{LinkItEngine$get_audit()}}
\item \href{#method-is_object_in_dossier}{\code{LinkItEngine$is_object_in_dossier()}}
\item \href{#method-add_object_to_dossier}{\code{LinkItEngine$add_object_to_dossier()}}
\item \href{#method-remove_object_from_dossier}{\code{LinkItEngine$remove_object_from_dossier()}}
\item \href{#method-add_persoon_kenmerken}{\code{LinkItEngine$add_persoon_kenmerken()}}
\item \href{#method-add_unknown_person}{\code{LinkItEngine$add_unknown_person()}}
\item \href{#method-get_persoon}{\code{LinkItEngine$get_persoon()}}
\item \href{#method-edit_unknown_persoon}{\code{LinkItEngine$edit_unknown_persoon()}}
\item \href{#method-add_unknown_location}{\code{LinkItEngine$add_unknown_location()}}
\item \href{#method-delete_unknown_location}{\code{LinkItEngine$delete_unknown_location()}}
\item \href{#method-get_unknown_location}{\code{LinkItEngine$get_unknown_location()}}
\item \href{#method-set_adres_as_hoofdadres}{\code{LinkItEngine$set_adres_as_hoofdadres()}}
\item \href{#method-set_object_relations}{\code{LinkItEngine$set_object_relations()}}
\item \href{#method-get_object_kenmerk}{\code{LinkItEngine$get_object_kenmerk()}}
\item \href{#method-set_object_kenmerk}{\code{LinkItEngine$set_object_kenmerk()}}
\item \href{#method-get_objects}{\code{LinkItEngine$get_objects()}}
\item \href{#method-add_dossier_kenmerken}{\code{LinkItEngine$add_dossier_kenmerken()}}
\item \href{#method-get_dossier_kenmerken}{\code{LinkItEngine$get_dossier_kenmerken()}}
\item \href{#method-replace_dossier_kenmerken}{\code{LinkItEngine$replace_dossier_kenmerken()}}
\item \href{#method-get_dossierid_dossier_kenmerken}{\code{LinkItEngine$get_dossierid_dossier_kenmerken()}}
\item \href{#method-add_coordinatoren}{\code{LinkItEngine$add_coordinatoren()}}
\item \href{#method-delete_dossier_coordinatoren}{\code{LinkItEngine$delete_dossier_coordinatoren()}}
\item \href{#method-replace_dossier_coordinatoren}{\code{LinkItEngine$replace_dossier_coordinatoren()}}
\item \href{#method-get_coordinatoren}{\code{LinkItEngine$get_coordinatoren()}}
\item \href{#method-get_dossierid_coordinatoren}{\code{LinkItEngine$get_dossierid_coordinatoren()}}
\item \href{#method-encrypt}{\code{LinkItEngine$encrypt()}}
\item \href{#method-decrypt}{\code{LinkItEngine$decrypt()}}
\item \href{#method-clone}{\code{LinkItEngine$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$new(gemeente, schema, pool, config_file = "conf/config.yml")}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-list_tables"></a>}}
\if{latex}{\out{\hypertarget{method-list_tables}{}}}
\subsection{Method \code{list_tables()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$list_tables()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-close"></a>}}
\if{latex}{\out{\hypertarget{method-close}{}}}
\subsection{Method \code{close()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$close()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-read_table"></a>}}
\if{latex}{\out{\hypertarget{method-read_table}{}}}
\subsection{Method \code{read_table()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$read_table(table, lazy = FALSE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-append_data"></a>}}
\if{latex}{\out{\hypertarget{method-append_data}{}}}
\subsection{Method \code{append_data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$append_data(table, data)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-query"></a>}}
\if{latex}{\out{\hypertarget{method-query}{}}}
\subsection{Method \code{query()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$query(txt, glue = TRUE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-execute_query"></a>}}
\if{latex}{\out{\hypertarget{method-execute_query}{}}}
\subsection{Method \code{execute_query()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$execute_query(txt, glue = TRUE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-delete_rows_where"></a>}}
\if{latex}{\out{\hypertarget{method-delete_rows_where}{}}}
\subsection{Method \code{delete_rows_where()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$delete_rows_where(table, col_compare, val_compare)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-delete_rows_where2"></a>}}
\if{latex}{\out{\hypertarget{method-delete_rows_where2}{}}}
\subsection{Method \code{delete_rows_where2()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$delete_rows_where2(
  table,
  col_compare,
  val_compare,
  col_compare2,
  val_compare2
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-replace_value_where"></a>}}
\if{latex}{\out{\hypertarget{method-replace_value_where}{}}}
\subsection{Method \code{replace_value_where()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$replace_value_where(
  table,
  col_replace,
  val_replace,
  col_compare,
  val_compare
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_con"></a>}}
\if{latex}{\out{\hypertarget{method-get_con}{}}}
\subsection{Method \code{get_con()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_con()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-has_value"></a>}}
\if{latex}{\out{\hypertarget{method-has_value}{}}}
\subsection{Method \code{has_value()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$has_value(table, column, value)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-make_choices"></a>}}
\if{latex}{\out{\hypertarget{method-make_choices}{}}}
\subsection{Method \code{make_choices()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$make_choices(values_from, names_from = values_from, data = NULL)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_user"></a>}}
\if{latex}{\out{\hypertarget{method-add_user}{}}}
\subsection{Method \code{add_user()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_user(userid, role = 1, username)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-username_from_userid"></a>}}
\if{latex}{\out{\hypertarget{method-username_from_userid}{}}}
\subsection{Method \code{username_from_userid()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$username_from_userid(userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-is_admin"></a>}}
\if{latex}{\out{\hypertarget{method-is_admin}{}}}
\subsection{Method \code{is_admin()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$is_admin(userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_favorites"></a>}}
\if{latex}{\out{\hypertarget{method-get_favorites}{}}}
\subsection{Method \code{get_favorites()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_favorites(userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-is_favorite"></a>}}
\if{latex}{\out{\hypertarget{method-is_favorite}{}}}
\subsection{Method \code{is_favorite()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$is_favorite(userid, dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-format_dossierid"></a>}}
\if{latex}{\out{\hypertarget{method-format_dossierid}{}}}
\subsection{Method \code{format_dossierid()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$format_dossierid(id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_favorite"></a>}}
\if{latex}{\out{\hypertarget{method-add_favorite}{}}}
\subsection{Method \code{add_favorite()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_favorite(userid, dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-delete_favorite"></a>}}
\if{latex}{\out{\hypertarget{method-delete_favorite}{}}}
\subsection{Method \code{delete_favorite()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$delete_favorite(userid, dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toggle_favorite"></a>}}
\if{latex}{\out{\hypertarget{method-toggle_favorite}{}}}
\subsection{Method \code{toggle_favorite()}}{
Toggle favorie dossier for a user
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$toggle_favorite(userid, dossierid)}\if{html}{\out{</div>}}
}

\subsection{Details}{
Delete the favorite if the dossier is a favorite, otherwise add it.
}

\subsection{Returns}{
TRUE if successful.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-laatste_login"></a>}}
\if{latex}{\out{\hypertarget{method-laatste_login}{}}}
\subsection{Method \code{laatste_login()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$laatste_login(userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-next_dossier_id"></a>}}
\if{latex}{\out{\hypertarget{method-next_dossier_id}{}}}
\subsection{Method \code{next_dossier_id()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$next_dossier_id()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-add_dossier}{}}}
\subsection{Method \code{add_dossier()}}{
Make a new dossier, write to DB
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_dossier(
  dossiername,
  userid,
  aanmaakdatum,
  verloopdatum,
  betrokkeninstanties,
  id = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dossiername}}{Name of the dossier (char)}

\item{\code{userid}}{Current user (char)}

\item{\code{aanmaakdatum}}{Creation date (posixct)}

\item{\code{verloopdatum}}{Expiry date (posixct)}

\item{\code{betrokkeninstanties}}{(char)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The (usually new) ID of the dossier.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-get_dossier}{}}}
\subsection{Method \code{get_dossier()}}{
Get a dossier based on its ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_dossier(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_my_active_dossiers"></a>}}
\if{latex}{\out{\hypertarget{method-get_my_active_dossiers}{}}}
\subsection{Method \code{get_my_active_dossiers()}}{
Retrieves dossiers for a user 'Actief' or 'Volgen' status for
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_my_active_dossiers(userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_all_active_dossiers"></a>}}
\if{latex}{\out{\hypertarget{method-get_all_active_dossiers}{}}}
\subsection{Method \code{get_all_active_dossiers()}}{
Find all dossiers with 'Actief' or 'Volgen' status
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_all_active_dossiers()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_verwijderde_dossiers"></a>}}
\if{latex}{\out{\hypertarget{method-get_verwijderde_dossiers}{}}}
\subsection{Method \code{get_verwijderde_dossiers()}}{
Find all dossiers with 'Verwijderd' status
Retrieve all pseudo-BSNs of all persons in active dossiers for the current user.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_verwijderde_dossiers()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_personen_active_dossiers"></a>}}
\if{latex}{\out{\hypertarget{method-get_personen_active_dossiers}{}}}
\subsection{Method \code{get_personen_active_dossiers()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_personen_active_dossiers(
  userid = NULL,
  method = c("current_user", "all")
)}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Vector of pseudo-BSNs
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-dossier_naam_from_id"></a>}}
\if{latex}{\out{\hypertarget{method-dossier_naam_from_id}{}}}
\subsection{Method \code{dossier_naam_from_id()}}{
Find the name of a dossier based on its ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$dossier_naam_from_id(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-delete_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-delete_dossier}{}}}
\subsection{Method \code{delete_dossier()}}{
Really delete a dossier.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$delete_dossier(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-verwijder_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-verwijder_dossier}{}}}
\subsection{Method \code{verwijder_dossier()}}{
Set status dossier to 'Verwijderd'
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$verwijder_dossier(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-undo_verwijder_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-undo_verwijder_dossier}{}}}
\subsection{Method \code{undo_verwijder_dossier()}}{
Undo verwijder_dossier
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$undo_verwijder_dossier(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-dossier_exists"></a>}}
\if{latex}{\out{\hypertarget{method-dossier_exists}{}}}
\subsection{Method \code{dossier_exists()}}{
Does the given Dossier ID actually exist?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$dossier_exists(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-log_audit"></a>}}
\if{latex}{\out{\hypertarget{method-log_audit}{}}}
\subsection{Method \code{log_audit()}}{
Log an audit action
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$log_audit(userid, dossierid, actie, pseudo_bsn = "")}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_audit"></a>}}
\if{latex}{\out{\hypertarget{method-get_audit}{}}}
\subsection{Method \code{get_audit()}}{
Get dossier kenmerken for a dossierid
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_audit(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-is_object_in_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-is_object_in_dossier}{}}}
\subsection{Method \code{is_object_in_dossier()}}{
Is this objectinstance a member of dossier?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$is_object_in_dossier(objectinstance, dossierid)}\if{html}{\out{</div>}}
}

\subsection{Returns}{
TRUE if object is in the dossier, FALSE otherwise.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_object_to_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-add_object_to_dossier}{}}}
\subsection{Method \code{add_object_to_dossier()}}{
Add person(s) (who has a BSN) to a dossier
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_object_to_dossier(
  ids,
  dossierid,
  userid,
  type = c("bsn_persoon", "onbekend_persoon", "bag_locatie", "onbekend_locatie"),
  dossierid_check = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ids}}{Vector of pseudo-BSNs, or persoon-IDs (in case of unknown persons)}

\item{\code{dossierid}}{Dossier ID (char)}

\item{\code{userid}}{The current user (for audit trail)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
TRUE if successful, FALSE if object already added to dossier.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-remove_object_from_dossier"></a>}}
\if{latex}{\out{\hypertarget{method-remove_object_from_dossier}{}}}
\subsection{Method \code{remove_object_from_dossier()}}{
Remove an object from a dossier
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$remove_object_from_dossier(ids, dossierid, userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_persoon_kenmerken"></a>}}
\if{latex}{\out{\hypertarget{method-add_persoon_kenmerken}{}}}
\subsection{Method \code{add_persoon_kenmerken()}}{
Add kenmerken to a persoon
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_persoon_kenmerken(
  dossierid,
  id,
  geboortedatum = "",
  geslacht = ""
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_unknown_person"></a>}}
\if{latex}{\out{\hypertarget{method-add_unknown_person}{}}}
\subsection{Method \code{add_unknown_person()}}{
Register a new 'unknown' person
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_unknown_person(
  name,
  adresseerbaarobject = NULL,
  locatieid = "",
  woonadresomschrijving = ""
)}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The new ID of the person
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_persoon"></a>}}
\if{latex}{\out{\hypertarget{method-get_persoon}{}}}
\subsection{Method \code{get_persoon()}}{
Get a person (an unknown person) by ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_persoon(id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-edit_unknown_persoon"></a>}}
\if{latex}{\out{\hypertarget{method-edit_unknown_persoon}{}}}
\subsection{Method \code{edit_unknown_persoon()}}{
Update name, BSN of an unknown person
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$edit_unknown_persoon(id, naam, bsn)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_unknown_location"></a>}}
\if{latex}{\out{\hypertarget{method-add_unknown_location}{}}}
\subsection{Method \code{add_unknown_location()}}{
Register an 'unknown' location
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_unknown_location(
  locatieomschrijving,
  wijk = "",
  buurt = "",
  postcode = "",
  straatnaam = "",
  huisnummer = "",
  huisnummertoevoeging = "",
  plaatsnaam = ""
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{locatieomschrijving}}{Description of the location - mandatory}

\item{\code{wijk}}{Optional}

\item{\code{buurt}}{Optional}

\item{\code{postcode}}{Optional}

\item{\code{straatnaam}}{Optional}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The new ID of the location
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-delete_unknown_location"></a>}}
\if{latex}{\out{\hypertarget{method-delete_unknown_location}{}}}
\subsection{Method \code{delete_unknown_location()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$delete_unknown_location(id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_unknown_location"></a>}}
\if{latex}{\out{\hypertarget{method-get_unknown_location}{}}}
\subsection{Method \code{get_unknown_location()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_unknown_location(id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_adres_as_hoofdadres"></a>}}
\if{latex}{\out{\hypertarget{method-set_adres_as_hoofdadres}{}}}
\subsection{Method \code{set_adres_as_hoofdadres()}}{
Make an adres the hoofdadres of the dossier
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$set_adres_as_hoofdadres(dossierid, objectinstance)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_object_relations"></a>}}
\if{latex}{\out{\hypertarget{method-set_object_relations}{}}}
\subsection{Method \code{set_object_relations()}}{
Fill 'relation' and 'related_to' column for an object (typically, person) 
in a dossier.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$set_object_relations(
  dossierid,
  objectinstance,
  relation,
  related_to
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_object_kenmerk"></a>}}
\if{latex}{\out{\hypertarget{method-get_object_kenmerk}{}}}
\subsection{Method \code{get_object_kenmerk()}}{
Get kenmerk column for an object in a dossier.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_object_kenmerk(dossierid, objectinstance)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dossierid}}{The dossier ID (integer)}

\item{\code{objectinstance}}{The object ID instance.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_object_kenmerk"></a>}}
\if{latex}{\out{\hypertarget{method-set_object_kenmerk}{}}}
\subsection{Method \code{set_object_kenmerk()}}{
Fill 'kenmerk' column for an object in a dossier.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$set_object_kenmerk(dossierid, objectinstance, kenmerk)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{kenmerk}}{Text field (no validation)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_objects"></a>}}
\if{latex}{\out{\hypertarget{method-get_objects}{}}}
\subsection{Method \code{get_objects()}}{
Get all objects in a dossier
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_objects(dossierid)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dossierid}}{The dossier ID (integer)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_dossier_kenmerken"></a>}}
\if{latex}{\out{\hypertarget{method-add_dossier_kenmerken}{}}}
\subsection{Method \code{add_dossier_kenmerken()}}{
Save kenmerktypes for a dossier
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_dossier_kenmerken(dossierid, kenmerktypeid, userid)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dossierid}}{The dossier ID}

\item{\code{A}}{vector of kenmerktypes}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_dossier_kenmerken"></a>}}
\if{latex}{\out{\hypertarget{method-get_dossier_kenmerken}{}}}
\subsection{Method \code{get_dossier_kenmerken()}}{
Get dossier kenmerken for a dossierid
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_dossier_kenmerken(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-replace_dossier_kenmerken"></a>}}
\if{latex}{\out{\hypertarget{method-replace_dossier_kenmerken}{}}}
\subsection{Method \code{replace_dossier_kenmerken()}}{
Replace kenmerken for a dossier
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$replace_dossier_kenmerken(dossierid, kenmerktypeid, userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_dossierid_dossier_kenmerken"></a>}}
\if{latex}{\out{\hypertarget{method-get_dossierid_dossier_kenmerken}{}}}
\subsection{Method \code{get_dossierid_dossier_kenmerken()}}{
Get dossierid's for given dossierkenmerken
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_dossierid_dossier_kenmerken(kenmerken)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{coordinatoren}}{A vector of coordinatoren userid's.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A vector of dossierid's
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_coordinatoren"></a>}}
\if{latex}{\out{\hypertarget{method-add_coordinatoren}{}}}
\subsection{Method \code{add_coordinatoren()}}{
Write coordinatoren for a dossier.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$add_coordinatoren(dossierid, userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-delete_dossier_coordinatoren"></a>}}
\if{latex}{\out{\hypertarget{method-delete_dossier_coordinatoren}{}}}
\subsection{Method \code{delete_dossier_coordinatoren()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$delete_dossier_coordinatoren(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-replace_dossier_coordinatoren"></a>}}
\if{latex}{\out{\hypertarget{method-replace_dossier_coordinatoren}{}}}
\subsection{Method \code{replace_dossier_coordinatoren()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$replace_dossier_coordinatoren(dossierid, userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_coordinatoren"></a>}}
\if{latex}{\out{\hypertarget{method-get_coordinatoren}{}}}
\subsection{Method \code{get_coordinatoren()}}{
Get coordinatoren for a dossierid
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_coordinatoren(dossierid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_dossierid_coordinatoren"></a>}}
\if{latex}{\out{\hypertarget{method-get_dossierid_coordinatoren}{}}}
\subsection{Method \code{get_dossierid_coordinatoren()}}{
Get dossierid's for given coordinatoren
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$get_dossierid_coordinatoren(coordinatoren)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{coordinatoren}}{A vector of coordinatoren userid's.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A vector of dossierid's
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-encrypt"></a>}}
\if{latex}{\out{\hypertarget{method-encrypt}{}}}
\subsection{Method \code{encrypt()}}{
Encrypt a value using a secret
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$encrypt(x, secret = getOption("linkit_secret"))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{A vector of values to encrypt}

\item{\code{secret}}{The secret to use, set options(linkit_secret = "abc")}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
A vectorized version of `safer::encrypt_string`
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-decrypt"></a>}}
\if{latex}{\out{\hypertarget{method-decrypt}{}}}
\subsection{Method \code{decrypt()}}{
Decrypt a value using a secret
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$decrypt(x, secret = getOption("linkit_secret"))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{A vector of values to decrypt}

\item{\code{secret}}{The secret to use, set options(linkit_secret = "abc")}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
A vectorized version of `safer::decrypt_string`.
Missing values and empty strings are returned unchanged.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LinkItEngine$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
