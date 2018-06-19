#' Old Namespaces
# "old_ns"
# old_ns <- NULL

#' previous Namespaces
#' @export
nsnap <-
  function(){
    # assign("old_ns", search(), envir = parent.env(environment()))
    old_ns <- paste0(search(), collapse = ",")
    Sys.setenv("NSNAP_OLD_NS" = old_ns)
  }

#' Remove new Namespaces
#' @export
nsnap_recover <-
  function(){
    new_ns <- search()
    old_ns <- Sys.getenv("NSNAP_OLD_NS")
    if(old_ns == ""){
      stop("No NSNAP_OLD_NS.")
    }else{
      old_ns <- strsplit(old_ns, ",")[[1]]
      willbe_removed <- new_ns[!(new_ns %in% old_ns)]
      if(length(willbe_removed) == 0){
        message("No packages will be detached.")
      }else{
        message("Following packages will be detached.")
        message(paste0(willbe_removed, collapse = " "))
        for(i in willbe_removed) detach(name = i, character.only = T)
      }
    }
  }

#' Show NSNAP_OLD_NS environment variables
#' @export
nsnap_show <-
  function(){
    Sys.getenv("NSNAP_OLD_NS")
  }

#' Clear NSNAP_OLD_NS environment variables
#' @export
nsnap_clean <-
  function(){
    Sys.unsetenv("NSNAP_OLD_NS")
  }
