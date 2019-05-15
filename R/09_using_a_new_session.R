

#' in_parralel tag to execute a call in another session
#'
#' by executing a call in another session we gain the opporunity of displaying
#' dynamically the status, break the action after a timeout, or run the code
#' asynchronously and keep going.
#'
#' Draft, ellipsis is not supported and might be easy to break.
#'
#' @param .wait wether we wait for the result
#' @param .path path where all temp files are stored
#' @export
#' @examples
#' u <- cars
#' v <- 3
#' fun <- function(y,m){
#'   Sys.sleep(5)
#'   head(y, m)
#' }
#' x <- in_parralel(.wait=FALSE)$fun(u,v)
#' x
#' y <- in_parralel$fun(u,v)
#' y
in_parralel <- tag(args= alist(.wait = TRUE, .path = getwd()),{
  CALL[[1]] <- quote(f)
  CALL[-1] <- lapply(names(CALL[-1]),as.symbol)
  objects <- c(f= f, lapply(ARGS,eval), CALL = CALL)
  input_file  <- file.path(.path, "in_a_new_session_input.RDdata")
  script_file  <- file.path(.path, "in_a_new_session_script.R")
  output_file <- file.path(.path, "in_a_new_session_output.RDS")
  if(file.exists(output_file)) unlink(output_file)
  on.exit(walk(list(input_file, script_file, output_file),
              ~if(file.exists(.)) unlink(.)))
  R <- file.path(R.home(), "bin","RScript.exe")
  save(list = names(objects), file = input_file, envir = as.environment(objects))
  writeLines(c(
    "load('in_a_new_session_input.RDdata')",
    "res <- eval(CALL)",
    "saveRDS(res, 'in_a_new_session_output.RDS')"),
    script_file)
  cmd <- sprintf('%s "%s"', R, script_file)
  cat(cmd)
  system(cmd, wait = FALSE)
  # here we can periodically look if the script is done running, and run a pb
  # meanwhile, or just display how lon it has been running ?
  if(.wait){
  t <- Sys.time()
  txt <- character(0)
  cat("\n")
  while(!file.exists(output_file)) {
    Sys.sleep(1)
    elapsed <- Sys.time()-t
    cat(strrep("\b",nchar(txt)+1), strrep(" ",nchar(txt)), strrep("\b",nchar(txt)+1))
    txt <- paste("~",round(elapsed,2), units(elapsed))
    cat(txt)
  }
  res <- readRDS(output_file)
  } else {
    res <- NULL
  }
  res
})
