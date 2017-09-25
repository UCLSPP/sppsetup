#----------------------------------------------------------------
send_report <- function(dsn, logfile, user, exception) {

  sys_info <- Sys.info()
  sys_user <- paste(sys_info[c("user", "nodename")], collapse = "@")

  if (!nchar(user$name))
    user$name <- sys_user

  exception <- list(type = exception$type,
                    value = paste("[", sys_user, "]", sep = ""),
                    message = exception$message)

  dsn <- "https://29845a7c0f4b4ebf929a1cbcac3be9a4:b8ed650559d84a96845361bbddd4b6dd@sentry.io/219443"
  client <- ravenr::sentry_client(dsn = dsn, user = user)

  extra <- c()
  if (file.exists(logfile))
    extra$errorlog = paste(readLines(logfile), collapse = "\n")

  response <- ravenr::capture_exception(client, exception, extra)
  httr::message_for_status(response)
  cat("\n")

  return(response)
}

