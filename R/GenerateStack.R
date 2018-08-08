#' GenerateStack
#' @param folder_clean a
#' @param folder_results a
#' @param f a
#' @param dateDataMinusOneWeek a
#' @param dateData a
#' @importFrom RAWmisc YearWeek
#' @importFrom RAWmisc YearN
#' @import data.table
#' @export GenerateStack
GenerateStack <- function(
  folder_clean=DashboardFolder("data_clean"),
  folder_results=DashboardFolder("results"),
  f,
  dateDataMinusOneWeek,
  dateData
  ){

  stack <- data.frame(fylke=c(0,normomo::CONFIG$VALID_FYLKE))

  stack$runName <- c(
      "Norway",
      sprintf("Fylke_%s",formatC(normomo::CONFIG$VALID_FYLKE,width=2,flag=0))
    )

  stack$data_clean_name <- paste0(
    folder_clean,c(
      sprintf("data_%s.RDS",f),
      sprintf("fylke_%s_%s.RDS",normomo::CONFIG$VALID_FYLKE,f)
    )
  )

  stack$plotGraphs <- c(TRUE,rep(FALSE,length(normomo::CONFIG$VALID_FYLKE)))
  stack$MOMOFolderInput <- folder_clean

  stack$MOMOFolderResults <- file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "MOMO")

  stack$MOMOFolderResultsData <- file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "Data",
    paste0(stack$runName,".RDS")
  )

  stack$MOMOFolderResultsGraphsOldStyle <- file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "Graphs_old_style"
  )

  stack$MOMOFolderResultsGraphsNewStyle <- file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "Graphs_new_style"
  )

  stack$MOMOYsum <- RAWmisc:::YearN(dateDataMinusOneWeek)
  stack$dateDataMinusOneWeek <- dateDataMinusOneWeek
  stack$dateData <- dateData

  tmp <- list(
    list(
      "0to4" =  "age >= 0 & age <=4",
      "5to14" = "age >= 5 & age <=14",
      "15to64" = "age >= 15 & age <=64",
      "65P" = "age >= 65 | is.na(age)",
      "Total" = "age >= 0 | is.na(age)"
    )
  )
  for(i in normomo::CONFIG$VALID_FYLKE){
    tmp[[sprintf("Fylke_%s",i)]] <- list(
      "Total" = "age >= 0 | is.na(age)"
    )
  }
  stack$MOMOgroups <- tmp

  tmp <- list(
    c(
      "0to4" = "LINE",
      "5to14" = "LINE",
      "15to64" = "LINE_SIN",
      "65P" = "LINE_SIN",
      "Total" = "LINE_SIN"
    )
  )
  for(i in normomo::CONFIG$VALID_FYLKE){
    tmp[[sprintf("Fylke_%s",i)]] <- c(
      "Total" = "LINE_SIN"
    )
  }
  stack$MOMOmodels <- tmp

  return(stack)
}
