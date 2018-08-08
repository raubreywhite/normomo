GraphTogether <- function(data,norwegian=TRUE,title1=NULL,title1a=NULL,title1b=NULL,title2,includeRealDeaths=FALSE){
  if(!is.null(title1)){
    plottingData1 <- data[wk>=max(wk)-52]
    plottingData2 <- data[wk>=max(wk)-52*5+1]

    plottingData1[,titlex:=title1]
    plottingData2[,titlex:=title2]

    plottingData1[,type:="top"]
    plottingData2[,type:="bottom"]

    plottingData <- rbind(plottingData1,plottingData2)
    plottingData[,titlex:=factor(titlex,levels=c(title1,title2))]
  } else {
    plottingData1a <- data[wk>=max(wk)-52]
    plottingData1b <- data[wk>=max(wk)-52]
    plottingData2 <- data[wk>=max(wk)-52*5+1]

    plottingData1a[,titlex:=title1a]
    plottingData1b[,titlex:=title1b]
    plottingData2[,titlex:=title2]

    plottingData1a[,type:="top"]
    plottingData1b[,type:="top"]
    plottingData2[,type:="bottom"]

    plottingData <- rbind(plottingData1a,plottingData1b,plottingData2)
    plottingData[,titlex:=factor(titlex,levels=c(title1a,title1b,title2))]
  }

  plottingData[,ymax := max(nbc,UPIb4)]
  plottingData[,ymin := min(nbc,UPIb4)]
  plottingData[,Lower:=Pnb-abs(UPIb2-Pnb)]
  plottingData[Lower<0, Lower := 0]
  plottingData[,unstableEstimates:="Stable"]
  plottingData[wk>=max(wk)-7,unstableEstimates:="Unstable"]

  plottingData[,wkSplit:=wk]
  plottingData[type=="bottom",wkSplit:=wk*10]

  breaks <- unique(plottingData[,c("WoDi","YoDi","wk"),with=FALSE])
  breaksTop <- breaks[seq(1,53,4)]
  breaksTop[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

  breaks <- unique(plottingData[,c("wk","YoDi"),with=FALSE])
  setorder(breaks,wk)
  breaks[,YoDi2:=shift(YoDi)]
  breaksBottom <- na.omit(breaks[breaks$YoDi!=breaks$YoDi2,])
  breaksBottom$label <- paste("1/",breaksBottom$YoDi,sep="")
  breaksBottom[,wk:=wk*10]

  breaks <- rbind(breaksTop[,c("wk","label")],breaksBottom[,c("wk","label")])

  if(norwegian){
    filllabels1=c("Prediksjonsintervall","Betydelig h\u00F8yere enn forventet","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
    filllabels2=c("Prediksjonsintervall","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
    shapelabels=c("Forel\u00F8pig")
    colourlabels=c("Korrigert for forsinkelse","Rapporterte d\u00F8dsfall")
    ylabel="Antall d\u00F8de per uke"
  } else {
    filllabels1=c("Prediction interval","Significantly higher than expected","Higher than expected","Expected","Lower than expected")
    filllabels2=c("Prediction interval","Higher than expected","Expected","Lower than expected")
    shapelabels=c("Preliminary numbers")
    colourlabels=c("Corrected for delays","Reported deaths")
    ylabel="Deaths per week"
  }

  q <- ggplot(plottingData, aes(x=wkSplit))
  q <- q + geom_ribbon(aes(ymin=-Inf,ymax=Lower, fill="4lower"),alpha=0.7)
  q <- q + geom_ribbon(aes(ymin=Lower,ymax=UPIb2, fill="3expected"),alpha=0.7)
  q <- q + geom_ribbon(aes(ymin=UPIb2,ymax=UPIb4, fill="2high"),alpha=0.7)
  q <- q + geom_ribbon(aes(ymin=UPIb4,ymax=Inf, fill="1veryhigh"),alpha=0.7)
  q <- q + geom_ribbon(data=plottingData[type=="top"],mapping=aes(ymin=LPIc,ymax=UPIc, fill="0predinterval"),alpha=0.3)
  if(includeRealDeaths) q <- q + geom_line(data=plottingData[titlex %in% c(title1,title1a)],mapping=aes(y=nb,colour="Rapporterte"),lwd=0.5)
  q <- q + geom_line(aes(y=nbc,colour="Korrigert"),lwd=0.5)
  q <- q + geom_point(data=plottingData[unstableEstimates=="Unstable"],aes(y=nbc,shape="Usikkert"),size=2)
  q <- q + facet_wrap(~titlex,scales="free",ncol=1)
  #q <- q + labs(title=title)
  q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
  q <- q + scale_y_continuous(ylabel)
  q <- q + scale_fill_manual("",
                             values=c("0predinterval"="#636363","1veryhigh"="#fc8d59","2high"="#ffffbf","3expected"="#91bfdb","4lower"="white"),
                             labels=filllabels1)
  q <- q + scale_shape_manual("",
                              values=c("Usikkert"=16),
                              labels=shapelabels)
  q <- q + scale_colour_manual("",
                               values=c("Korrigert"="black","Rapporterte"="red"),
                               labels=colourlabels)
  q <- q + theme_gray(base_size=18)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  q <- q + guides(fill = guide_legend(title.position="top", reverse=F, order=1, ncol=1))
  q <- q + guides(colour = guide_legend(title.position="top", reverse=F, order=2, ncol=1))
  q <- q + guides(shape = guide_legend(title.position="top", reverse=F, order=3, ncol=1))

  if(!is.null(title1)){
    q <- q + theme(legend.position = "right")
  } else {
    q <- q + theme(legend.position = "bottom")
  }
  #q <- SMAOFormatGGPlot(q, legendPos="right", xAngle=90,ncol=1,legendBorder=TRUE)
  #q <- format_plot(q,2,2,stripes=TRUE, xangle=90)
  return(q)
}

#' Running graphs
#' @param runName Name
#' @param data data
#' @param folder a
#' @param yearWeek a
#' @param dateData a
#' @import stringr
#' @importFrom RAWmisc FootnoteGridArrange
#' @importFrom fhi SMAOpng
#' @import data.table
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.off
#' @export RunGraphs
RunGraphs <- function(runName="Norway",
                               data,
                               folder=fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(),"Graphs")),
                               yearWeek=RAWmisc::YearWeek(),
                               dateData=Sys.time()){

  #### NORWEGIAN

  storedData <- list()
  if(runName=="Norway"){
    runList <- c("Total","0to4","5to14","15to64","65P")
  } else runList <- "Total"
  for(i in runList){
    if(i=="Total"){
      title1 <- "Totalt antall d\u00F8de per uke siste \u00E5r"
      title1a <- "Totalt antall d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Totalt antall d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Totalt antall d\u00F8de per uke siste 5 \u00E5r"
    } else if(i=="0to4"){
      title1 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r"
      title1a <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste 5 \u00E5r"
    } else if(i=="5to14"){
      title1 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
    } else if(i=="15to64"){
      title1 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
    } else if(i=="65P"){
      title1 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
    }

    q <- gridExtra::grid.arrange(
      GraphTogether(data=data[GROUP==i & !is.na(excess)],
                  title1=title1,
                  title2=title2,
                  includeRealDeaths=FALSE
                  ),
      ncol=1,
      newpage=F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
      )
    RAWmisc::saveA4(q,filename=paste0(folder,"/excl_reported_",runName,"-",i,"-", yearWeek,".png"))

    q <- gridExtra::grid.arrange(
      GraphTogether(data=data[GROUP==i & !is.na(excess)],
                    title1=title1,
                    title2=title2,
                    includeRealDeaths=TRUE
      ),
      ncol=1,
      newpage=F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
    )
    RAWmisc::saveA4(q,filename=paste0(folder,"/incl_reported_",runName,"-",i,"-", yearWeek,".png"))


    q <- gridExtra::grid.arrange(
      GraphTogether(data=data[GROUP==i & !is.na(excess)],
                    title1a=title1a,
                    title1b=title1b,
                    title2=title2,
                    includeRealDeaths=TRUE
      ),
      ncol=1,
      newpage=F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
    )
    RAWmisc::saveA4(q,filename=paste0(folder,"/both_reported_",runName,"-",i,"-", yearWeek,".png"),landscape=F)


  }


}
