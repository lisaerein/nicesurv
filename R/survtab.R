#' Lisa's Survival Summary Table
#'
#' This function creates a summary table from a survfit object including survival estimates with CIs, number of events, and number at risk.
#' It returns a dataframe and prints a table using kable or htmlTable.
#' @param sfit Survfit object (REQUIRED).
#' @param times Numeric vector of times for survival estimates (REQUIRED).
#' @param timelab Character label for time-points. Default = "Time".
#' @param groups Character vector for groups as listed in dataset. Default = NA (no groups).
#' @param grlabs Character vector for group names, must be in same order as groups. Default = NA (no groups or use group levels from dataset).
#' @param surv.dec Number of decimal places to report for survival estimates. Default = 2.
#' @param perc Logical indicator to report survival estimates as percentages. Default = FALSE.
#' @param color Character Hex color to use for htmlTable striping. Default = "#EEEEEE" (light grey).
#' @param printorig Logical indicator to print original summary of survfit object for checking purposes. Default = TRUE.
#' @param kable Logical. Indicator to use kable to display table. Default = TRUE.
#' @param htmlTable Logical. Indicator to use htmlTable package to display table instead of kable Default = FALSE.
#' @keywords summary survival table consulting Lisa
#' @import survival
#' @importFrom knitr kable
#' @importFrom htmlTable htmlTable
#' @export
survtab <- function(sfit,
                    times,
                    timelab = "Time",
                    groups = NA,
                    grlabs = NA,
                    surv.dec = 2,
                    perc = FALSE,
                    color = "#EEEEEE",
                    printorig = TRUE,
                    kable = TRUE,
                    htmlTable = FALSE){

    if (printorig) print(summary(sfit, times=times))
    stable <- summary(sfit, times=times)

    Time <- summary(sfit, times=times)$time

    form <- paste("%", surv.dec+2, ".", surv.dec, "f", sep="")
    if (perc) form <- paste("%", surv.dec+3, ".", surv.dec, "f", sep="")

    mult <- 1
    if (perc) mult <- 100

    symb <- ""
    if (perc) symb <- "%"

    Survival <- paste(sprintf(form, round(stable$surv*mult,surv.dec)), symb,
                      " [",
                      sprintf(form, round(stable$lower*mult,surv.dec)), symb,
                      ", ",
                      sprintf(form, round(stable$upper*mult,surv.dec)), symb,
                      "]", sep="")

    sdata <- data.frame(Time,
                        Survival,
                        stable$n.event,
                        stable$n.risk)

    for (i in 1:ncol(sdata)){
        sdata[,i] <- as.character(sdata[,i])
    }

    names(sdata) <- c("Time", "Survival", "N events", "N at risk")

    ### if time = 0 remove estimates for survival and events
    sdata[sdata[,"Time"] == paste("0", timelab),"Survival"] <- "---"
    sdata[sdata[,"Time"] == paste("0", timelab),"N events"] <- "---"

    ### check if sfit object has stratafication or not
    ### if stratified check group names and apply group labels if any
    grouped <- FALSE
    if (!is.null(sfit$strata)) grouped <- TRUE

    if (!grouped){
        if (htmlTable){
            print(htmlTable(sdata[,2:ncol(sdata)],
                        css.cell='padding-left: 2em; padding-right: 2em;',
                        rnames = Time,
                        rowlabel = timelab,
                        col.rgroup=c('none')))
        }
        if (kable){
            names(sdata)[1] <- timelab
            print(
                kable(x = sdata,
                      row.names = FALSE,
                      align = "lcrr")
            )
        }
    }

    if (grouped){
        stratavar <- strsplit(levels(stable$strata)[1], '=')[[1]][1]
        groups_eq <- NA

        # check group names (if not NA), remove group names if invalid
        if (!is.na(groups[1])) {
            groups_eq = paste(stratavar, groups, sep="=")
            if (sum(groups_eq %in% stable$strata) != nlevels(stable$strata)) groups <- grlabs <- NA
        }

        # apply group labels (if not NA)
        sdata$strata <- stable$strata
        sdata$order <- 1:nrow(sdata)
        if (is.na(groups[1])) groups <- groups_eq <- levels(stable$strata)
        if (is.na(grlabs[1])) grlabs <- unlist(lapply(groups_eq, function(x) strsplit(x, '=')[[1]][2]))

        sdata$strata <- factor(as.character(sdata$strata),
                               levels = groups_eq,
                               labels = grlabs)
        sdata <- sdata[order(sdata$strata, sdata$order),]

        rows <- table(sdata$strata)

        sdata <- sdata[,!(names(sdata) %in% c("strata", "order"))]

        if (htmlTable & kable) htmlTable <- FALSE

        if (htmlTable){
            print(htmlTable(sdata[,2:ncol(sdata)],
                            rowlabel = timelab,
                            rgroup = grlabs,
                            rnames = sdata$Time,
                            n.rgroup = rows,
                            col.rgroup=c("#EEEEEE", 'none'),
                            css.cell='padding-left: 5em; padding-right: 2em;'))
        }
        if (kable){

            sdata_k <- NULL
            for (r in 1:length(rows)){
                blank <- sdata[1,]
                blank <- NA
                # blank[1] <- grlabs[r]
                sdata_k <- rbind(sdata_k, blank, sdata[1:rows[r],])
                sdata <- sdata[rows[r] +1:nrow(sdata),]
            }
            sdata_k$Time[!is.na(sdata_k$Time)] <- paste("&nbsp; &nbsp; &nbsp;", sdata_k$Time[!is.na(sdata_k$Time)])
            sdata_k$Time[is.na(sdata_k$Time)] <- paste("<b>", grlabs, "<b/>", sep="")

            names(sdata_k)[1] <- timelab
            print(
                kable(x = sdata_k,
                      row.names = FALSE,
                      align = "lcrr")
            )
            sdata <- sdata_k
        }
    }

    return(sdata)
}

