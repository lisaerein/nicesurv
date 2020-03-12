#' Lisa's Cumulative Incidence Survival Table
#'
#' This function creates a summary table from a survfit object of type "mstate" including cumulative incidence estimates with CIs.
#' It returns a dataframe and prints a table using kable or htmlTable.
#' @param msfit survfit object of type "mstate" (REQUIRED).
#' @param times Numeric vector of times for cumulative incidence estimates (REQUIRED).
#' @param timelab Character label for time. Default = "Time".
#' @param events Character vector for event names (including state 0).
#' @param evlabs Character vector for event labels.
#' @param groups Character vector for groups as listed in dataset. Default = NA (no groups).
#' @param grlabs Character vector for group names, must be in same order as groups. Default = NA (no groups or use group levels from dataset).
#' @param grcolname Character label for group column. Default = "Group".
#' @param state0 Character label for state 0. Default = NA (do not display estimates for state 0).
#' @param ci.dec Number of decimal places to report for cumlative incidence estimates. Default = 2.
#' @param perc Logical indicator to report estimates as percentages. Default = FALSE.
#' @param color Character Hex color to use for htmlTable striping. Default = "#EEEEEE" (light grey).
#' @param kable Logical. Indicator to use kable to display table. Default = TRUE.
#' @param htmlTable Logical. Indicator to use htmlTable package to display table instead of kable Default = FALSE.
#' @param printorig Logical indicator to print original summary of ms-survfit object for checking purposes. Default = TRUE.
#' @keywords summary survival table consulting Lisa cumulative incidence mstate
#' @import survival
#' @importFrom knitr kable
#' @importFrom htmlTable htmlTable
#' @export
citab   <- function(msfit,
                    times,
                    timelab = "Time",
                    events = NA,
                    evlabs = NA,
                    groups = NA,
                    grlabs = NA,
                    grcolname = "Group",
                    state0 = NA,
                    ci.dec = 2,
                    perc = FALSE,
                    color = "#EEEEEE",
                    kable = TRUE,
                    htmlTable = FALSE,
                    printorig = TRUE){

    if (printorig) print(summary(msfit, times = times))

    est <- summary(msfit, times = times)

    probs <- data.frame(est$pstate)
    lower <- data.frame(est$lower)
    upper <- data.frame(est$upper)

    lower[probs == 0 | probs == 1] <- NA
    upper[probs == 0 | probs == 1] <- NA

    if (perc){
        probs <- 100*probs
        lower <- 100*lower
        upper <- 100*upper
    }

    format <- paste("%.", ci.dec, "f", sep="")

    ci <- matrix(rep(NA, nrow(probs)*ncol(probs)), nrow = nrow(probs))

    for (i in 1:length(est$states)){
        ci[,i] <- paste(sprintf(format, round(probs[,i], ci.dec)),
                        " [",
                        sprintf(format, round(lower[,i], ci.dec)),
                        ", ",
                        sprintf(format, round(upper[,i], ci.dec)),
                        "]", sep="")
        ci[,i] <- gsub("NA|NaN", "---", ci[,i])
    }
    ci <- data.frame(ci)
    names(ci) <- c(est$states[1:(length(est$states)-1)], ifelse(!is.na(state0), state0, "state0"))

    ### check if msfit object has stratafication or not
    ### if stratified check group names and apply group labels if any
    grouped <- FALSE
    if (!is.null(msfit$strata)) grouped <- TRUE

    ### relabel/reorder events?
    evnts <- names(ci)
    if (!is.na(events[1])) {
        if (sum(events %in% evnts) == 0) events <- evnts <- NA
    }
    if (is.na(events[1])) events <- evnts
    if (is.na(evlabs[1])) evlabs <- events

    if (!grouped){
        res <- data.frame("time" = c(est$time), rbind(ci))
        # names(res)[1] <- timelab

        res <- res[,c("time", events)]

        if (htmlTable){

            restab <- res[,2:ncol(res), drop = F]
            names(restab) <- evlabs

                print(htmlTable(restab,
                                n.cgroup = ncol(restab),
                                cgroup = "Cumulative incidence [95% CI]",
                                header = names(restab),
                                rgroup = timelab,
                                n.rgroup = nrow(res),
                                css.cell='padding-left: 5em; padding-right: 2em;',
                                rnames = times,
                                col.rgroup=c('none')))
        }
        if (kable){
            resdat <- res
            names(resdat) <- c(timelab, evlabs)
            print(
                kable(x = resdat
                      ,row.names = FALSE
                      ,align = paste("l", rep("c", ncol(resdat)-1), sep="")
                      ,caption = "Cumulative incidence [95% CI]"
                )
            )
        }
    }

    if (grouped){

        stratavar <- strsplit(levels(est$strata)[1], '=')[[1]][1]
        groups_eq <- NA

        # check group names (if not NA), remove group names if invalid
        if (!is.na(groups[1])) {
            groups_eq = paste(stratavar, groups, sep="=")
            if (sum(groups_eq %in% est$strata) != nlevels(est$strata)) groups <- grlabs <- NA
        }

        res <- data.frame("strata" = as.character(est$strata),
                          "time" = est$time,
                          rbind(ci),
                          stringsAsFactors = FALSE)

        # apply group labels (if not NA)
        if (is.na(groups[1])) groups <- groups_eq <- levels(est$strata)
        if (is.na(grlabs[1])) grlabs <- unlist(lapply(groups_eq, function(x) strsplit(x, '=')[[1]][2]))

        res$strata <- factor(as.character(res$strata),
                             levels = groups_eq,
                             labels = grlabs)
        res <- res[order(res$strata),]

        res <- res[,c("time", "strata", events)]

        rows <- table(res$strata)

        if (htmlTable){
            restab <- res[,3:ncol(res), drop = F]
            names(restab) <- evlabs

            print(htmlTable(restab,
                            n.cgroup = ncol(restab),
                            cgroup = "Cumulative incidence [95% CI]",
                            header = names(restab),
                            rowlabel = timelab,
                            rgroup = grlabs,
                            n.rgroup = rows,
                            rnames = res$time,
                            col.rgroup=c("#EEEEEE", 'none'),
                            css.cell='padding-left: 5em; padding-right: 2em;'
                            )
                  )
        }

        if (kable){
            timecol <- res$time
            stratcol <- as.character(res$strata)
            restab <- res[,3:ncol(res)]
            stratcol[timecol!= min(times)] <- NA
            resdat <- data.frame(stratcol, timecol, restab)
            names(resdat) <- c(grcolname, timelab, evlabs)
            print(
                kable(x = resdat
                      ,row.names = FALSE
                      ,align = paste("lr", rep("c", ncol(resdat)-2), sep="")
                      ,caption = "Cumulative incidence [95% CI]"
                      )
            )
        }

    }
    return(res)
}

