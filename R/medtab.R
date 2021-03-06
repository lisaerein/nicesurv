#' Lisa's Median Survival Table
#'
#' This function creates a summary table from a survfit object including median survival estimates with CIs.
#' It returns a dataframe and prints a table using kable or htmlTable.
#' @param sfit Survfit object (REQUIRED).
#' @param groups Character vector for groups as listed in dataset. Default = NA (no groups).
#' @param grlabs Character vector for group names, must be in same order as groups. Default = NA (no groups or use group levels from dataset).
#' @param grcolname Character label for group column. Default = "Group".
#' @param med.dec Number of decimal places to report for median survival time. Default = 2.
#' @param medlab Character label for median and CI columns. Default = "Median survival time".
#' @param printorig Logical indicator to print original summary of survfit object for checking purposes. Default = TRUE.
#' @param kable Logical. Indicator to use kable to display table. Default = TRUE.
#' @param htmlTable Logical. Indicator to use htmlTable package to display table instead of kable Default = FALSE.
#' @keywords summary survival table consulting Lisa
#' @import survival
#' @importFrom knitr kable
#' @importFrom htmlTable htmlTable
#' @export
medtab <- function(sfit,
                   groups = NA,
                   grlabs = NA,
                   grcolname = "Group",
                   med.dec = 2,
                   medlab = "Median survival time",
                   printorig = TRUE,
                   kable = TRUE,
                   htmlTable = FALSE){

    if (printorig) print(data.frame(summary(sfit)$table))

    ### check if sfit object has stratafication or not
    ### if stratified check group names and apply group labels if any
    grouped <- FALSE
    if (!is.null(sfit$strata)) grouped <- TRUE

    ## numeric format
    form <- paste("%.", med.dec, "f", sep="")

    if (!grouped){

      tab <- data.frame(t(summary(sfit)$table))

      names(tab) <- tolower(names(tab))
      names(tab) <- gsub("x", "", names(tab))

      tab[1,"Median"] <- sprintf(form, round(tab[1,"median"], med.dec))
      tab[1,"95% CI"] <- paste("[",
                              sprintf(form, round(tab[1,"0.95lcl"], med.dec)),
                              ", ",
                              sprintf(form, round(tab[1,"0.95ucl"], med.dec)),
                              "]", sep="")

      if (htmlTable){
        print(htmlTable(tab[1,c("Median", "95% CI")],
                        rnames = FALSE,
                        cgroup = medlab,
                        n.cgroup = 2,
                        css.cell='padding-left: 1em; padding-right: 1em;'))
      }
      if (kable){
        print(
          kable(x = tab[1,c("Median", "95% CI")],
                row.names = FALSE,
                align = "cc")
        )
      }
    }

    if (grouped){

        tab <- data.frame(summary(sfit)$table)

        names(tab) <- tolower(names(tab))
        names(tab) <- gsub("x", "", names(tab))

        tab[,"Median"] <- sprintf(form, round(tab[,"median"], med.dec))
        tab[,"95% CI"] <- paste("[",
                                sprintf(form, round(tab[,"0.95lcl"], med.dec)),
                                ", ",
                                sprintf(form, round(tab[,"0.95ucl"], med.dec)),
                                "]", sep="")

        stratavar <- strsplit(names(sfit$strata)[1], '=')[[1]][1]
        groups_eq <- NA

        # check group names (if not NA), remove group names if invalid
        if (!is.na(groups[1])) {
            groups_eq = paste(stratavar, groups, sep="=")
            if (sum(groups_eq %in% names(sfit$strata)) != length(names((sfit$strata)))) groups <- grlabs <- NA
        }

        # apply group labels (if not NA)
        tab$strata <- rownames(tab)
        if (is.na(groups[1])) groups <- groups_eq <- names(sfit$strata)
        if (is.na(grlabs[1])) grlabs <- unlist(lapply(groups_eq, function(x) strsplit(x, '=')[[1]][2]))

        tab$strata <- factor(as.character(tab$strata),
                               levels = groups_eq,
                               labels = grlabs)
        tab <- tab[order(tab$strata),]

        if (htmlTable){
          print(htmlTable(tab[, c("Median", "95% CI")],
                          rnames = tab$strata,
                          rowlabel = grcolname,
                          n.rgroup = rep(1, nrow(tab)),
                          cgroup = medlab,
                          n.cgroup = 2,
                          css.cell='padding-left: 1em; padding-right: 1em;'))
        }
        if (kable){
          tab[,grcolname] <- tab$strata
          print(
            kable(x = tab[, c(grcolname, "Median", "95% CI")],
                  row.names = FALSE,
                  align = "lcc")
          )
        }
    }

}
