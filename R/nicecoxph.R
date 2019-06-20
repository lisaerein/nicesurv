#' Lisa's Cox Proportional Hazards Regression Table Reporting Function
#'
#' This function creates a nice looking table of regression results for a coxph model.
#' Input either a coxph object or dataframe, event variable, time variable, vector of covariates, and type of analysis (univariate or multiple regression).
#' The function returns a dataframe of formatted regression results and prints the results table using htmlTable.
#' @param fit coxph model object [fit or df/event/ttevent/covs are REQUIRED].
#' @param df Dataframe [fit or df/event/ttevent/covs are REQUIRED].
#' @param covs Character. Vector of covariates to include in model [fit or df/event/ttevent/covs are REQUIRED].
#' @param ttevent Character. Name of time to event variable [fit or df/event/ttevent/covs are REQUIRED].
#' @param event Character. Name of event variable [fit or df/event/ttevent/covs are REQUIRED].
#' @param ttentry Character. Name of entry time variable for left truncated data. Default is NA (no delayed entry).
#' @param weightvar Character. Name of weighting variable. Default is NA (no weights).
#' @param cluster Character. Name of cluster variable. Default is NA (no clustering).
#' @param strata Character. Vector of names of strata variable(s). Default is NA (no strata).
#' @param regtype Logical. Should the covariates be run separately ("uni") or together in a multiple regression model ("multi") [REQUIRED if no fit].
#' @param type2 Logical. If TRUE, type II p-values will be added to the table. Default is FALSE.
#' @param type3 Logical, If TRUE, type III p-values will be added to the table. Default is FALSE.
#' @param phtest Logical. If TRUE, a test for proportional hazards (via cox.zph) will be included in the footnote. Default is FALSE.
#' @param labels Character. Vector of labels for covariates. Defauly is NA (use variable names).
#' @param estname Character. Label for estimate. Default = NA: "HR" for univariate and "aHR" for multiple regression.
#' @param refcat Logical. If TRUE the table will create a separate line for the reference category. Default is FALSE.
#' @param est.dec Numeric. Number of decimal places for estimates. Default is 2.
#' @param ci.dec Numeric. Number of decimal places for confidence interval. Default is 2.
#' @param pval.dec Numeric. Number of decimal places for pvalues. Must be an integer from 1 to 4. Default is 3.
#' @param color Character. Color to use for htmlTable striping. Default is "#EEEEEE" (light grey). Use "white" for no striping.
#' @param htmlTable Logical. If TRUE, the table will be optimized for html output. Default is TRUE.
#' @param title Character. Optional title above table. Default is "".
#' @param footer Logical. If TRUE, table will include a footnote with model details, nobs, R2. Default is TRUE.
#' @keywords coxph survival table cox proportional hazards regression reporting
#' @importFrom htmlTable htmlTable
#' @importFrom survival coxph cox.zph
#' @importFrom car Anova
#' @export
nicecoxph <- function(fit = NA,
                      df = NA,
                      covs = NA,
                      ttevent = "ttevent",
                      event = "event",

                      ttentry= NA,
                      weightvar = NA,
                      cluster = NA,
                      strata = NA,

                      regtype = "uni",
                      type2 = FALSE,
                      type3 = FALSE,
                      phtest = FALSE,

                      labels = NA,
                      estname = NA,
                      refcat = FALSE,
                      est.dec = 2,
                      ci.dec = 2,
                      pval.dec = 3,

                      color = "#EEEEEE",
                      htmlTable = TRUE,
                      title = "",
                      footer = TRUE){

    ### set contrast settings - factor labels lost with non-default settings
    current_contr = options("contrasts")
    options(contrasts = c("contr.treatment","contr.poly"))

    # check user inputs -------------------------------------------------------

    if (!is.na(fit[1])) regtype <- "multi"
    try(if (is.na(fit[1]) & (is.na(regtype) | !(regtype %in% c("uni", "multi")))) stop("regtype must be uni or multi\n"))

    if (is.na(fit[1])){

        try(if (class(df) != "data.frame" | is.na(covs[1]) | is.na(event[1]) | is.na(ttevent[1])) stop("must provide model object or dataframe + covariates + event + ttevent\n"))

        ## remove any covs that do not appear in dataset
        covs2 <- covs[covs %in% names(df)]
        if (length(covs2) != length(covs)) cat("Warning! Covariate(s):", covs[!(covs %in% names(df))],"do not exist in dataset\n")
        covs <- covs2
        try(if (length(covs) == 0) stop("No valid covs\n"))

        ## check that event appears in dataset
        event2 <- event[event %in% names(df)]
        try(if (length(event2) != 1) stop("Event variable: ", out[!(out %in% names(df))]," does not exist in dataset\n"))
        event <- event2

        ## check that ttevent appears in dataset
        ttevent2 <- ttevent[ttevent %in% names(df)]
        try(if (length(ttevent2) != 1) stop("Time to event variable: ", out[!(out %in% names(df))]," does not exist in dataset\n"))
        ttevent <- ttevent2

        try(if (class(covs[1]) != "character") stop("covs must be a character vector\n"))
        try(if (class(event[1]) != "character" | length(event) != 1) stop("event must be a single character string\n"))
        try(if (class(ttevent[1]) != "character" | length(ttevent) != 1) stop("ttevent must be a single character string\n"))
    }

    # define formats and helper functions -------------------------------------

    trim <- function(x) gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)

    simcap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
    }

    esformat <- paste("%." ,  est.dec, "f", sep="")
    ciformat <- paste("%." ,   ci.dec, "f", sep="")
    pvformat <- paste("%." , pval.dec, "f", sep="")

    ### function to format p-values to specified decimal places
    pvfun <- function(pvals){

        pvals2 <- sprintf(pvformat, round(pvals, pval.dec))

        if (pval.dec == 2) pvals2[pvals < 0.01  ]   <- "< 0.01"
        if (pval.dec == 3) pvals2[pvals < 0.001 ]   <- "< 0.001"
        if (pval.dec == 4) pvals2[pvals < 0.0001]   <- "< 0.0001"

        if (pval.dec == 2) pvals2[pvals > 0.99  ]   <- "> 0.99"
        if (pval.dec == 3) pvals2[pvals > 0.999 ]   <- "> 0.999"
        if (pval.dec == 4) pvals2[pvals > 0.9999]   <- "> 0.9999"

        if (htmlTable){
            pvals2 <- gsub("<", "&lt;", pvals2)
            pvals2 <- gsub(">", "&gt;", pvals2)
        }

        return(pvals2)
    }

    ### function to format summary(glmfit) coef table
    tblfun <- function(fit){

        coef_tbl <- data.frame(summary(fit)$coef)

        ### if no estimate name is specified pick a reasonable name
        if (is.na(estname) & regtype == "uni") estname <- "HR"
        if (is.na(estname) & regtype == "multi") estname <- "aHR"

        coef_tbl$Estimate <- exp(coef_tbl[,"coef"])
        coef_tbl$Estimate <- sprintf(esformat, round(coef_tbl$Estimate, est.dec))

        names(coef_tbl)[grepl("Est", names(coef_tbl))] <- estname
        names(coef_tbl)[grepl("Pr" , names(coef_tbl))] <- "p_value"

        ### conf function formats confidence intervals
        conf <- function(x){
            paste("[",
                  sprintf(ciformat, round(exp(x), ci.dec)[1]), ", ",
                  sprintf(ciformat, round(exp(x), ci.dec)[2]) , "]", sep="")
        }

        cimat <- data.frame(confint(fit))
        if (nrow(coef_tbl) == 1) coef_tbl$CI <- conf(t(cimat))
        if (nrow(coef_tbl) >  1) coef_tbl$CI <- apply(cimat,1,conf)

        coef_tbl$p_value <- pvfun(pvals = coef_tbl$p_value)

        ### get type II and type III p-values
        t2fit <- data.frame(Anova(fit, type = "II"))
        t3fit <- data.frame(Anova(fit, type = "III"))

        t2fit <- subset(t2fit, !is.na(Df))
        t3fit <- subset(t3fit, !is.na(Df))

        names(t2fit)[grepl("Pr" , names(t2fit))] <- "t2_pvalue"
        names(t3fit)[grepl("Pr" , names(t3fit))] <- "t3_pvalue"

        t2fit$t2_pvalue <- pvfun(pvals = t2fit$t2_pvalue)
        t3fit$t3_pvalue <- pvfun(pvals = t3fit$t3_pvalue)

        t2fit$varname <- row.names(t2fit)
        t3fit$varname <- row.names(t3fit)

        coef_tbl$coefname <- row.names(coef_tbl)

        vars <- NULL
        for (i in 1:nrow(t3fit)){
            vars <- c(vars, rep(t3fit[i, "varname"], t3fit[i,"Df"]))
        }

        ### get variable classes
        vtypes <- data.frame(attr(terms(fit), "dataClasses")[-1], stringsAsFactors = FALSE)
        names(vtypes) <- "vtype"
        vtypes$varname <- row.names(vtypes)

        ### get reference levels for factors
        xlevs <- fit$xlevels
        if (length(xlevs) > 0) {
            refs <- data.frame(sapply(xlevs, function(x) simcap(x[1])), stringsAsFactors = FALSE)
            names(refs) <- "ref"
            refs$varname <- row.names(refs)
        }
        if (length(xlevs) == 0){
            refs <- vtypes
            names(refs) <- c("ref", "varname")
            refs$ref <- NA
        }

        coef_tbl$varname <- vars

        coef_tbl$order <- 1:nrow(coef_tbl)

        coef_tbl2 <- merge(coef_tbl,   t2fit, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
        coef_tbl2 <- merge(coef_tbl2,  t3fit, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
        coef_tbl2 <- merge(coef_tbl2,   refs, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
        coef_tbl2 <- merge(coef_tbl2, vtypes, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)

        coef_tbl2$vtype[grepl(":",coef_tbl2$coefname)] <- "interaction"

        coef_tbl2$levname <- NA
        for (i in 1:nrow(coef_tbl2)){
            coef_tbl2$levname[i] <- simcap(substring(coef_tbl2$coefname[i], nchar(coef_tbl2$varname[i]) + 1))
        }

        coef_tbl2$comp <- paste(coef_tbl2$levname, "vs.", coef_tbl2$ref)
        coef_tbl2$comp[!(coef_tbl2$vtype %in% c("character", "factor"))] <- NA

        coef_tbl2 <- coef_tbl2[order(coef_tbl2$order), ]

        coef_tbl2$vseq <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) seq_along(x))

        # create a duplicate row for header - just for factors
        coef_tbl2 <- coef_tbl2[rep(seq_len(nrow(coef_tbl2)), each=2),]
        coef_tbl2$dup <- 0
        coef_tbl2$dup[grepl(".1", row.names(coef_tbl2))] <- 1
        coef_tbl2 <- subset(coef_tbl2, dup == 0 | (vtype %in% c("character", "factor", "interaction") & vseq == 1))

        coef_tbl2 <- coef_tbl2[order(coef_tbl2$order, -coef_tbl2$dup),]

        coef_tbl2$compref <- coef_tbl2$levname
        coef_tbl2$compref[coef_tbl2$dup == 1] <- paste(as.character(coef_tbl2$ref[coef_tbl2$dup == 1]), "(Ref)")
        coef_tbl2$compref[!(coef_tbl2$vtype %in% c("factor", "character"))] <- NA

        coef_tbl2$comp[coef_tbl2$dup == 1] <- NA

        coef_tbl2[coef_tbl2$dup == 1, estname  ] <- NA
        coef_tbl2[coef_tbl2$dup == 1, "CI"     ] <- NA
        coef_tbl2[coef_tbl2$dup == 1, "p_value"] <- NA
        coef_tbl2[coef_tbl2$dup == 0 & !(coef_tbl2$vtype %in% c("numeric","integer")), "t2_pvalue"] <- NA
        coef_tbl2[coef_tbl2$dup == 0 & !(coef_tbl2$vtype %in% c("numeric","integer")), "t3_pvalue"] <- NA

        coef_tbl2$order <- 1:nrow(coef_tbl2)
        coef_tbl2$vseq <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) seq_along(x))
        coef_tbl2$vrows <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) length(x))
        coef_tbl2$varname[coef_tbl2$vseq > 1] <- NA

        coef_tbl2$comp[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"] <- coef_tbl2$coefname[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"]
        coef_tbl2$compref[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"] <- coef_tbl2$coefname[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"]

        if ( refcat) coef_tbl3 <- coef_tbl2[,c("varname", "compref", estname, "CI", "p_value", "t2_pvalue", "t3_pvalue")]
        if (!refcat) coef_tbl3 <- coef_tbl2[,c("varname", "comp"   , estname, "CI", "p_value", "t2_pvalue", "t3_pvalue")]

        vrows  <- coef_tbl2$vrows[coef_tbl2$vseq == 1]
        vnames <- coef_tbl3$varname[coef_tbl2$vseq == 1]

        return(list("tbl"     = coef_tbl3
                    ,"vrows"  = vrows
                    ,"vnames" = vnames
                    ,"estname" = estname
        ))
    }

    # Create multiple regression table ----------------------------------------

    if (regtype == "multi") {

        ### if model is not provided, run the model
        covs_col <- paste(covs, collapse = " + ")

        if (is.na(fit[1]) & sum(!is.na(covs) > 0)  & !is.na(event[1]) & !is.na(ttevent[1])){
            if ( is.na(ttentry)) formch <- paste("Surv(", ttevent, ",", event, ") ~ ", covs_col)
            if (!is.na(ttentry)) formch <- paste("Surv(", ttentry, ",", ttevent, ",", event, ") ~ ", covs_col)
            if (!is.na(cluster)){
                formch <- paste(formch, "+ cluster(", cluster, ")")
            }
            if (!is.na(strata)){
                formch <- paste(formch, "+ strata(", strata, ")")
            }
            form <- as.formula(formch)
            if (is.na(weightvar)){
                coxfit <- coxph(form,
                                data = df,
                                method="breslow",
                                na.action=na.exclude)
            }
            if (!is.na(weightvar)){
                coxfit <- coxph(form,
                                data = df,
                                method="breslow",
                                weight = df[,weightvar],
                                na.action=na.exclude)
            }
            fit <- coxfit
        }

        tbl <- tblfun(fit)

        ftbl <- tbl[["tbl"]]
        ftbl <- ftbl[,names(ftbl) != "varname"]

        estname = tbl[["estname"]]

        head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p", "Type III p")
        alignr <- "lccrrr"

        if (type2 & !type3) {
            ftbl <- ftbl[,names(ftbl) != "t3_pvalue"]
            head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p")
            alignr <- "lccrr"
        }
        if (!type2 & type3) {
            ftbl <- ftbl[,names(ftbl) != "t2_pvalue"]
            head <- c("Variable", estname, "95% CI", "Wald p-value", "Type III p")
            alignr <- "lccrr"
        }
        if (!type2 & !type3) {
            ftbl <- ftbl[,!(names(ftbl) %in% c("t2_pvalue", "t3_pvalue"))]
            head <- c("Variable", estname, "95% CI", "p-value")
            alignr <- "lccr"
        }

        vnames = tbl[["vnames"]]
        if (!is.na(labels[1]) & sum(!is.na(labels)) == length(vnames)){
            vnames <- labels
        }

        footnote <- paste("Multiple CoxPH Regression, N = ", fit$n,
                          ", ",
                          "Events = ", fit$nevent,
                          sep= "")
        if (phtest) {
            footnote <- paste(footnote, ", Global cox.zph p-value: ", pvfun(pvals = cox.zph(fit)$table[nrow(cox.zph(fit)$table),"p"]), sep="")
            footer <- T
        }
        if (!footer) footnote <- ""

        print(
            htmlTable(ftbl,
                      header = head,
                      caption = title,
                      tfoot = footnote,
                      rnames = FALSE,
                      align = alignr,
                      rgroup = vnames,
                      n.rgroup = tbl[["vrows"]],
                      col.rgroup = c(color, "white"),
                      css.cell = "padding-left: 2em; padding-right: 1em;")
        )
    }

    # Create univariate regression table --------------------------------------

    if (regtype == "uni") {

        tbl_uni <- NULL
        vrows_uni <- NULL
        vnames_uni <- NULL
        nobs_uni <- NULL
        phtest_uni <- NULL

        for (j in 1:length(covs)){

            ### if model is not provided, run the model
            covs_col <- covs[j]

            if (is.na(fit[1]) & sum(!is.na(covs) > 0)  & !is.na(event[1]) & !is.na(ttevent[1])){
                if ( is.na(ttentry)) formch <- paste("Surv(", ttevent, ",", event, ") ~ ", covs_col)
                if (!is.na(ttentry)) formch <- paste("Surv(", ttentry, ",", ttevent, ",", event, ") ~ ", covs_col)
                if (!is.na(cluster)){
                    formch <- paste(formch, "+ cluster(", cluster, ")")
                }
                if (!is.na(strata)){
                    formch <- paste(formch, "+ strata(", strata, ")")
                }
                formj <- as.formula(formch)
                if (is.na(weightvar)){
                    coxfitj <- coxph(formj,
                                     data = df,
                                     method="breslow",
                                     na.action=na.exclude)
                }
                if (!is.na(weightvar)){
                    coxfitj <- coxph(formj,
                                     data = df,
                                     method="breslow",
                                     weight = df[,weightvar],
                                     na.action=na.exclude)
                }
                fitj <- coxfitj
            }

            tblj <- tblfun(fitj)

            tbl_uni    <- rbind(tbl_uni   , tblj[["tbl"]])
            vrows_uni  <- c(vrows_uni , tblj[["vrows"]])
            vnames_uni <- c(vnames_uni, tblj[["vnames"]])
            nobs_uni <- c(nobs_uni, paste("<br>(N = ", fitj$n,
                                          ", ",
                                          "Events = ", fitj$nevent, ")", sep=""))
            phtest_uni <- c(phtest_uni, cox.zph(fitj)$table[nrow(cox.zph(fitj)$table),"p"])
        }

        ftbl <- tbl_uni
        ftbl <- ftbl[,names(ftbl) != "varname"]

        estname <- tblj[["estname"]]

        head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p", "Type III p")
        alignr <- "lccrrr"

        if (type2 & !type3) {
            ftbl <- ftbl[,names(ftbl) != "t3_pvalue"]
            head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p")
            alignr <- "lccrr"
        }
        if (!type2 & type3) {
            ftbl <- ftbl[,names(ftbl) != "t2_pvalue"]
            head <- c("Variable", estname, "95% CI", "Wald p-value", "Type III p")
            alignr <- "lccrr"
        }
        if (!type2 & !type3) {
            ftbl <- ftbl[,!(names(ftbl) %in% c("t2_pvalue", "t3_pvalue"))]
            head <- c("Variable", estname, "95% CI", "p-value")
            alignr <- "lccr"
        }

        footnote <- "Univariate (unadjusted) CoxPH Regression"
        if (phtest){
            phtest_stars <- rep("", length(phtest_uni))
            # phtest_stars[phtest_uni <  0.10] <- "~"
            phtest_stars[phtest_uni <  0.05] <- "*"
            phtest_stars[phtest_uni <  0.01] <- "**"
            phtest_stars[phtest_uni < 0.001] <- "***"

            vnames_uni <- paste(vnames_uni, phtest_stars, sep="")
            footnote <- paste(footnote, ", cox.zph p-value: * = <0.05, ** = <0.01, *** = <0.001")

            footer <- TRUE
        }
        if (!footer) footnote <- ""

        vnames_uni <- paste(vnames_uni, nobs_uni)

        print(
            htmlTable(ftbl,
                      header = head,
                      caption = title,
                      tfoot = footnote,
                      rnames = FALSE,
                      align = alignr,
                      rgroup = vnames_uni,
                      n.rgroup = vrows_uni,
                      col.rgroup = c(color, "white"),
                      css.cell = "padding-left: 3em; padding-right: 1em;")
        )

    }

    ### restore contrast settings
    options(contrasts = unlist(current_contr))

    return(ftbl)
}



