#' Lisa's Survival Plot
#'
#' This function creates a survival plot from a survfit object using ggplot2 graphics.
#' @param sfit survfit object (REQUIRED).
#' @param surv.lty Number for line type of survival lines. Default = 1.
#' @param surv.size Number for size of survival lines. Default = 1.
#' @param surv.col Character for color of survival lines. Default = "black".
#' @param ci Logical indicator to plot confidence intervals. Default = TRUE.
#' @param ci.lty Number for line type of CI lines. Default = 2.
#' @param ci.size NUmber for size of CI lines, Default = 1.
#' @param ci.ribbon Logical indicator to use ribbons instead of lines for CI area. Default = FALSE. 
#' @param cens Logical indicator to plot markers for censoring. Default = TRUE.
#' @param cens.col Character for color of censoring symbols. Default = "red".
#' @param cens.shape Number for censoring symbol (pch graphical parameter). Default = 3 (plus sign).
#' @param groups Character vector for groups as listed in dataset. Default = NA (no groups).
#' @param grlabs Character vector for group levels, must be in same order as groups. Default = NA (no groups or use group levels from dataset).
#' @param grname Character label for group legend name. Default = NA (no legend title).
#' @param perc Logical indicator to show y axis as percentages. Default = FALSE.
#' @param xlab Character label for x axis. Default = "Time".
#' @param ylab Character label for y axis. Default = "Survival".
#' @param main Character label for plot title. Default = " ".
#' @param themebw Indicator for use of ggplot2 theme_bw. Default = TRUE. 
#' @keywords summary KM curve plot ggplot2 survival consulting Lisa 
#' @import survival
#' @import ggplot2
#' @importFrom pammtools geom_stepribbon
#' @export
ggsurv <- function(sfit,
                   surv.lty = 1,
                   surv.size = 1,
                   surv.col = NA, 
                   ci = TRUE, 
                   ci.lty = 2,
                   ci.size = 1,
                   ci.ribbon = FALSE,
                   cens = TRUE,
                   cens.col = "red", 
                   cens.size = 1,
                   cens.shape = 3, 
                   groups = NA,
                   grlabs = NA,
                   grname = NA,
                   perc = FALSE,
                   xlab = "\n Time",
                   ylab = "Survival \n", 
                   main = "", 
                   themebw = TRUE){
    
    strata <- ifelse(is.null(sfit$strata) ==T, 1, length(sfit$strata))
    stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
    stopifnot(length(surv.lty) == 1 | length(surv.lty) == strata)
    
    if (strata == 1){
        
        dat <- data.frame(time = c(0, sfit$time    ),
                          surv = c(1, sfit$surv    ),
                          up   = c(1, sfit$upper   ),
                          low  = c(1, sfit$lower   ),
                          cens = c(0, sfit$n.censor))
        dat.cens <- subset(dat, cens != 0)
        
        if (is.na(surv.col)) surv.col = "black"
        
        pl <- ggplot(dat, aes(x = time, y = surv)) +
                     xlab(xlab) +
                     ylab(ylab) + 
                     ggtitle(main) +
                     geom_step(col = surv.col, lty = surv.lty, size = surv.size) +
                     scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
        
        if (ci){
            if (ci.ribbon){
                pl <- pl + geom_stepribbon(aes(ymin = low, ymax = up), alpha = 0.2, fill = surv.col)
            }
            if (!ci.ribbon){
                pl <- pl + 
                      geom_step(aes(y = up ), color = surv.col, lty = ci.lty, size = ci.size) +
                      geom_step(aes(y = low), color = surv.col, lty = ci.lty, size = ci.size)
            }
        }
        
        if (cens & (nrow(dat.cens) > 0)){
            pl <- pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col, size = cens.size)
        }
        
        if (themebw) pl <- pl + theme_bw()
        
        pl
    }
    
    if (strata > 1){
        
        n <- sfit$strata
        
        grps  <-  as.character(unlist(lapply(names(sfit$strata), function(x) strsplit(x, '=')[[1]][2])))
        gr.name <-  unlist(strsplit(names(sfit$strata[1]), '='))[1]
        gr.df   <- vector('list', strata)
        ind     <- vector('list', strata)
        n.ind   <- c(0,n); n.ind <- cumsum(n.ind)
        for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
        
        for(i in 1:strata){
            gr.df[[i]] <- data.frame(time  = c(0, sfit$time[ ind[[i]] ]),
                                     surv  = c(1, sfit$surv[ ind[[i]] ]),
                                     up    = c(1, sfit$upper[ ind[[i]] ]),
                                     low   = c(1, sfit$lower[ ind[[i]] ]),
                                     cens  = c(0, sfit$n.censor[ ind[[i]] ]),
                                     group = rep(grps[i], n[i] + 1),
                                     stringsAsFactors = FALSE)
        }
        
        # check group names (if not NA), remove group names if invalid
        if (!is.na(groups[1])) {
            if (sum(groups %in% grps) != length(grps)) groups <- grlabs <- NA
        }
        
        # apply group labels (if not NA)
        if (is.na(groups[1])) groups <- grps
        if (is.na(grlabs[1])) grlabs <- grps
        
        dat <- do.call(rbind, gr.df)
        dat$group <- factor(dat$group,
                            levels= groups,
                            labels = grlabs)
        dat.cens <- subset(dat, cens != 0)
        
        pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
                     xlab(xlab) + 
                     ylab(ylab) + 
                     ggtitle(main) +
                     geom_step(aes(col = group), lty = surv.lty, size = surv.size) +
                     scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
        
        legend_title <- " "
        if (!is.na(grname)) legend_title <- grname
        
        ### colors for survival lines    
        if (length(surv.col == 1) & is.na(surv.col[1])){
            pl <- pl + scale_colour_discrete(name = legend_title)
        }
        if (!is.na(surv.col[1]) & length(surv.col == 1)){
            pl <- pl + scale_colour_manual(name = legend_title, values = rep(surv.col, strata))
        }
        if (!is.na(surv.col[1]) & length(surv.col > 1)){    
            pl <- pl + scale_colour_manual(name = legend_title, values = surv.col)
        }
        
        ### linetypes for survival lines
        if (length(surv.lty) == 1){
            pl <- pl + scale_linetype_manual(name = legend_title, values = rep(surv.lty, strata))
        }
        if (length(surv.lty) > 1){ 
            pl <- pl + scale_linetype_manual(name = legend_title, values = surv.lty)
        }
        
        ### plotting confidence intervals
        if (ci){
            if (ci.ribbon){
                pl <- pl + geom_stepribbon(aes(ymin = low, ymax = up, fill = group), alpha = 0.2)
                
                if (length(surv.col == 1) & is.na(surv.col[1])){
                    pl <- pl + scale_fill_discrete(name = legend_title)
                }
                if (!is.na(surv.col[1]) & length(surv.col == 1)){
                    pl <- pl + scale_fill_manual(name = legend_title, values = rep(surv.col, strata))
                }
                if (!is.na(surv.col[1]) & length(surv.col > 1)){    
                    pl <- pl + scale_fill_manual(name = legend_title, values = surv.col)
                }
                
            }
            if (!ci.ribbon){
                pl <- pl + 
                    geom_step(aes(y = up , color = group), lty = ci.lty, size = ci.size) +
                    geom_step(aes(y = low, color = group), lty = ci.lty, size = ci.size)
            }
        }
        
        ### plotting censored observations
        if (cens & (nrow(dat.cens) > 0)){
            pl <- pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col, size = cens.size)
        }
        
        if (themebw) pl <- pl + theme_bw()
        
        pl
    }
    return(pl)
}
