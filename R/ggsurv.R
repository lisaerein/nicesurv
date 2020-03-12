#' Lisa's Survival Plot
#'
#' This function creates a survival plot from a survfit object using ggplot2 graphics.
#' @param sfit survfit object (REQUIRED).
#' @param surv.lty Numeric vector for line type of survival lines. Default = 1.
#' @param surv.size Number for size of survival lines. Default = 1.
#' @param surv.col Character vector for color of survival lines. Default = "black".
#' @param ci Logical indicator to plot confidence intervals. Default = TRUE.
#' @param ci.lty Number for line type of CI lines. Default = 2.
#' @param ci.size Number for size of CI lines, Default = 1.
#' @param ci.ribbon Logical indicator to use ribbons instead of lines for CI area. Default = FALSE.
#' @param ci.alpha Number between 0 and 1 for shading (alpha) of CI ribbon. Default = 0.20.
#' @param cens Logical indicator to plot markers for censoring. Default = TRUE.
#' @param cens.col Character for color of censoring symbols. Default = "red".
#' @param cens.shape Number for censoring symbol (pch graphical parameter). Default = 3 (plus sign).
#' @param groups Character vector for groups as listed in dataset. Default = NA (no groups).
#' @param grlabs Character vector for group levels, must be in same order as groups. Default = NA (no groups or use group levels from dataset).
#' @param grname Character label for group legend name. Default = NA (no legend title).
#' @param perc Logical indicator to show y axis as percentages. Default = FALSE.
#' @param xlab Character label for x axis. Default = "Time".
#' @param ylab Character label for y axis. Default = "Survival".
#' @param xlim Numeric vector of length 2 for x axis min and max. Default = NA using 0 to data maximum.
#' @param ylim Numeric vector of length 2 for y axis min and max. Default = c(0,1).
#' @param xby Number for x axis major tick marks. Default = NA (ggplot selected).
#' @param yby Numeric for y axis major tick marks. Default = 0.1.
#' @param risktab Logical indicator to include risk table at bottom of plot. Default = FALSE.
#' @param risktab.times Numeric vector of times for risk table. Default = NA (Includes times for x axis tick marks).
#' @param risktab.label Character label for risk table. Default = "Number at risk".
#' @param risktab.margins Numeric vector for plot margins in format c(top, right, bottom, left) where each value is a proportion of plot dimensions.
#' Adjustments may be needed to accomodate long group names or a large number of groups. Default = c(0.00, 0.00, 0.20, 0.15).
#' @param risktab.events Logical indicator to include number of events (for non-stratified plots only). Default = TRUE.
#' @param main Character label for plot title. Default = " ".
#' @param plab Character for annotation in top right corner (for p-value or other notes). Default = NA (no annotation).
#' @param grid Logical vector of length 2 to plot major gridlines in x and y directions. Default = c(TRUE, TRUE).
#' If one value is provided it will apply to both directions.
#' @param themebw Indicator for use of ggplot2 theme_bw. Default = TRUE.
#' @param cuminc Logical indicator whether to apply 1-x transformation to plot cumulative incidence. Default = FALSE.
#' @keywords summary KM curve plot ggplot2 survival consulting Lisa
#' @import survival
#' @import ggplot2
#' @importFrom pammtools geom_stepribbon
#' @export
ggsurv <- function(sfit
                  ,surv.lty = 1
                  ,surv.size = 1
                  ,surv.col = NA
                  ,ci = TRUE
                  ,ci.lty = 2
                  ,ci.size = 0.5
                  ,ci.ribbon = TRUE
                  ,ci.alpha = 0.20
                  ,cens = TRUE
                  ,cens.col = "red"
                  ,cens.size = 2
                  ,cens.shape = 3
                  ,groups = NA
                  ,grlabs = NA
                  ,grname = NA
                  ,perc = FALSE
                  ,xlab = "\nTime"
                  ,ylab = "Survival\n"
                  ,xlim = NA
                  ,ylim = c(0,1)
                  ,xby = NA
                  ,xbrlabs = NA
                  ,yby = 0.1
                  ,risktab = FALSE
                  ,risktab.times = NA
                  ,risktab.label = "Number at risk"
                  ,risktab.margins = c(0.00, 0.00, 0.20, 0.15)
                  ,risktab.events = TRUE
                  ,main = ""
                  ,plab = NA
                  ,grid = c(TRUE, TRUE)
                  ,themebw = TRUE
                  ,citrans = FALSE
                   ){

    strata <- ifelse(is.null(sfit$strata) == TRUE, 1, length(sfit$strata))

    if (is.na(surv.col[1]) & strata == 1) surv.col <- "black"

    if (is.na(surv.col[1]) & strata  > 1) surv.col <- c("red2",
                                                        "dodgerblue",
                                                        "darkorange",
                                                        "forestgreen",
                                                        "mediumpurple",
                                                        "royalblue4",
                                                        "turquoise",
                                                        "gold",
                                                        "lightcyan3",
                                                        "lightcoral",
                                                        "darkolivegreen3")

    if (length(surv.col) == 1) surv.col  <- rep(surv.col , strata)
    if (length(surv.lty) == 1) surv.lty  <- rep(surv.lty , strata)

    if (length(surv.col) > strata ) surv.col  <- surv.col[1:strata]
    if (length(surv.lty) > strata ) surv.lty  <- surv.lty[1:strata]

    if ((length(surv.col ) > 1) & (length(surv.col ) < strata)) surv.col  <- rep(surv.col , length = strata)
    if ((length(surv.lty ) > 1) & (length(surv.lty ) < strata)) surv.lty  <- rep(surv.lty , length = strata)

    if (length(surv.size) > 1) surv.size <- surv.size[1]

    if (!ci) ci.ribbon <- FALSE

    if (length(grid) == 1) grid <- rep(grid,2)

    if (sum(is.na(xlim)) == 0){
      xmin <- xlim[1]
      xmax <- xlim[2]
      xlim <- c(xmin, xmax)
      if ( is.na(xby)) xbrs <- pretty(xlim, 10)
      if (!is.na(xby)) xbrs <- seq(xmin, xmax, by = xby)
      xmin_exp <- xmin - (0.01*diff(xlim))
      xmax_exp <- xmax + (0.01*diff(xlim))
    }
    if (sum(is.na(xlim)) > 0) {
      xmin <- 0
      xmax <- max(sfit$time)
      xlim <- c(xmin, xmax)
      if ( is.na(xby)) xbrs <- pretty(xlim, 10)
      if (!is.na(xby)) xbrs <- seq(xmin, xmax, by = xby)
      xmin_exp <- xmin - (0.01*diff(xlim))
      xmax_exp <- xmax + (0.01*diff(xlim))
    }
    if (length(xbrlabs) == 1 & is.na(xbrlabs[1])) xbrlabs <- xbrs

    if (sum(is.na(ylim)) == 0){
      ymin <- ylim[1]
      ymax <- ylim[2]
      ylim <- c(ymin, ymax)
      ybrs <- seq(ymin, ymax, by = yby)
      ymin_exp <- ymin - (0.01*diff(ylim))
      ymax_exp <- ymax + (0.01*diff(ylim))
    }
    if (sum(is.na(ylim)) > 0) {
      ymin <- 0
      ymax <- 1
      ylim <- c(ymin, ymax)
      ybrs <- seq(ymin, ymax, by = yby)
      ymin_exp <- ymin - (0.01*diff(ylim))
      ymax_exp <- ymax + (0.01*diff(ylim))
    }

    if (strata == 1){

        dat <- data.frame(time = c(0, sfit$time    ),
                          surv = c(1, sfit$surv    ),
                          up   = c(1, sfit$upper   ),
                          low  = c(1, sfit$lower   ),
                          cens = c(0, sfit$n.censor))
        if (citrans){
            dat$surv <- 1 - dat$surv
            dat$up   <- 1 - dat$up
            dat$low  <- 1 - dat$low
        }

        dat0 <- subset(dat, (time >= xmin_exp) & (time <= xmax_exp))

        if (xmin > min(dat$time)) {
            dat_below <- subset(dat, time < xmin_exp)
            dat_below <- dat_below[order(-dat_below$time),] ## get most recent timepoint and carry forward to xmin
            dat_below <- dat_below[1,]
            dat_below$time <- xmin_exp
            dat_below$cens <- 0
            dat0 <- rbind(dat_below, dat0)
        }
        if (xmax < max(dat$time)){
            dat_above <- subset(dat, time > xmax_exp)
            dat_above <- dat_above[order(dat_above$time),] ## get next timepoint and carry back to xmax
            dat_above <- dat_above[1,]
            dat_above$time <- xmax_exp
            dat_above$cens <- 0
            dat0 <- rbind(dat0, dat_above)
        }

        dat <- dat0
        dat.cens <- subset(dat, cens != 0)

        pl <- ggplot(dat, aes(x = time, y = surv)) +
                xlab(xlab) +
                ylab(ylab) +
                ggtitle(main) +
                geom_step(col = surv.col, lty = surv.lty, size = surv.size) +
                scale_x_continuous(breaks = xbrs, expand = c(0.01, 0), labels = xbrlabs) +
                coord_cartesian(xlim = xlim, ylim = ylim, clip = "off")

        if (!perc) pl <- pl + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0))
        if ( perc) pl <- pl + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0),
                                                 labels = scales::percent_format(accuracy = 1))

        if (ci){
            if (ci.ribbon){
                pl <- pl + geom_stepribbon(aes(ymin = low, ymax = up), fill = surv.col, alpha = ci.alpha)
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

        if (!is.na(plab)){
            pl <- pl + annotate(geom = "text", x = xmax, y = ymax, label = plab, hjust = 1, vjust = 1)
        }

        if (risktab){
            if (length(risktab.times) == 1 & is.na(risktab.times[1])) risktab.times <- xbrs

            risktab.sum <- summary(sfit, times = risktab.times)
            risktab.ns <- risktab.sum$n.risk
            risktab.ts <- risktab.sum$time
            risktab.ev <- risktab.sum$n.event

            ystep <- (diff(ylim)*0.10)*(risktab.margins[3]/0.2)
            xstep <- diff(xlim)*0.10
            ystep1 <- 2.5*ystep
            ystep2 <- 3.5*ystep

            pl <- pl +
                theme(aspect.ratio= 3/5) +
                theme(legend.position = "none") +
                theme(plot.margin=unit(risktab.margins, "npc")) +
                annotate("text", x = xmin - (0.5*xstep), y = ymin - ystep1, label = "Number at risk"  , hjust = 1, fontface = 2) +
                annotate("text", x = risktab.ts, y = ymin - ystep1, label = risktab.ns, hjust = 0.5)

            if (risktab.events){
                pl <- pl +
                    annotate("text", x = xmin - (0.5*xstep), y = ymin - ystep2, label = "Number of events", hjust = 1, fontface = 2) +
                    annotate("text", x = risktab.ts, y = ymin - ystep2, label = risktab.ev, hjust = 0.5)
            }
        }

        pl <- pl +
              theme(panel.grid.minor = element_blank())
              # + theme(axis.title.x = element_text(face="bold"))
              # + theme(axis.title.y = element_text(face="bold"))
        if (!grid[1]) pl <- pl + theme(panel.grid.major.x = element_blank())
        if (!grid[2]) pl <- pl + theme(panel.grid.major.y = element_blank())
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
            if (citrans){
                gr.df[[i]]$surv <- 1 - gr.df[[i]]$surv
                gr.df[[i]]$up   <- 1 - gr.df[[i]]$up
                gr.df[[i]]$low  <- 1 - gr.df[[i]]$low
            }

            dat0 <- subset(gr.df[[i]], (time >= xmin_exp) & (time <= xmax_exp))

            if (xmin_exp > min(gr.df[[i]]$time)) {
                dat_below <- subset(gr.df[[i]], time < xmin_exp)
                dat_below <- dat_below[order(dat_below$time),] ## get most recent timepoint and carry forward to xmin
                dat_below <- dat_below[1,]
                dat_below$time <- xmin_exp
                dat_below$cens <- 0
                dat0 <- rbind(dat_below, dat0)
            }
            if (xmax_exp < max(gr.df[[i]]$time)){
                dat_above <- subset(gr.df[[i]], time > xmax_exp)
                dat_above <- dat_above[order(dat_above$time),] ## get next timepoint and carry back to xmax
                dat_above <- dat_above[1,]
                dat_above$time <- xmax_exp
                dat_above$cens <- 0
                dat0 <- rbind(dat0, dat_above)
            }

            dat0$surv[dat0$surv > ymax_exp] <- ymax_exp
            dat0$surv[dat0$surv < ymin_exp] <- ymin_exp

            dat0$up[dat0$up > ymax_exp] <- ymax_exp
            dat0$up[dat0$up < ymin_exp] <- ymin_exp

            dat0$low[dat0$low > ymax_exp] <- ymax_exp
            dat0$low[dat0$low < ymin_exp] <- ymin_exp

            gr.df[[i]] <- dat0

        }

        # check group names (if not NA), remove group names if invalid
        if (!is.na(groups[1])) {
            if (sum(groups %in% grps) != length(grps)) groups <- grlabs <- NA
        }

        # apply group labels (if not NA)
        if (is.na(groups[1])) groups <- grps
        if (is.na(grlabs[1])) grlabs <- groups

        dat <- do.call(rbind, gr.df)
        dat$group <- factor(dat$group,
                            levels= groups,
                            labels = grlabs)
        dat.cens <- subset(dat, cens != 0)

        pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
                xlab(xlab) +
                ylab(ylab) +
                ggtitle(main) +
                geom_step(aes(col = group, lty = group), size = surv.size) +
            scale_x_continuous(breaks = xbrs, expand = c(0.01, 0), labels = xbrlabs) +
            coord_cartesian(xlim = xlim, ylim = ylim, clip = "off")

        if (!perc) pl <- pl + scale_y_continuous(breaks = ybrs, expand = c(0.01,0))
        if ( perc) pl <- pl + scale_y_continuous(breaks = ybrs, expand = c(0.01,0),
                                                 labels = scales::percent_format(accuracy = 1))

        legend_title <- " "
        if (!is.na(grname)) legend_title <- grname

        ### colors for survival lines
        if (is.na(surv.col[1]) & length(surv.col == 1) ){
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
                pl <- pl + geom_stepribbon(aes(ymin = low, ymax = up, fill = group), alpha = ci.alpha)

                if (is.na(surv.col[1]) & length(surv.col == 1)){
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

        if (!is.na(plab)){
            pl <- pl + annotate(geom = "text", x = xmax, y = ymax, label = plab, hjust = 1, vjust = 1)
        }

        pl <- pl +
            theme(panel.grid.minor = element_blank()) +
            # theme(axis.title.x = element_text(face="bold")) +
            # theme(axis.title.y = element_text(face="bold")) +
            theme(legend.position = c(0, 0)
                  , legend.justification = c(0,0)
                  , legend.background = element_rect(fill = "transparent", colour = "transparent")
                  , legend.key.width = unit(3, "char"))

        if (!grid[1]) pl <- pl + theme(panel.grid.major.x = element_blank())
        if (!grid[2]) pl <- pl + theme(panel.grid.major.y = element_blank())

        if (risktab){
            if (length(risktab.times) == 1 & is.na(risktab.times[1])) risktab.times <- xbrs

            risktab.sum <- summary(sfit, times = risktab.times)

            ystep <- (diff(ylim)*0.10)*(risktab.margins[3]/0.2)
            xstep <- diff(xlim)*0.10
            ystep1 <- 2.0*ystep
            grstep <- ystep*(2/strata)

            pl <- pl +
                theme(aspect.ratio= 3/5) +
                theme(legend.position = "none") +
                theme(plot.margin=unit(risktab.margins, "npc")) +
                annotate("text", x = xmin - (0.5*xstep), y = ymin - ystep1, label = risktab.label, hjust = 1, fontface = 2)

            for (i in 1:length(groups)){
                grpi <- paste(gr.name, groups[i], sep="=")

                risktab.ns <- risktab.sum$n.risk[risktab.sum$strata == grpi]
                risktab.ts <- risktab.sum$time[risktab.sum$strata == grpi]
                risktab.ev <- risktab.sum$n.event[risktab.sum$strata == grpi]

                pl <- pl +
                    annotate("text", x = xmin - (1.6*xstep), y = ymin - ystep1 - (i*grstep), label = grlabs[i] , hjust = 1.0) +
                    annotate("text", x = risktab.ts        , y = ymin - ystep1 - (i*grstep), label = risktab.ns, hjust = 0.5) +
                    annotate("line",
                             x = c(xmin - (1.4*xstep),
                                   xmin - (0.6*xstep)),
                             y = c(ymin - ystep1 - (i*grstep),
                                   ymin - ystep1 - (i*grstep)),
                             color = surv.col[i],
                             size = surv.size,
                             lty = surv.lty[i])
                if (ci & ci.ribbon){
                    pl <- pl +
                        annotate("rect",
                             xmin = xmin - (1.5*xstep),
                             xmax = xmin - (0.5*xstep),
                             ymin = ymin - ystep1 - ((i + 0.4)*grstep),
                             ymax = ymin - ystep1 - ((i - 0.4)*grstep),
                             alpha = ci.alpha,
                             fill = surv.col[i])
                    }

            }
        }

        pl
    }
    return(pl)
}
