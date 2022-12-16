#' Lisa's Cumulative Incidence Plots
#'
#' This function creates cumulative incidence plots from a survfit object of type "mstate" using gglot2 graphics.
#' It returns a ggplot2 object and prints the resulting plot.
#' @param msfit survfit object of type "mstate" (REQUIRED).
#' @param groups Character vector for groups as listed in dataset. Default = NA (no groups).
#' @param grlabs Character vector for group names, must be in same order as groups. Default = NA (no groups or use group levels from dataset).
#' @param grname Character label for group legend. Default = " ".
#' @param events Character vector for event names (do not include 0 level).
#' @param evlabs Character vector for event labels (do not include 0 level).
#' @param evtitle Character label for event legend. Default = "Events".
#' @param perc Logical indicator to show y axis as percentages. Default = FALSE.
#' @param xlab Character label for x axis. Default = "Time".
#' @param ylab Character label for y axis. Default = "Probability".
#' @param xlim Numeric vector of length 2 for x axis min and max. Default = NA, using 0 to data maximum.
#' @param ylim Numeric vector of length 2 for y axis min and max. Default = c(0, 1).
#' @param xby Number for x axis major tick marks. Default = NA (ggplot selected).
#' @param yby Numeric for y axis major tick marks. Default = 0.1.
#' @param xbrlabs Vector for alternative x axis labels. Default = NA.
#' @param stack Character to indicate stacking events "events" or groups "groups". Default = "events".
#' @param state0 Character label for state 0. Default = "state_0".
#' @param cuminc.col Character vector for color of shaded cumulative incidence bands (do not include 0 level). Default = default colors.
#' @param cuminc.lty Numeric vector for line type of cumulative incidence lines. Default = 1.
#' @param cuminc.size Number for size of cumulative incidence lines. Default = 1.
#' @param step Logical indicator to use step function or not. Default = TRUE.
#' @param main Character label for plot title. Default = " ".
#' @param graystest Logical indicator to provide Gray's test p-values (Default = FALSE). If TRUE, you must provide df, timevar, and eventvar.
#' @param df Dataframe to use for Gray's test. Default = NA.
#' @param timevar Character name of time to event variable. Default = NA.
#' @param eventvar Character nane of event variable. Default = NA.
#' @keywords summary survival plot consulting Lisa cumulative incidence mstate cmprsk
#' @import survival
#' @import ggplot2
#' @import cmprsk
#' @export
ggcuminc <- function(msfit,
                    groups = NA,
                    grlabs = NA,
                    grname = " ",
                    events = NA,
                    evlabs = NA,
                    evtitle = "Event",
                    perc = FALSE,
                    xlab = "Time",
                    ylab = "Probability",
                    xlim = NA,
                    ylim = c(0,1),
                    xby = NA,
                    yby = 0.1,
                    xbrlabs = NA,
                    stack = "events",
                    state0 = "state_0",
                    cuminc.col = NA,
                    cuminc.lty = NA,
                    cuminc.size = 1.5,
                    step = TRUE,
                    main = "",
                    graystest = FALSE,
                    df = NA,
                    timevar = NA,
                    eventvar = NA){

    ### error checking
    if (graystest){
        if (class(df) != "data.frame") graystest <- FALSE
        if (is.na(timevar)) graystest <- FALSE
        if (is.na(eventvar)) graystest <- FALSE
        if (class(df) == "data.frame") {
            if (!(timevar %in% names(df)) | !(eventvar %in% names(df))){
                graystest <- FALSE
            }
        }
    }

    if (is.na(stack) | !(stack %in% c("events", "groups"))) stack <- "events"

    est <- summary(msfit)

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
        xmax <- max(msfit$time)
        xlim <- c(xmin, xmax)
        if ( is.na(xby)) xbrs <- pretty(xlim, 10)
        if (!is.na(xby)) xbrs <- seq(xmin, xmax, by = xby)
        xmin_exp <- xmin - (0.01*diff(xlim))
        xmax_exp <- xmax + (0.01*diff(xlim))
    }
    if (length(xbrlabs) == 1 & is.na(xbrlabs[1])) xbrlabs <- xbrs

    times <- c(0, unique(est$time), max(msfit$time))

    est <- summary(msfit, times = times)

    probs <- data.frame(est$pstate)
    names(probs) <- est$states
    names(probs)[which(names(probs) %in% "(s0)")] <- "state_0"

    ### is model stratified or not?
    by <- ifelse(is.null(msfit$strata) == TRUE, NA, names(msfit$strata)[1])

    if (!is.na(by)){

        by <- unlist(strsplit(by, '='))[1]

        res <- data.frame("group" = as.character(est$strata)
                          ,"time" = c(est$time)
                          ,rbind(probs)
                          ,stringsAsFactors = FALSE
                          )

        res_l <- melt(res
                      ,id.vars = c("group", "time")
                      ,value.name = "prob"
                      ,variable.name = "event"
                      )

        if (step){
            res_l2 <- ddply(res_l
                            ,c("event","group")
                            ,mutate
                            ,time_l = (lead(time,1)- 1e-9)
                            )
        }
        if (!step){
            res_l2 <- ddply(res_l
                            ,c("event","group")
                            ,mutate
                            ,time_l = time + 1e-9
                            )
        }

        old <- res_l2[,c("group","event", "time", "prob")]
        new <- res_l2[,c("group", "event", "time_l", "prob")]
        names(new) <- names(old)
        res_l <- rbind(old,new)
        res_l <- subset(res_l, time >= 0)
        res_l <- res_l[order(res_l$group, res_l$event, res_l$time),]
        res_l <- res_l[!duplicated(res_l),]
    }
    if (is.na(by)){
        res <- data.frame("time" = c(est$time)
                          ,rbind(probs)
                          )

        res_l <- melt(res
                      ,id.vars = c("time")
                      ,value.name = "prob"
                      ,variable.name = "event"
                      )
        if (step){
            res_l2 <- ddply(res_l
                            ,"event"
                            ,mutate
                            ,time_l = (lead(time, 1) - 1e-9)
                            )
        }
        if (!step){
            res_l2 <- ddply(res_l
                            ,"event"
                            ,mutate
                            ,time_l = time + 1e-9
                            )
        }

        old <- res_l2[,c("event", "time", "prob")]
        new <- res_l2[,c("event", "time_l", "prob")]
        names(new) <- names(old)
        res_l <- rbind(old, new)
        res_l <- res_l[!duplicated(res_l),]
    }

    ## check if input events/labels are valid and apply if so
    evnts <- levels(res_l$event)

    if (!is.na(events[1])) {
        if (sum(events %in% evnts) != length(events)) events <- NA
    }

    state0_lab <- "state_0"
    if (!is.na(state0)) state0_lab <- state0

    if (is.na(events[1])) events <- evnts[-1]
    if (is.na(evlabs[1])) evlabs <- evnts[-1]

    events <- c("state_0", events)
    evlabs <- c(state0_lab, evlabs)

    res_l$event <- factor(res_l$event
                          ,levels = events
                          ,labels = evlabs
                          )

    ## check if input groups/labels are valid and apply if so
    if (!is.na(by)){

        grps  <-  as.character(unlist(lapply(names(msfit$strata), function(x) strsplit(x, '=')[[1]][2])))
        gr.name <-  unlist(strsplit(names(msfit$strata[1]), '='))[1]

        res_l$group <- as.character(unlist(lapply(res_l$group, function(x) strsplit(x, '=')[[1]][2])))

        # check group names (if not NA), remove group names if invalid
        if (!is.na(groups[1])) {
            if (sum(groups %in% grps) != length(grps)) groups <- grlabs <- NA
        }

        # apply group labels (if not NA)
        if (is.na(groups[1])) groups <- grps
        if (is.na(grlabs[1])) grlabs <- groups

        res_l$group <- factor(res_l$group,
                              levels = groups,
                              labels = grlabs)

        # estlabs <- levels(est$strata)
        # estlabs <- gsub(paste(by, "=", sep=""), "", estlabs)

    }

    # cols <- c("#ECF0F1","#D2B4DE","#76D7C4","#F8C471","#7fbbe1","#F7DC6F","#EC7063","#A6DBFF")
    cols <- c("#ECF0F1","tomato","dodgerblue","gold","forestgreen","darkorchid","burleywood","darkorange","turquoise3")
    ltys <- rep(1, length(groups))
    if (!is.na(cuminc.col[1])) cols <- c("#ECF0F1", cuminc.col)
    if (!is.na(cuminc.lty[1])) ltys <- cuminc.lty

    if (stack == "events"){
        g <- ggplot(data = res_l, aes(x = time, y = prob)) +
                    geom_area(aes(fill = event, color = event)) +
                    scale_fill_manual(name = evtitle , values = cols, labels = evlabs) +
                    scale_color_manual(name = evtitle, values = cols, labels = evlabs) +
                    theme_bw() +
                    theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          strip.background = element_blank(),
                          legend.key.width=unit(2,"line")) +
                    scale_x_continuous(breaks = xbrs, expand = c(0.01, 0), labels = xbrlabs) +
                    coord_cartesian(xlim = xlim, ylim = ylim, clip = "on") +
                    xlab(xlab) +
                    ylab(ylab) +
                    ggtitle(main)
        if (!perc) g <- g + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0))
        if ( perc) g <- g + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0),
                                               labels = scales::percent_format(accuracy = 1))
        if (!is.na(by)){
            g <- g + facet_grid(.~group)
        }
    }

    if (stack == "groups"){

        if (is.na(by)){
            g <- ggplot(data = subset(res_l, event != evlabs[which(events == state0)]), aes(x = time, y = prob)) +
                        geom_step(size = cuminc.size, colour = cols[1], linetype = ltys[1]) +
                        # scale_linetype_manual(name = grname, values = ltys, labels = grlabs) +
                        # scale_color_manual(name = grname, values = cols, labels = grlabs) +
                        facet_wrap(~event, nrow = 1) +
                        theme_bw() +
                        theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              strip.background = element_blank(),
                              legend.key.width=unit(2,"line")) +
                        scale_x_continuous(breaks = xbrs, expand = c(0.01, 0), labels = xbrlabs) +
                        coord_cartesian(xlim = xlim, ylim = ylim, clip = "on") +
                        xlab(xlab) +
                        ylab(ylab) +
                        ggtitle(main)
            if (!perc) g <- g + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0))
            if ( perc) g <- g + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0),
                                                   labels = scales::percent_format(accuracy = 1))
        }

        if (!is.na(by)){
        if (graystest){
            gtest <- cuminc(df[,timevar], df[,eventvar], group=df[,gr.name])$Tests[,"pv"]
            p <- data.frame(gtest)
            p$event <- rownames(p)
            p$event <- factor(p$event,
                              levels = events,
                              labels = evlabs)
            p <- subset(p, event != evlabs[which(events == state0)])
            p$x <- xmin
            p$y <- ymax
            p$pval <- paste("Gray's test p =", sprintf("%4.3f", p$gtest))
            p$pval[p$gtest < 0.001] <- "Gray's test p < 0.001"
        }

        g <- ggplot(data = subset(res_l, event != evlabs[which(events == state0)]), aes(x = time, y = prob)) +
                geom_step(aes(color = group, linetype = group), size = cuminc.size) +
                scale_linetype_manual(name = grname, values = ltys, labels = grlabs) +
                   scale_color_manual(name = grname, values = cols, labels = grlabs) +
                facet_wrap(~event, nrow = 1) +
                theme_bw() +
                theme(panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      strip.background = element_blank(),
                      legend.key.width=unit(2,"line")) +
                scale_x_continuous(breaks = xbrs, expand = c(0.01, 0), labels = xbrlabs) +
                coord_cartesian(xlim = xlim, ylim = ylim, clip = "on") +
                xlab(xlab) +
                ylab(ylab) +
                ggtitle(main)
        if (!perc) g <- g + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0))
        if ( perc) g <- g + scale_y_continuous(breaks = ybrs, expand = c(0.01, 0),
                                               labels = scales::percent_format(accuracy = 1))
        if (graystest) g <- g + geom_text(data = p, aes(x = x, y = y, label = pval), hjust = 0, vjust = 1)
        }
    }
    return(g)
}
