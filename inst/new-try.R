#' @examples
#' cols <- c("#FCAE91", "#FB6A4A", "#CB181D", "#BDD7E7", "#6BAED6", "#2171B5")
#' ggpar(vars = list("gear", "cyl", "gear"), data=mtcars) +
#' # method="hammock", text.angle=0, ratio=0.2) +
#'   scale_fill_manual(values=cols) + scale_colour_manual(values=cols) +
#'   theme_bw()
#' mtcars$cyl <- factor(mtcars$cyl, levels = c("8","6","4"))
#' mtcars$gear <- factor(mtcars$gear)
#' ggpar(list("gear", "cyl", "gear"), data=mtcars)
#' ggpar(list("cyl", "gear", "cyl"), data=mtcars)
ggpar <- function (data, vars, width = 0.25, alpha = 0.6, labels = TRUE, method = "parset", ...) {
  get_ribbons <- function(xpos, dx, dy) {
    dframe <- data.frame(dx = dx, dy = dy)

    dxy <- dframe %>% group_by(dx, dy) %>% tally()
    dxy$ypos <- sum(dxy$n) - cumsum(dxy$n)
    dxy$xpos <- xpos + width/2

    dyx <- dframe %>% group_by(dy, dx) %>% tally()
    dyx$ypos <- sum(dyx$n) - cumsum(dyx$n)
    dyx$xpos <- xpos + 1 - width/2

    dfm <- rbind(dxy, dyx)

    if (method == "parset") {
      gr <- geom_ribbon(aes(x=xpos,
                    ymin=ypos,
                    ymax= ypos+n, group=interaction(dx, dy),
                    fill=dx,
                    colour=dx), alpha = alpha, data = dfm)
    }
    if (method == "hammock") {
      gr <- geom_ribbon(aes(x=xpos,
                            ymin=ypos,
                            ymax= ypos+n, group=interaction(dx, dy),
                            fill=dx,
                            colour=dx), alpha = alpha, data = dfm)
    }

    gr
  }
  stopifnot(length(vars) >= 2)

  data_ <- data[,as.character(vars)]
  for (i in 1:length(vars)) {
    data_[,i] <- as.factor(data_[,i])
    levels(data_[,i]) <- paste(vars[[i]], levels(data_[,i]), sep=":")
  }
  data__ <- suppressWarnings(tidyr::gather(data_, factor_key = TRUE))
  bars <- list(geom_bar(data = data__, aes(x = key, color = value, fill=value),
                        width = width, ...),
    scale_x_discrete("", labels = as.character(vars)))

  ribbons <- list()
  for (i in 1:(length(vars)-1)) {
    ribbons[[i]]  <- get_ribbons(i, data_[,i], data_[,i+1])
  }

  label <- list()
  if (labels) {
    for (i in 1:(length(vars))) {
browser()
      dx <- data_%>% group_by_(vars[[i]]) %>% tally()

      dx$xpos <- i
      dx$ypos <- sum(dx$n) - cumsum(dx$n) + dx$n/2
      names(dx)[1] <- "key"
#      browser()
      dx <- dx %>% tidyr::separate(key, into=c("key", "value"), sep =":")
      label[[i]] <- list(
        geom_text(aes(x = xpos, y = ypos, label = value), colour = "grey10",
                  nudge_x = .01, nudge_y = 1/sum(dx$n), data = dx),
        geom_text(aes(x = xpos, y = ypos, label = value), colour = "grey90", data = dx))
    }
  }

  ggplot() +ribbons  + bars + label
}
