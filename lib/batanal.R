# batanal.R --- analyse battery statistics from batlog.sh

d <- read.csv(file = "~/battery-log-002.csv",
              header = T,
              comment.char = "#")

d$date_time    <- as.POSIXct(d$unix_time, origin = "1970-01-01")
d$percent_full <- d$power_supply_energy_now / d$power_supply_energy_full * 100

# Group observations into moments, i.e. continous series of
# observations that arent deparated by an interval greater than five
# minutes.
d$moment <- c(0, cumsum(diff(d$date_time) > 5))

moments   <- unique(d$moment)
n.moments <- length(moments) # number of moments

png(filename = "battery.png",
    height = (1300 * n.moments) + 500,
    width = 3000)

colour.fg    <- "white"
colour.bg    <- "#222222"
colours.grid <- "#666666"
colours.lda  <- c("yellow", "limegreen", "cyan")
text.size    <- 4
par(mfrow = c(n.moments, 1),
    bg = colour.bg, fg = colour.fg,
    col.main = colour.fg,
    col.lab = colour.fg,
    col.sub = colour.fg,
    col = colour.fg,
    cex = text.size)

for (m in moments) {
    d.m <- d[d$moment == m, ]
    par(mar=c(5, 4, 4, 4) + 0.3)
    # Battery levels (% of usable capacity).
    plot(d.m$date_time, d.m$percent_full,
         main = sprintf("moment %s", m),
         ylab = "battery %",
         xlab = "",
         xaxt = "n",
         type = "l",
         col.axis = colour.fg)
    grid(col = colours.grid)
    # Show legend only in the initial graph.
    if (m == 0) {
        legend(x      = "topright",
               title  = "Legend",
               legend = c("battery level", "lda1", "lda2", "lda3"),
               lty    = c(1, 1, 1, 1),
               col    = c(colour.fg, colours.lda))
    }
    # Load averages.
    for(avg.n in 0:2) {
        par(new = T)
        plot(d.m$date_time,
             d.m[[sprintf("load_%d", avg.n)]],
             col = colours.lda[avg.n+1],
             ylab = "",
             xlab = "",
             xaxt = "n",
             type = "l",
             axes = F,
             add = T)
    }
    axis.POSIXct(1, x = d.m$date_time, format = "%a %H:%M",
                 col.axis = colour.fg)
    # Manually set up the axis for load averages.
    d.m$max_load <- mapply(max, d.m$load_0, d.m$load_1, d.m$load_2)
    xpos.load    <- seq(0, max(d.m$max_load), by = 0.2)
    axis(side = 4, at = xpos.load, col.axis = colour.fg)
    mtext("load averages", side = 4, line = 2.5, cex = text.size)
}

mtext("Battery levels and load averages",
      side = 3,
      line = -1.4,
      outer = T,
      cex = text.size + 1)

mtext("Time (Day HH:MM)",
      side = 1,
      outer = T,
      line = -2.4,
      cex = text.size)

# Writes the file.
dev.off()
