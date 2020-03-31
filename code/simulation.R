source("code/functions.R")


# Fig. 3 ------------------------------------------------------------------

fig3a <- fig3.func(D = 0)
fig3b <- fig3.func(D = 0.01)
fig3c <- fig3.func(D = 0.075)
fig3d <- fig3.func(D = 0.15)

png("figs/fig3.png", width = 3, height = 6, units = "in", res = 600)
par(mfrow = c(4, 1), mar = c(5,4,1.5,1))
plot(fig3a$times[-(1:warmup)], fig3a$N[-(1:warmup)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(a)")
plot(fig3b$times[-(1:warmup)], fig3b$N[-(1:warmup)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(b)")
plot(fig3c$times[-(1:warmup)], fig3c$N[-(1:warmup)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(c)")
plot(fig3d$times[-(1:warmup)], fig3d$N[-(1:warmup)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(d)")
dev.off()