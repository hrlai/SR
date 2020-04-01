source("code/functions.R")


# Fig. 3 ------------------------------------------------------------------
w <- 5000  # number of timestep to let population reach equilibrium
pw <- 500  # number of post-warmup timesteps
fig3a <- fig3.func(D = 0, warmup = w, post.warmup = pw)
fig3b <- fig3.func(D = 0.01, warmup = w, post.warmup = pw)
fig3c <- fig3.func(D = 0.075, warmup = w, post.warmup = pw)
fig3d <- fig3.func(D = 0.15, warmup = w, post.warmup = pw)

png("figs/fig3.png", width = 3, height = 6, units = "in", res = 150)
par(mfrow = c(4, 1), mar = c(5,4,1.5,1))
plot(fig3a$times[-(1:w)], fig3a$N[-(1:w)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(a)")
plot(fig3b$times[-(1:w)], fig3b$N[-(1:w)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(b)")
plot(fig3c$times[-(1:w)], fig3c$N[-(1:w)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(c)")
# abline(v = 11 * 1:500, col = "gray")
plot(fig3d$times[-(1:w)], fig3d$N[-(1:w)], 
     type = "l", las = 1,
     ylim = c(0, 400), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Adult pop. size",
     main = "(d)")
dev.off()


png("figs/fig3_supp_env.png", width = 3, height = 6, units = "in", res = 150)
par(mfrow = c(4, 1), mar = c(5,4,1.5,1))
plot(fig3a$times[-(1:w)], fig3a$E[-(1:w)], 
     type = "l", las = 1,
     ylim = c(1.5, 2.5), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Environmental factor",
     main = "(a)")
plot(fig3b$times[-(1:w)], fig3b$E[-(1:w)], 
     type = "l", las = 1,
     ylim = c(1.5, 2.5), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Environmental factor",
     main = "(b)")
plot(fig3c$times[-(1:w)], fig3c$E[-(1:w)], 
     type = "l", las = 1,
     ylim = c(1.5, 2.5), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Environmental factor",
     main = "(c)")
plot(fig3d$times[-(1:w)], fig3d$E[-(1:w)], 
     type = "l", las = 1,
     ylim = c(1.5, 2.5), yaxs="i", xaxs="i",
     xlab = "Time",
     ylab = "Environmental factor",
     main = "(d)")
dev.off()





# Figure 4 ----------------------------------------------------------------

outbreaks <- extract.outbreak(D = 0.075, warmup = w, post.warmup = 100000)
outbreaks.index <- which(outbreaks > 0)
waiting.time <- diff(outbreaks.index)

png("figs/fig4a.png", width = 4, height = 4, units = "in", res = 150)
par(mar = c(5,4,1,1))
hist(waiting.time, 
     breaks = seq(1, max(waiting.time), 1),
     las = 1,
     xlim = c(0, 80), ylim = c(0, 250),
     xlab = "Waiting time", ylab = "Number of intervals",
     main = "",
     col = "black",
     xaxt = "n")
axis(1, at = c(11 * 1:7))
dev.off()
