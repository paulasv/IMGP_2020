
### Fishing mortality
## Background colour
df <- expand.grid(gamma = seq(0.01, 1.8, length = 300),
                  delta = seq(0.01, 1.8, length =300))
df$y <- with(df, gamma/delta - 1)

points <-  c("61", "11", "28", "80", "39", "51", "91", "2", "15", "40")

datapoints <- datap%>%
  filter(!lab %in% points)

datalabs <- datap%>%
  filter(lab %in% points)

t1 <- ggplot() +
  geom_tile(data = df, aes(x = delta, y = gamma, fill = y)) +
  geom_hline(yintercept = 1, linetype = 3,colour = "gray")+
  geom_vline(xintercept = 1, linetype = 3,colour = "gray")+
  geom_abline(intercept = 0, slope = 1, linetype = 3, colour = "gray")+
  geom_point(data = datapoints, aes( x = delta, y = gamma), size = .2, alpha = 0.6)+
  scale_fill_gradient2(low = "#0072B2",  mid = "white", high = "#D55E00", limits =c(-3.2,3.2), na.value = "#D55E00" ,  labels = scales::percent) +
  geom_text(data = datalabs, aes(x = delta, y = gamma, label = lab), check_overlap = FALSE, size = 3)+
  #geom_text_repel(data = datap, aes(x = delta, y = gamma, label = lab), size = 2.5, box.padding = unit(0.06, "lines"))+
  #geom_label(data = datap, aes(x = delta, y = gamma, label = lab), colour = "black", size = 2.5, cex = .7)+
  scale_color_manual(values = c("black", "gray41"))+
  theme(legend.position = "")+
  scale_y_continuous(breaks = c(0, 1,1.5))+
  xlim(0,2)+ylim(0,1.5)+
  labs(x = expression(paste(delta, " -Proportional change in F"[MSY])), y = expression(paste(gamma, " -Proportional change in average F")), fill = "Proportional\nchange in\nstatus", title = "a2")

cor.test(datap$delta, datap$gamma,method = "pearson")

c1 <- ggplot() +
  geom_tile(data = df, aes(x = delta, y = gamma, fill = y)) +
  geom_hline(yintercept = 1, linetype = 3,colour = "gray")+
  geom_vline(xintercept = 1, linetype = 3,colour = "gray")+
  geom_abline(intercept = 0, slope = 1, linetype = 3, colour = "gray")+
  geom_point(data = datapoints, aes( x = delta_c, y = gamma_c), size = .2, alpha = 0.6)+
  scale_fill_gradient2(low = "#0072B2",  mid = "white", high = "#D55E00", limits =c(-3.2,3.2), na.value = "#D55E00" ,  labels = scales::percent) +
  geom_text(data = datalabs, aes(x = delta_c, y = gamma_c, label = lab), check_overlap = FALSE, size = 3)+
  scale_color_manual(values = c("black", "gray41"))+
  theme(legend.position = "")+
  labs(x = expression(paste(delta, " -Proportional change in F"[MSY])), y = expression(paste(gamma, " -Proportional change in average F")), fill = "Proportional\nchange in\nstatus", title = "a1")

cor.test(datap$delta_c, datap$gamma_c,method = "pearson")


### Biomass
## Background colour
df <- expand.grid(gamma = seq(0.01, 2.5, length = 300),
                  delta = seq(0.01, 2.5, length =300))
df$y <- with(df, gamma/delta - 1)

points <-  c("61", "11", "28", "80", "63", "39", "51", "91", "2", "15", "40")

datapoints <- datap%>%
  filter(!lab %in% points)

datalabs <- datap%>%
  filter(lab %in% points)

t2 <- ggplot() +
  geom_tile(data = df, aes(x = delta, y = gamma, fill = y)) +
  geom_hline(yintercept = 1, linetype = 3,colour = "gray")+
  geom_vline(xintercept = 1, linetype = 3,colour = "gray")+
  geom_abline(intercept = 0, slope = 1, linetype = 3, colour = "gray")+
  geom_point(data = datapoints, aes( x = delta_b, y = gamma_b), size = .2, alpha = 0.6)+
  scale_fill_gradient2(low = "#0072B2",  mid = "white", high = "#D55E00", limits =c(-3.2,3.2), na.value = "#D55E00" ,  labels = scales::percent) +
  geom_text(data = datalabs, aes(x = delta_b, y = gamma_b, label = lab), check_overlap = TRUE,  size = 3)+
  scale_color_manual(values = c("black", "gray41"))+
  theme(legend.position = "")+
  labs(x = expression(paste(delta, " -Proportional change in MSYB"[trigger])), y = expression(paste(gamma, " -Proportional change in average SSB")), fill = "Proportional\nchange in\nstatus", title = "b2")

cor.test(datap$delta_b, datap$gamma_b,method = "pearson")

c2 <- ggplot() +
  geom_tile(data = df, aes(x = delta, y = gamma, fill = y)) +
  geom_hline(yintercept = 1, linetype = 3,colour = "gray")+
  geom_vline(xintercept = 1, linetype = 3,colour = "gray")+
  geom_abline(intercept = 0, slope = 1, linetype = 3, colour = "gray")+
  geom_point(data = datapoints, aes( x = delta_b_c, y = gamma_b_c), size = .2, alpha = 0.6)+
  scale_fill_gradient2(low = "#0072B2",  mid = "white", high = "#D55E00", limits =c(-3.2,3.2), na.value = "#D55E00" ,  labels = scales::percent) +
  geom_text(data = datalabs, aes(x = delta_b_c, y = gamma_b_c, label = lab), check_overlap = FALSE,  size = 3)+
  scale_color_manual(values = c("black", "gray41"))+
  theme(legend.position = "")+
  labs(x = expression(paste(delta, " -Proportional change in MSYB"[trigger])), y = expression(paste(gamma, " -Proportional change in average SSB")), fill = "Proportional\nchange in\nstatus", title = "b1")

cor.test(datap$delta_b_c, datap$gamma_b_c,method = "pearson")

png("fig4_new.png", width = 3280, height = 2580, res = 300)
ggarrange(c1, t1, c2, t2,  ncol = 2, nrow = 2, common.legend = TRUE, legend = "right")
dev.off()


# Marginal relationship between average proportional change in status and proportional change in reference point (Fig. 5)

expected <- data.frame (d = seq(0.2, 2.5, by  = 0.01), g =1/seq(0.2, 2.5, by  = 0.01)-1)
#scale_x_log10()+

efe <- ggplot(data = datapoints, aes(delta, y)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(d, g), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_log10()+
  #scale_y_continuous(labels = scales::percent)+
  geom_text(data = datalabs, aes(label = lab, fontface = 2), color = "black", check_overlap = TRUE, alpha = 0.6, size = 3)+
  labs( y = expression("Change in F/F"[MSY]), x =expression(paste(delta, " -Proportional change in F"[MSY])), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())


ebe <- ggplot(data = datapoints, aes(delta_b, y_b)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(d, g), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  geom_text(data = datalabs, aes(label = lab, fontface = 2), color = "black", check_overlap = TRUE, alpha = 0.6, size = 3)+
  labs( y = expression("Change in SSB/MSYB"[trigger]), x =expression(paste(delta, " -Proportional change in MSYB"[trigger])), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())

ggarrange(efe, ebe, nrow = 1, common.legend = TRUE, legend = "right")


expected <- data.frame (ga = seq(0.5, 1.65, by  = 0.01), de = seq(0.5, 1.65, by  = 0.01)/1-1)

efeg <- ggplot(data = datapoints, aes(gamma, y)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(ga, de), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  geom_text(data = datalabs, aes(label = lab, fontface = 2), color = "black", check_overlap = TRUE, alpha = 0.6, size = 3)+
  labs(y = expression("Change in F/F"[MSY]), x =expression(paste(gamma, " -Proportional change in F")), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())

ebeg <- ggplot(data = datapoints, aes(gamma_b, y_b)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data =  expected, aes(ga, de), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  geom_text(data = datalabs, aes(label = lab, fontface = 2), color = "black", check_overlap = TRUE, alpha = 0.6, size = 3)+
  labs(y = expression("Change in SSB/MSYB"[trigger]), x =expression(paste(gamma, " -Proportional change in SSB")), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())

png("fig5_new.png", width = 3280, height = 2580, res = 300)
ggarrange(efe, ebe, efeg, ebeg, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right")
dev.off()
