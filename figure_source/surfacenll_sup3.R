# sup 3 surface -----------------------------------------------------------
## surface ##
name <- "basic"
p1 <- surface[[name]] %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll), interpolate = TRUE) +
  geom_contour(colour = "black", binwidth = 5) +
  metR::geom_text_contour(aes(z = nll), rotate = F, min.size = 10)+
  theme_publication +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"),
                       limits = gg$limit.nll) +
  coord_flip()+
  theme(legend.position = "none")+
  geom_point(data = (pubmodel[[name]] %>% filter(tag == hero)), 
             aes(x = par.alpha, y = par.beta, z = NA), colour = "black")

name <- "fictitious"
p2 <- surface[[name]] %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll), interpolate = TRUE) +
  geom_contour(colour = "black", binwidth = 5) +
  metR::geom_text_contour(aes(z = nll), rotate = F, min.size = 10)+
  theme_publication +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"), limits = gg$limit.nll) +
  coord_flip()+
  theme(legend.position = "none")+
  geom_point(data = (pubmodel[[name]] %>% filter(tag == hero)), 
             aes(x = par.alpha, y = par.beta, z = NA), colour = "black")


# lattice::wireframe(nll ~ alpha*beta, data = surface$basic,
#           xlab = "alpha", ylab = "beta",
#           #col.groups=c("grey"),
#           alpha = .5,
#           par.settings = list(axis.line = list(col = "transparent"), 
#                               alpha = 0.7),
#           col.groups=c(rgb(red=255,green=153,blue=102,
#                            alpha=200,maxColorValue=255)),  # Orange
#           #drape = TRUE,
#           scales = list(arrows=FALSE, col="black"),
#           screen = list(z = -45, x = -45))
# 
# persp(temp,xlab = "alpha", ylab = "beta", theta = c(-45, -45), phi = 30, 
# col = c("gray"), axes = TRUE, ticktype = "detailed", border = NA, 
# shade = .7, ltheta = c(.5,.5))
# plot3D::scatter3D(x = surface$basic$alpha, y = surface$basic$beta, z = surface$basic$nll,
#                   pch = 18, col = ramp.col(col = c("lightgrey", "black")))
# 
# plotly::plot_ly() %>% add_surface(z~temp, x = as.numeric(colnames(temp)[-1]),
#                                   y = temp[,1])
# 
# plot_ly() %>% 
#   add_contour(z~temp, coloraxis = 'coloraxis', 
#               contours = list(showlabels = TRUE))