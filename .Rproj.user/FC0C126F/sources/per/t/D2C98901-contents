
Diameter boxplot
```{r warning=FALSE}

gran = read_xlsx('sources/Гран состав.xlsx')
names(gran)[1] = 'name'
names(gran)[5] = '0.175'
names(gran)[6] = '0.075'
N = gran$name


gran = gran %>% reshape2::melt() %>% mutate(variable = as.numeric(as.character(variable)))

g = ggplot(data = gran, aes(y = value, x = variable, col = name))+ geom_point() + 
  #geom_line() + 
  #geom_smooth(se = F) +
  #geom_smooth(se = F, method = lm, formula = y ~ poly(x, 6))+
  #geom_spline(nknots = 9) +
  #geom_line(data=data.frame(spline(gran, n = n*10))) + 
  ggalt::geom_xspline(spline_shape = 0.2) + 
  #geom_smooth(stat = "smooth", method = "lm", se = FALSE, lwd=, formula = 'y~x')+
  #geom_xspline3(shape = -.25) +
  scale_colour_manual(
    
    values = c(
      
      "1Б"="#f95ddf"
      ,"2А"="#e859d8"
      ,"2Б"="#d656d0"
      ,"3А"="#c652c8"
      ,"3Б"="#b54ec0"
      ,"4А"="#a54ab7"
      ,"4Б"="#9546ae"
      ,"5А"="#8641a4"
      ,"5Б"="#773d9b"
      ,"6А"="#693890"
      ,"7А"="#5b3486"
      ,"7Б"="#4d2f7b"
      ,"8А"="#402a71"
      ,"8Б"="#342466"
      ,"9А"="#281f5b"
      ,"9Б"="#1d1a50"
      ,"10А"="#111545"
      ,"10Б"="#070e3a"
      ,"1"="#5cff5c"
      ,"2"="#44ea5e"
      ,"3"="#2cd55f"
      ,"4"="#12c05d"
      ,"5"="#00ab5a"
      ,"6"="#009756"
      ,"7"="#008350"
      ,"8"="#006f48"
      
    )
    
    ,breaks = N
    , labels = N
    
  ) +
  xlab('Диаметр, мм') + ylab('Доля данной фракции в пробе') + ylim(c(0, 0.7)) + theme_bw()

g = g + scale_x_continuous(trans = "log10") 
#ggsave(plot = g , filename = 'out/g5.png', width = 12, height = 9)
```

```{r}
GeomXSpline3 <- ggproto("GeomXSpline3", Geom,
                        required_aes = c("x", "y"),
                        default_aes = aes(colour = "black", shape=-1, open=T),
                        draw_key = draw_key_point,
                        
                        draw_panel = function(data, panel_params, coord) {
                          coords <- coord$transform(data, panel_params)
                          grid::xsplineGrob(
                            coords$x, coords$y,
                            shape = coords$shape, 
                            open = coords$open[1],
                            gp = grid::gpar(col = coords$col)
                          )
                        }
)

geom_xspline3 <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    geom = GeomXSpline3, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Plot with ggarrange
myplot = ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_xspline3(shape = -.25) + geom_point()
ggpubr::ggarrange(myplot, myplot) 

```






multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
