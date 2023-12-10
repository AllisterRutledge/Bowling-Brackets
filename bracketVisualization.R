library(ggplot2)

players <- 8
games <- log2(players)
bracket <- data.frame()

nameplaces <- data.frame(x = 1,
                         y = seq(1, players, 1),
                         xend = 2,
                         yend = seq(1, players, 1),
                         game = 1,
                         lineType = "name")

bracket <- rbind(bracket, nameplaces)


for (g in 2:games){
 matchlink <- data.frame(x = g,
                         y = nameplaces$y,
                         xend = g,
                         yend = nameplaces$y + c(0.5, -0.5) * 2^(g-2),
                         game = g,
                         lineType = "link")
 
 nameplaces <- data.frame(x = g,
                          y = unique(matchlink$yend),
                          xend = g + 1,
                          yend = unique(matchlink$yend),
                          game = g,
                          lineType = "name")
 
 bracket <- rbind(bracket, nameplaces, matchlink)
 
}

bracket$player[bracket$lineType == "name"] <- " 225 James Allister Rutledge"
bracket$winner <- c(0,2)

bracket |> ggplot(aes(y = y, x = x)) +
 geom_segment(aes(yend = yend, xend = xend)) +
 geom_text(aes(label = player, hjust = -0.01, vjust = -0.5, fontface = winner),
           size = 4, #TODO: text size ratio and aspect ratio of image
           na.rm = TRUE) +
 labs(title = " ") +
 theme_void()

# data.frame(players = c(4,8,16,32),
#            textsize = c(5,4,3,2)) |>
#  ggplot(aes(players, textsize)) +
#  geom_point()
