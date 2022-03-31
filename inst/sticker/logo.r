rstudioapi::restartSession()

library(hexSticker)
library(sysfonts)
library(TestDesign)

font_add_google("Roboto Mono")

p <- TestDesign::array_p_1pl(matrix(seq(-3, 3, 0.2), , 1), b = 0)

s <- sticker(
  ~{
    plot(
      0, 0, xlab = NA, ylab = NA,
      xlim = c(0.5, 3.5),
      ylim = c(4.5, -0.5),
      bty = "n", axes = FALSE, type = "n"
    )
    lines(
      seq(-1.5, 5.5, length.out = 31),
      (1 - p) * 5 - 0.5,
      lwd = 4,
      col = "#333399"
    )
    b <- matrix(TRUE, 3, 3)
    b[2:3, 1] <- FALSE
    for (x in 1:3) {
      for (y in 1:3) {
        cell_color <- "#000080"
        if (x == 1 & y == 1) cell_color <- "#00FF40"
        if (x == 2 & y == 3) cell_color <- "#FF0040"
        if (x == 3 & y == 2) cell_color <- "#00FFFF"
        if (b[y, x]) {
          rect(
            x - 0.35, y - 0.3, x + 0.35, y + 0.3,
            lwd = 2,
            border = "white",
            ljoin = x - 1,
            lend = y - 1,
            lmitre = 99999,
            col = cell_color
          )
        }
      }
    }
  },
  h_fill = "#000040",
  h_color = "#000040",
  package = "TestDesign",
  p_color = "#ffffff",
  p_size = 60,
  p_family = "Roboto Mono",
  p_fontface = "bold",
  p_y = 0.6,
  s_x = 1.0, s_y = 1.0,
  s_width = 1.0, s_height = 1.0,
  filename = "inst/figures/testdesign_logo.png",
  dpi = 1024
)
plot(s)
