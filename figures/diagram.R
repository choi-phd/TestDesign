library(diagram)

names <- c("Item pool\nloadItemPool()",
           "Item attributes\nloadItemAttrib()",
           "Stimulus attributes\nloadStAttrib()",
           "Constraints\nloadConstraints()",
           "Fixed-form Assembly\nStatic()",
           "Optimal Test Assembly",
           "Adaptive Assembly\nShadow()",
           "Maximum information\nTarget TIF\nTarget TCC",
           "Multiple-form Assembly\nSplit()",
           "Linear-on-the-fly\nMST on the fly\nFully adaptive\nHybrid")


par(mar = rep(0.5, 4), family = "mono", font = 2)

k = length(names)
M = matrix(nrow = k, ncol = k, byrow = TRUE, data = 0)
colnames(M) = names
rownames(M) = names

M[2, 1] = ""
M[4, 2] = ""
M[4, 3] = ""

M[6, 4] = ""
M[5, 6] = ""
M[7, 6] = ""

M[8, 5] = ""
M[9, 6] = ""
M[10, 7] = ""

arr = matrix(nrow = k, ncol = k, byrow = TRUE, data = .25)
arr[8, 5] = 0
arr[10, 7] = 0

coord = coordinates(pos = c(1, 2, 1, 3, 3))
coord[2, 1] = 0.5

setEPS()
postscript("figures/diagram.eps", width = 12, height = 8, family = "Helvetica")

plotmat(M, coord, name = names, lwd = 1, curve = 0,
        box.lwd = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2), cex.txt = 0.8,
        box.size = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.125, 0.1),
        box.type = 'rect',
        box.prop = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.5, 0.3, 0.5),
        box.col = c('white', 'white', 'white', 'white', 'white', 'grey75', 'white', 'white', 'white', 'white'),
        box.lcol = c('black', 'black', 'black', 'black', 'black', 'black', 'black', 'black', 'black', 'black'),
        txt.col = c('black', 'black', 'black', 'black', 'black', 'black', 'black'),
        shadow.col = c('grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey50', 'grey75'),
        shadow.size = c(0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005),
        arr.length = arr,
        arr.width = arr, arr.type = "triangle", arr.pos = .5,
        font = 2)

dev.off()
