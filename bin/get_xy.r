# Get xy coordinates using geom_text_repel
get_xy_repel <- function(df){

    library(ggplot2)
    library(ggrepel)

    p1 <- ggplot(data = df, aes(x = lon, y = lat)) + theme_bw() + 
    geom_text_repel(aes(label = label), 
                    box.padding = unit(1, "lines"), max.overlaps = Inf, force = 3, xlim = c(-Inf, Inf), ylim = c(-Inf, Inf)) +
    geom_point(size = 2)

    ggp1 <- ggplot_build(p1)
    # Get x and y plot ranges 
    xrg <- ggp1$layout$panel_params[[1]]$x.range
    yrg <- ggp1$layout$panel_params[[1]]$y.range

    library(grid)
    {grid.force()
    kids <- childNames(grid.get("textrepel", grep = TRUE))}

    labs  <- kids[str_detect(kids,"textrepel")]

    # Function: get the x and y positions of a single ggrepel label
    get.xy.pos.labs <- function(n) {
    grb <- grid.get(n)
    data.frame(
    x = xrg[1]+diff(xrg)*convertX(grb$x, "native", valueOnly = TRUE),
    y = yrg[1]+diff(yrg)*convertY(grb$y, "native", valueOnly = TRUE)
    )
    }

    # Get positions of all ggrepel labels
    dts <- do.call(rbind, lapply(labs, get.xy.pos.labs))
    dts$lab <- df$label

    return(dts)
}
