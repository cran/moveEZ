#' Move plot
#'
#' Create animated biplot on samples in a biplot
#'
#' @param bp biplot object from biplotEZ
#' @param time.var time variable
#' @param group.var group variable
#' @param move whether to animate (TRUE) or facet (FALSE) samples, according to time.var
#' @param hulls whether to display sample points or convex hulls
#' @param scale.var scaling the vectors representing the variables
#' @param shadow whether the animation will keep past states (only when hulls = FALSE)
#'
#' @returns
#' \item{bp}{Returns the elements of the biplot object \code{bp} from \code{biplotEZ}.}
#' \item{plot}{An animated or a facet of biplots based on the dynamic frame.}
#'
#' @export
#'
#' @examples
#' data(Africa_climate)
#' bp <- biplotEZ::biplot(Africa_climate, scaled = TRUE) |> biplotEZ::PCA()
#' bp |> moveplot(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE)
#' bp |> moveplot(time.var = "Year", group.var = "Region", hulls = FALSE, move = FALSE)
#' \donttest{
#' if(interactive()) {
#' bp |> moveplot(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE)}}
moveplot <- function(bp, time.var, group.var, move = TRUE, hulls = TRUE,
                     scale.var = 5,shadow=FALSE)
{

  if(!is.null(group.var)) bp$group.aes <- bp$raw.X[,which(colnames(bp$raw.X) == group.var)] else
    bp$group.aes = NULL

  tvi <- which(colnames(bp$raw.X) == time.var)
  gvi <- which(colnames(bp$raw.X) == group.var)

  iterations <- nlevels(bp$raw.X[[tvi]])
  iter_levels <- levels(bp$raw.X[[tvi]])

  group_levels <- levels(bp$raw.X[[gvi]])


  # Samples
  Z <- bp$Z
  Z <- suppressMessages(dplyr::bind_cols(Z, bp$Xcat))
  colnames(Z)[1:2] <- c("V1","V2")
  Z_tbl <- dplyr::as_tibble(Z)

  axes_info <- axes_moveEZ(bp)
  Vr <- bp$Vr
  colnames(Vr) <- c("V1","V2")
  Vr <- dplyr::as_tibble(Vr)
  Vr_tbl <- Vr |> dplyr::mutate(var = colnames(bp$X)) |>
    dplyr::mutate(slope = sign(axes_info$slope)) |>
    dplyr::mutate(hadj = -slope, vadj = -1)


  # C Hulls for points
  chull_reg <- vector("list", iterations)
  for(i in 1:iterations)
  {
    idx <- which(bp$raw.X[[tvi]] == iter_levels[i])
    Y <- Z[idx,]
    chull_reg_iter <- vector("list", length(group_levels))
    for(j in 1:length(group_levels))
    {
      temp <- which(Y[[group.var]] == group_levels[j]) # index of the group var
      chull_reg_iter[[j]] <- Y[temp,][grDevices::chull(Y[temp,]),]
      chull_reg[[i]][[j]] <- chull_reg_iter[[j]]
    }
    chull_reg[[i]] <- do.call(rbind,chull_reg[[i]])
  }

  chull_reg <- do.call(rbind,chull_reg)
  chull_reg <- dplyr::as_tibble(chull_reg)


  # Plotting

  # move – TRUE --- Animated sliced Z
  # move – FALSE  --- Facet on sliced Z
  bp$plot <- ggplot() +
      # Axes
      geom_segment(data=Vr_tbl,aes(x=0,y=0,xend=V1*scale.var,yend=V2*scale.var,group=var),
                   arrow=arrow(length=unit(0.1,"inches"))) +
      geom_text(data=Vr_tbl,aes(x=V1*scale.var, y=V2*scale.var,
                                label = var,
                                hjust="outward", vjust="outward",group=var),colour="black",size=4) +
      # Sample polygons or points
      {if(hulls){
        geom_polygon(data = chull_reg,
                     aes(x=V1, y=V2,group = .data[[group.var]],
                         fill = .data[[group.var]]), alpha=0.5)

      } else {
        geom_point(data = Z_tbl,
                   aes(x=V1, y=V2,
                       group = .data[[group.var]],
                       fill =.data[[group.var]],
                       colour = .data[[group.var]]),size=2, alpha=0.8)
      }} +
      {if(move) { gganimate::transition_states(.data[[time.var]],
                                     transition_length = 2,
                                     state_length = 1) } else {
                                       facet_wrap(~.data[[time.var]]) }} +
      {if(move) { labs(title = '{time.var}: {closest_state}',x="",y="")}} +
      {if(!hulls & shadow) { gganimate::shadow_mark(alpha=0.3) }} +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      theme_classic() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = ggplot2::element_text(size=30,face ="bold") )

  if(move==TRUE)
    print(gganimate::animate(bp$plot,duration = 15,fps=10)) else
      print(bp$plot)
  bp
}


#' Move plot 2
#'
#' Create animated biplot on samples and variables in a biplot
#'
#' @param bp biplot object from biplotEZ
#' @param time.var time variable
#' @param group.var group variable
#' @param move whether to animate (TRUE) or facet (FALSE) samples and variables, according to time.var
#' @param hulls whether to display sample points or convex hulls
#' @param scale.var scaling the vectors representing the variables
#' @param align.time a vector specifying the levels of time.var for which the biplots should be aligned. Only biplots corresponding to these time points will be used to compute the alignment transformation.
#' @param reflect a character vector specifying the axis of reflection to apply at each corresponding time point in align.time. One of FALSE (default), "x" for reflection about the x-axis, "y" for reflection about the y-axis and "xy" for reflection about both axes.
#'
#' @returns
#' \item{bp}{Returns the elements of the biplot object \code{bp} from \code{biplotEZ}.}
#' \item{plot}{An animated or a facet of biplots based on the dynamic frame.}
#'
#' @export
#'
#' @examples
#' data(Africa_climate)
#' bp <- biplotEZ::biplot(Africa_climate, scaled = TRUE) |> biplotEZ::PCA()
#' \donttest{
#' if(interactive()) {
#' bp |> moveplot2(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE)}}
moveplot2 <- function(bp, time.var, group.var, move = TRUE, hulls = TRUE, scale.var = 5,
                      align.time = NA, reflect = NA)
{

  if(!is.null(group.var)) bp$group.aes <- bp$raw.X[,which(colnames(bp$raw.X) == group.var)] else
    bp$group.aes = NULL

  tvi <- which(colnames(bp$raw.X) == time.var)
  gvi <- which(colnames(bp$raw.X) == group.var)

  iterations <- nlevels(bp$raw.X[[tvi]])
  iter_levels <- levels(bp$raw.X[[tvi]])

  group_levels <- levels(bp$raw.X[[gvi]])

  align_levels <- which(iter_levels==align.time)

  # Samples
  Z <- bp$Z
  Z <- suppressMessages(dplyr::bind_cols(Z, bp$Xcat))
  colnames(Z)[1:2] <- c("V1","V2")
  Z_tbl <- dplyr::as_tibble(Z)

  # Set limits
  # xlim
  minx <- min(Z_tbl$V1)
  maxx <- max(Z_tbl$V1)
  range_x <- maxx - minx

  # ylim
  miny <- min(Z_tbl$V2)
  maxy <- max(Z_tbl$V2)
  range_y <- maxy - miny

  perc <- 20/100
  xlim <- c(minx - perc*range_x,maxx + perc*range_x)
  ylim <- c(miny - perc*range_y,maxy + perc*range_y)


  # Basis extraction
  bp_list <- vector("list", iterations)
  axes_info <- vector("list", iterations)
  Z_list <- vector("list", iterations)
  Vr_list <- vector("list", iterations)
  chull_reg <- vector("list", iterations)

  for (i in 1:iterations)
  {
    # Filter data by custom years

    temp <- bp$raw.X |> dplyr::filter(bp$raw.X[[tvi]] == iter_levels[i])
    bp_list[[i]] <- biplotEZ::biplot(temp,scaled=bp$scaled) |> biplotEZ::PCA(group.aes = temp[[gvi]])

    # if(i == any(align_levels)) bp_list[[i]] <- bp_list[[i]] |> reflect_biplot(reflect.axis = reflect[i])

    if (i %in% align_levels) bp_list[[i]] <- bp_list[[i]] |> biplotEZ::reflect(reflect.axis = reflect[which(align_levels == i)])

    colnames(bp_list[[i]]$Z) <- c("V1","V2")
    Z_list[[i]] <- dplyr::as_tibble(bp_list[[i]]$Z)
    Z_list[[i]] <- suppressMessages(dplyr::bind_cols(Z_list[[i]], bp_list[[i]]$Xcat))

    axes_info[[i]] <- axes_moveEZ(bp_list[[i]])
    colnames(bp_list[[i]]$Vr) <- c("V1","V2")
    Vr_list[[i]] <- dplyr::as_tibble(bp_list[[i]]$Vr)
    Vr_list[[i]] <- Vr_list[[i]] |> dplyr::mutate(var = colnames(bp$X)) |>
      dplyr::mutate(slope = sign(axes_info[[i]]$slope)) |>
      dplyr::mutate(hadj = -slope, vadj = -1) |>
      dplyr::mutate(time.var = iter_levels[i])


    #idx <- which(temp[[tvi]] == iter_levels[i])
    Y <- Z_list[[i]] #[idx,]
    chull_reg_iter <- vector("list", length(group_levels))
    for(j in 1:length(group_levels))
    {
      temp2 <- which(Y[[group.var]] == group_levels[j]) # index of the group var
      chull_reg_iter[[j]] <- Y[temp2,][grDevices::chull(Y[temp2,]),]
      #chull_reg[[i]][[j]] <- chull_reg_iter[[j]]
    }
    chull_reg[[i]] <- do.call(rbind,chull_reg_iter)

  }

    Z_tbl <- do.call(rbind,Z_list)
    Vr_tbl <- do.call(rbind,Vr_list)
    names(Vr_tbl)[7] <- time.var
    chull_reg <- do.call(rbind,chull_reg)
    chull_reg <- dplyr::as_tibble(chull_reg)

  # Plotting

  # Move – TRUE Animated separate Z,V
  # Move – FALSE Facet separate Z,V
  if(move==TRUE)
  {
    bp$plot <- ggplot() +
      # Axes
      geom_segment(data=Vr_tbl,aes(x=0,y=0,xend=V1*scale.var,yend=V2*scale.var,group=var),
                   arrow=arrow(length=unit(0.1,"inches"))) +
      geom_text(data=Vr_tbl,aes(x=V1*scale.var, y=V2*scale.var,
                                label = var,
                                hjust="outward", vjust="outward",group=var),colour="black",size=4) +
      gganimate::transition_states(.data[[time.var]],
                        transition_length = 2,
                        state_length = 1) +
      # Sample polygons or points
      {if(hulls){
        geom_polygon(data = chull_reg,
                              aes(x=V1, y=V2,group = .data[[group.var]],
                         fill = .data[[group.var]]), alpha=0.5)

      } else {
        geom_point(data = Z_tbl,
                            aes(x=V1, y=V2,
                       group = .data[[group.var]],
                       fill =.data[[group.var]],
                       colour = .data[[group.var]]),size=2, alpha=0.8)
      }} +
      gganimate::transition_states(.data[[time.var]],
                        transition_length = 2,
                        state_length = 1) +
      labs(title = '{time.var}: {closest_state}',x="",y="") +
      #xlim(xlim) +
      #ylim(ylim) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      theme_classic() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = ggplot2::element_text(size=30,face ="bold"))

  } else {

    bp$plot <- ggplot() +
      # Axes
      geom_segment(data=Vr_tbl,aes(x=0,y=0,xend=V1*scale.var,yend=V2*scale.var,group=var),
                   arrow=arrow(length=unit(0.1,"inches"))) +
      geom_text(data=Vr_tbl,aes(x=V1*scale.var, y=V2*scale.var,
                                label = var,
                                hjust="outward", vjust="outward",group=var),colour="black",size=4) +
      # Sample polygons or points
      {if(hulls){
        geom_polygon(data = chull_reg,
                     aes(x=V1, y=V2,group = .data[[group.var]],
                         fill = .data[[group.var]]), alpha=0.5)

      } else {
        geom_point(data = Z_tbl,
                   aes(x=V1, y=V2,
                       group = .data[[group.var]],
                       fill =.data[[group.var]],
                       colour = .data[[group.var]]),size=2, alpha=0.8)
      }} + facet_wrap(~.data[[time.var]]) +
      #xlim(xlim) +
      #ylim(ylim) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      theme_classic() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())

  }

  if(move==TRUE)
    print(gganimate::animate(bp$plot,duration = 15,fps=10)) else
      print(bp$plot)
  bp
}

#' Move plot 3
#'
#' Create animated biplot on samples and variables in a biplot with a given target
#'
#' @param bp biplot object from biplotEZ
#' @param time.var time variable
#' @param group.var group variable
#' @param move whether to animate (TRUE) or facet (FALSE) samples and variables, according to time.var
#' @param hulls whether to display sample points or convex hulls
#' @param scale.var scaling the vectors representing the variables
#' @param target Target data set to which all biplots should be matched consisting of the the same dimensions. If not specified, the centroid of all available biplot sample coordinates from \code{time.var} will be used. Default `NULL`.
#'
#' @returns
#' \item{bp}{Returns the elements of the biplot object \code{bp} from \code{biplotEZ}.}
#' \item{iter_levels}{The levels of the time variable.}
#' \item{coord_set}{The coordinates of the configurations before applying Generalised Orthogonal Procrustes Analysis.}
#' \item{GPA_list}{The coordinates of the configurations after applying Generalised Orthogonal Procrustes Analysis.}
#' \item{plot}{An animated or a facet of biplots based on the dynamic frame.}
#'
#' @export
#'
#' @examples
#' data(Africa_climate)
#' data(Africa_climate_target)
#' bp <- biplotEZ::biplot(Africa_climate, scaled = TRUE) |> biplotEZ::PCA()
#' bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE,
#' move = FALSE, target = NULL)
#' \donttest{
#' if(interactive()) {
#' bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE,
#' move = TRUE, target = NULL)}}
#' bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE,
#' move = FALSE, target = Africa_climate_target)
moveplot3 <- function(bp, time.var, group.var, move = TRUE, hulls = TRUE,
                      scale.var = 5, target = NULL)
{
  if(!is.null(group.var)) bp$group.aes <- bp$raw.X[,which(colnames(bp$raw.X) == group.var)] else
    bp$group.aes = NULL

  tvi <- which(colnames(bp$raw.X) == time.var)
  gvi <- which(colnames(bp$raw.X) == group.var)
  gviT <- which(colnames(target) == group.var)

  iterations <- nlevels(bp$raw.X[[tvi]])
  iter_levels <- levels(bp$raw.X[[tvi]])

  bp$iter_levels <- iter_levels

  group_levels <- levels(bp$raw.X[[gvi]])

  # Samples
  Z <- bp$Z
  Z <- suppressMessages(dplyr::bind_cols(Z, bp$Xcat))
  colnames(Z)[1:2] <- c("V1","V2")
  Z_tbl <- dplyr::as_tibble(Z)

  # Set limits
  # xlim
  minx <- min(Z_tbl$V1)
  maxx <- max(Z_tbl$V1)
  range_x <- maxx - minx

  # ylim
  miny <- min(Z_tbl$V2)
  maxy <- max(Z_tbl$V2)
  range_y <- maxy - miny

  perc <- 20/100
  xlim <- c(minx - perc*range_x,maxx + perc*range_x)
  ylim <- c(miny - perc*range_y,maxy + perc*range_y)

  # Basis extraction
  bp_list <- vector("list", iterations)
  axes_info <- vector("list", iterations)
  Z_list <- vector("list", iterations)
  Vr_list <- vector("list", iterations)
  chull_reg <- vector("list", iterations)

  for (i in 1:iterations)
  {
    # Filter data by custom years

    temp <- bp$raw.X |> dplyr::filter(bp$raw.X[[tvi]] == iter_levels[i])
    bp_list[[i]] <- biplotEZ::biplot(temp,scaled=bp$scaled) |> biplotEZ::PCA(group.aes = temp[[gvi]])
    colnames(bp_list[[i]]$Z) <- c("V1","V2")
    Z_list[[i]] <- dplyr::as_tibble(bp_list[[i]]$Z)
    Z_list[[i]] <- suppressMessages(dplyr::bind_cols(Z_list[[i]], bp_list[[i]]$Xcat))

    colnames(bp_list[[i]]$Vr) <- c("V1","V2")
    Vr_list[[i]] <- dplyr::as_tibble(bp_list[[i]]$Vr)
    Vr_list[[i]] <- Vr_list[[i]] |> dplyr::mutate(var = colnames(bp$X)) |>
      dplyr::mutate(time.var = iter_levels[i])
  }
  Z_tbl <- do.call(rbind,Z_list)
  Vr_tbl <- do.call(rbind,Vr_list)
  names(Vr_tbl)[4] <- time.var #don't want to hard code this

  # GPA preparation

  tviGZ <- which(colnames(Z_tbl) == time.var)
  tviGVr <- which(colnames(Vr_tbl) == time.var)

  # creating lists of row combined coordinates (Z and V)
  coord_set <- vector("list", iterations)
  for (i in 1:iterations)
  {
    Z_temp <- Z_tbl |> dplyr::filter(Z_tbl[[tviGZ]] == iter_levels[i])
    Vr_temp <- Vr_tbl |> dplyr::filter(Vr_tbl[[tviGVr]] == iter_levels[i])
    coord_set[[i]] <- dplyr::bind_rows(Z_temp[c("V1","V2")], Vr_temp[c("V1","V2")])
  }

  # determine sizes after final coord_set
  Z_split <- nrow(Z_temp)
  Vr_split <- nrow(Vr_temp)
  t_rows <- nrow(coord_set[[1]])

  if(!is.null(target))
  {
    temp <- biplotEZ::biplot(target,scaled=bp$scaled) |> biplotEZ::PCA(group.aes = target[[gviT]])
    G.target <- rbind(temp$Z, temp$Vr)
    GPA.out <- GPAbin::GPA(coord_set,G.target=G.target)
  } else {
    GPA.out <- GPAbin::GPA(coord_set,G.target=NULL)
  }

  G.target <- GPA.out[[4]] # centroid configuration
  Q.list <- GPA.out[[3]] # rotation matrix
  s.list <- GPA.out[[2]] # scaling factor
  # translation not used, since biplots are centred
  GPA_list <- GPA.out[[1]]

  bp$G.target <- G.target
  bp$coord_set <- coord_set
  bp$GPA_list <- GPA_list

  Z_GPA_list <- vector("list", iterations)
  Vr_GPA_list <- vector("list", iterations)
  chull_reg <- vector("list", iterations)

  # now bind_rows() of Z_GPA_list and Vr_GPA_list and adding columns of Z_tbl and Vr_tbl
  for (i in 1:iterations)
  {
    colnames(GPA_list[[i]]) <- c("V1","V2")
    Z_GPA_list[[i]] <- dplyr::as_tibble(GPA_list[[i]][1:Z_split,])
    Z_GPA_list[[i]] <- suppressMessages(dplyr::bind_cols(Z_GPA_list[[i]], bp_list[[i]]$Xcat))
    Vr_GPA_list[[i]] <- dplyr::as_tibble(GPA_list[[i]][((Z_split+1):t_rows),])
    Vr_GPA_list[[i]] <- Vr_GPA_list[[i]] |> dplyr::mutate(var = colnames(bp$X)) |>
      dplyr::mutate(time.var = iter_levels[i])
    names(Vr_GPA_list[[i]])[4] <- time.var

    Y <- Z_GPA_list[[i]]

    chull_reg_iter <- vector("list", length(group_levels))
    for(j in 1:length(group_levels))
    {
      temp2 <- which(Y[[group.var]] == group_levels[j]) # index of the group var
      chull_reg_iter[[j]] <- Y[temp2,][grDevices::chull(Y[temp2,]),]
    }
    chull_reg[[i]] <- do.call(rbind, chull_reg_iter)

  }

  Z_GPA_tbl <- do.call(rbind,Z_GPA_list)
  Vr_GPA_tbl <- do.call(rbind,Vr_GPA_list)
  chull_reg_GPA <- do.call(rbind,chull_reg)

  # Plotting

  # Move – TRUE Animated separate Z,V
  # Move – FALSE Facet separate Z,V
  if(move==TRUE)
  {
    bp$plot <- ggplot() +
      # Axes
      geom_segment(data=Vr_GPA_tbl, aes(x=0, y=0, xend=V1*scale.var, yend=V2*scale.var, group=var),
                   arrow=arrow(length=unit(0.1,"inches"))) +
      geom_text(data=Vr_GPA_tbl, aes(x=V1*scale.var, y=V2*scale.var,
                                     label = var,
                                     hjust="outward", vjust="outward",group=var),colour="black",size=4) +
      gganimate::transition_states(.data[[time.var]],
                                   transition_length = 2,
                                   state_length = 1) +
      # Sample polygons or points
      {if(hulls){
        geom_polygon(data = chull_reg_GPA,
                     aes(x=V1, y=V2,group = .data[[group.var]],
                         fill = .data[[group.var]]), alpha=0.5)

      } else {
        geom_point(data = Z_GPA_tbl,
                   aes(x=V1, y=V2,
                       group = .data[[group.var]],
                       fill =.data[[group.var]],
                       colour = .data[[group.var]]),size=2, alpha=0.8)
      }} +
      gganimate::transition_states(.data[[time.var]],
                                   transition_length = 2,
                                   state_length = 1) +
      labs(title = '{time.var}: {closest_state}',x="",y="") +
      #xlim(xlim) +
      #ylim(ylim) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      theme_classic() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = ggplot2::element_text(size=30,face ="bold"))

  } else {

    bp$plot <- ggplot() +
      # Axes
      geom_segment(data=Vr_GPA_tbl,aes(x=0,y=0,xend=V1*scale.var,yend=V2*scale.var,group=var),
                   arrow=arrow(length=unit(0.1,"inches"))) +
      geom_text(data=Vr_GPA_tbl,aes(x=V1*scale.var, y=V2*scale.var,
                                    label = var,
                                    hjust="outward", vjust="outward",group=var),colour="black",size=4) +
      # Sample polygons or points
      {if(hulls){
        geom_polygon(data = chull_reg_GPA,
                     aes(x=V1, y=V2,group = .data[[group.var]],
                         fill = .data[[group.var]]), alpha=0.5)

      } else {
        geom_point(data = Z_GPA_tbl,
                   aes(x=V1, y=V2,
                       group = .data[[group.var]],
                       fill =.data[[group.var]],
                       colour = .data[[group.var]]),size=2, alpha=0.8)
      }} + facet_wrap(~.data[[time.var]]) +
      #xlim(xlim) +
      #ylim(ylim) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      theme_classic() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
  }
  class(bp) <- append(class(bp), "moveplot3")
  if(move==TRUE)
    print(gganimate::animate(bp$plot,duration = 15,fps=10)) else
      print(bp$plot)
  bp
}

