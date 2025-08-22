## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(moveEZ); library(tibble); library(scales); library(gganimate)

## -----------------------------------------------------------------------------
library(moveEZ) 
data("Africa_climate")
tibble::tibble(Africa_climate)

## ----message=FALSE------------------------------------------------------------
library(biplotEZ)
bp <- biplot(Africa_climate, scaled = TRUE) |> 
  PCA(group.aes = Africa_climate$Region) |> 
  samples(opacity = 0.8, col = scales::hue_pal()(10)) |>
  plot()

## ----warning=FALSE------------------------------------------------------------
bp |> moveplot(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE)

## ----echo=FALSE,eval = FALSE,warning=FALSE, message=FALSE---------------------
# anim1 <- bp |> moveplot(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE)
# anim_rendered <- animate(anim1, renderer = gifski_renderer(), nframes = 100, fps = 10)
# anim_save("vignettes/anim1.gif", animation = anim_rendered)

## ----warning=FALSE------------------------------------------------------------
bp |> moveplot2(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE)

## ----echo=FALSE,eval = FALSE,warning=FALSE, message=FALSE---------------------
# anim2 <- bp |> moveplot2(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE,
#                 align.time = "1950", reflect = "x")
# anim_rendered <- animate(anim2, renderer = gifski_renderer(), nframes = 100, fps = 10)
# anim_save("vignettes/anim2.gif", animation = anim_rendered)

## -----------------------------------------------------------------------------
data("Africa_climate_target")
tibble::tibble(Africa_climate_target)

## ----warning=FALSE------------------------------------------------------------
bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE,
                target = NULL)

## ----warning=FALSE------------------------------------------------------------
bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE, 
                target = Africa_climate_target)

## ----echo=FALSE,eval = FALSE,warning=FALSE, message=FALSE---------------------
# anim3 <- bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE,
#                 target = NULL)
# anim_rendered <- animate(anim3, renderer = gifski_renderer(), nframes = 100, fps = 10)
# anim_save("vignettes/anim3.gif", animation = anim_rendered)

## ----echo=FALSE,eval = FALSE,warning=FALSE, message=FALSE---------------------
# anim4 <- bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE,
#                 target = Africa_climate_target)
# anim_rendered <- animate(anim4, renderer = gifski_renderer(), nframes = 100, fps = 10)
# anim_save("vignettes/anim4.gif", animation = anim_rendered)

## ----message=FALSE------------------------------------------------------------
results <- bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, 
                           move = FALSE, target = NULL) |> evaluation()

## -----------------------------------------------------------------------------
results$eval.list

## ----warning=FALSE------------------------------------------------------------
results$fit.plot

## ----warning=FALSE------------------------------------------------------------
results$bias.plot

