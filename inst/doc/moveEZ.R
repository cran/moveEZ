## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(moveEZ) ; library(tibble) ; library(scales); library(gganimate)

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

## ----warning=FALSE, message=FALSE---------------------------------------------
bp |> moveplot(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE)

## ----warning=FALSE------------------------------------------------------------
bp |> moveplot2(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE)

## ----warning=FALSE, message=FALSE---------------------------------------------
bp |> moveplot2(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE,
                align.time = "1950", reflect = "x")

## -----------------------------------------------------------------------------
data("Africa_climate_target")
tibble::tibble(Africa_climate_target)

## ----warning=FALSE------------------------------------------------------------
bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE,
                target = NULL)

## ----warning=FALSE------------------------------------------------------------
bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = FALSE, 
                target = Africa_climate_target)

## ----warning=FALSE, message=FALSE---------------------------------------------
bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE, 
                target = NULL)

## ----warning=FALSE, message=FALSE---------------------------------------------
bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE, move = TRUE, 
                target = Africa_climate_target)

