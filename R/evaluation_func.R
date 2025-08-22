#' Measures of comparison for move plot 3
#'
#' This function calculates measures of comparison after generalised orthogonal Procrustes Analysis
#' is performed in \code{moveplot3}. Orthogonal Procrustes Analysis is used to compare a target to a testee configuration.
#' The following measures are calculate: Procrustes Statistic (PS), Congruence Coefficient (CC), Absolute Mean Bias (AMB),
#' Mean Bias (MB) and Root Mean Squared Bias (RMSB).
#'
#'
#' @param bp biplot object from \code{moveEZ}
#' @param centring logical argument to apply centring or not (default is \code{TRUE})
#'
#' @returns
#' \item{eval.list}{Returns a list containing the measures of comparison for each level of the time variable.}
#' \item{fit.plot}{Returns a line plot with the fit measures that are bounded between zero and one: PS and CC. A small PS value and large CC value indicate good fit.}
#' \item{bias.plot}{Returns a line plot with bias measures taht are unbounded: AMB, MB and RMSB. Small values indicate low bias.}
#'
#' @export
#'
#' @examples
#' data(Africa_climate)
#' data(Africa_climate_target)
#' bp <- biplotEZ::biplot(Africa_climate, scaled = TRUE) |> biplotEZ::PCA()
#' results <- bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE,
#' move = FALSE, target = NULL) |> evaluation()
#' results$eval.list
#' results$fit.plot
#' results$bias.plot
#'
#' data(Africa_climate)
#' data(Africa_climate_target)
#' bp <- biplotEZ::biplot(Africa_climate, scaled = TRUE) |> biplotEZ::PCA()
#' results <- bp |> moveplot3(time.var = "Year", group.var = "Region", hulls = TRUE,
#' move = FALSE, target = Africa_climate_target) |> evaluation()
#' results$eval.list
#' results$fit.plot
#' results$bias.plot
#'
evaluation <- function(bp, centring = TRUE)
{
  if (inherits(bp, "moveplot3")){
    eval.list <- vector("list", length(bp$coord_set))

    target <- bp$G.target

    for (i in 1:length(bp$coord_set))
    {

      n.Y <- nrow(target)
      p.Y <- ncol(target)
      target <- as.matrix(target)

      testee <- bp$coord_set[[i]]
      n.X <- nrow(testee)
      p.X <- ncol(testee)
      testee <- as.matrix(testee)

      if(!centring)
      {
        testee <- testee
        target <- target
      }
      else
      {
        testee <- scale(testee, TRUE, FALSE)
        #centre=T, scale=F results are similar to Cox and Cox, Gower and #Dijkersthuis, Borg and Groenen
        target <- scale(target, TRUE, FALSE)
      }

      #transformations
      C.mat <- t(target)%*%testee
      svd.C <- svd(C.mat)
      A.mat <- svd.C[[3]]%*%t(svd.C[[2]])
      s.fact <- sum(diag(t(target)%*%testee%*%A.mat))/sum(diag(t(testee)%*%testee))
      #Gower and Dijksterhuis P32
      b.fact <- as.vector(1/n.Y * t(target - s.fact * testee %*% A.mat)%*%rep(1,n.Y))
      X.new <- b.fact + s.fact*testee%*%A.mat
      Res.SS <- sum(diag(t(((s.fact*testee%*%A.mat)-target))%*%((s.fact*testee%*%A.mat)-target)))
      Tot.SS <- s.fact^2*sum(diag(t(testee)%*%testee))+sum(diag(t(target)%*%target))
      Fit.SS <- 2*s.fact*sum(diag(svd.C[[1]]))

      PS <- Res.SS/sum(diag(t(target)%*%target))
      CC <- sum(stats::dist(testee) * stats::dist(target))/(sqrt(sum(stats::dist(testee)^2)) * sqrt(sum(stats::dist(target)^2)))
      RMSB <- ((sum(sum((target-testee)^2)))/length(testee))^(0.5)
      MB <- (sum(sum((target-testee)^1)))/length(testee)
      AMB <- (sum(sum(abs(target-testee))))/length(testee)

      REStable <- data.frame(c(PS, CC, AMB, MB, RMSB))
      colnames(REStable)<- paste("Target vs. ",bp$iter_levels[i], sep="")
      rownames(REStable)<- c("PS", "CC", "AMB", "MB", "RMSB")

      eval.list[[i]] <- REStable

    }

    bias_meas <- c("AMB", "MB", "RMSB")
    fit_meas <- c("PS", "CC")

    eval_df <- do.call(rbind, lapply(eval.list, function(x) {
      year <- as.numeric(gsub("Target vs. ", "", colnames(x)))
      data.frame(
        Year = year,
        Measure = rownames(x),
        Value = as.numeric(x[,1]),
        row.names = NULL
      )
    }))

    fit.plot <- ggplot() +
      ggplot2::geom_line(data = eval_df |> dplyr::filter(Measure  %in% fit_meas),
                         aes(x = Year, y = Value, color = Measure, group = Measure), linewidth = 1) +
      labs(x = "Year", y = "Value", title = "Evaluation Metrics over Time") +
      ggplot2::theme_minimal()

    bias.plot <- ggplot() +
      ggplot2::geom_line(data = eval_df |> dplyr::filter(Measure  %in% bias_meas),
                         aes(x = Year, y = Value, color = Measure, group = Measure), linewidth = 1) +
      labs(x = "Year", y = "Value", title = "Evaluation Metrics over Time") +
      ggplot2::theme_minimal()

    bp$bias.plot <- bias.plot
    bp$fit.plot <- fit.plot
    bp$eval.list <- lapply(eval.list, round,4)
    bp

  } else
    print("Evaluation measures can only be applied for moveplot3().")

  bp

}

