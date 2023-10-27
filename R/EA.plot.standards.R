#' @title Plots peak area vs. mass and d13C/d15N vs. peak area of working standards.
#'
#' @description This function...
#'
#'
#' @usage EA.plot.standards(results)
#'
#' @param results List containing results from previous functions.
#'
#' @import tidyverse
#' @importFrom ggpubr stat_cor stat_regline_equation
#' @importFrom gridExtra grid.arrange
#' @importFrom tools file_path_sans_ext
#' @importFrom tibble tibble
#'
#' @return List containing lm objects from four linear models:
#'              mass N vs. m/z 28
#'              mass C vs. m/z 44
#'              d15N vs. m/z 28
#'              d13C vs. m/z 44
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export


EA.plot.standards <- function(results){

  standard.CN <- results$standard.CN
  sequenceID <- results$analysis.dates
  current.data.columns <- results$current.data.columns
#  drift.correct.flag <- results$drift.correct.flag
#  blank.correct.flag <- results$blank.correct.flag

  # Pick the correct data depending on which corrections were performed.
  standard.CN.temp <- standard.CN[standard.CN$group=="GA1"|standard.CN$group=="GA2",
                                  c("Analysis", "Row", "Comment", "group", "Area.28", "Area.44", "mass.N.mg", "mass.C.mg", current.data.columns)]
  names(standard.CN.temp) <- c("Analysis", "Row", "Comment", "group", "Area.28", "Area.44", "mass.N.mg", "mass.C.mg", "d15N", "d13C")


  # PLOT 1 -----------------------------
  # Area Response vs. Measured Element Mass for GA1 & GA2 standards
    p1 <- ggplot(standard.CN.temp, aes(x = mass.N.mg, y = Area.28)) +
      geom_point() +
      labs(x = "mass (mg)", title = "Area Response vs. Input N") +
      geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggpubr::stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
               label.y.npc = "center") +
      ggpubr::stat_regline_equation()
    p2 <- ggplot(standard.CN.temp, aes(x = mass.C.mg, y = Area.44)) +
      geom_point() +
      labs(x = "mass (mg)", title = "Area Response vs. Input C") +
      geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
      ggpubr::stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
               label.y.npc = "center") +
      theme_minimal() +
      ggpubr::stat_regline_equation()
    ggsave("PeakArea_vs_Mass.png",gridExtra::grid.arrange(p2, p1, ncol = 2))
    p21 <- str_c(results$processed.data.dir, "/PeakArea_vs_Mass.png")

    #Save linear models for mass calculations later
    N.mass.vs.Area28.lm.coeff <- coefficients(lm(standard.CN.temp$mass.N.mg ~ standard.CN.temp$Area.28))
    C.mass.vs.Area44.lm.coeff <- coefficients(lm(standard.CN.temp$mass.C.mg ~ standard.CN.temp$Area.44))


    # PLOT 2 -----------------------------
    # Delta vs. Area of QTY standards
    # includes only  GA1 & GA2
      p3 <- ggplot(standard.CN.temp[standard.CN.temp$Comment=="STANDARD",], aes(x = Area.28, y = d15N, group = group, color = group)) +
        geom_point() +
        labs(y = "d15N", title = "d15N vs m/z 28") +
      ggpubr::stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
                label.y.npc = "center") +
        ggpubr::stat_regline_equation(label.y.npc = "top") +
        geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
        theme_minimal() +
        theme(legend.position = "none")
      p4 <-  ggplot(standard.CN.temp[standard.CN.temp$Comment=="STANDARD",], aes(x = Area.44, y = d13C, group = group, color = group)) +
        geom_point() +
        labs(y = "d13C", title = "d13C vs m/z 44 ") +
        ggpubr::stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
                label.y.npc = "center") +
        ggpubr::stat_regline_equation(label.y.npc = "top") +
        theme_minimal() +
        geom_smooth(method = lm, formula = y ~ x, se = FALSE)
      ggsave("Isotopes_vs_PeakArea.png", gridExtra::grid.arrange(p3, p4, ncol = 2))
      p34 <- str_c(results$processed.data.dir, "/Isotopes_vs_PeakArea.png")

      #Save linear models for corrections later
      d15N.vs.Area28.lm.coeff <- coefficients(lm(d15N ~ Area.28, data=standard.CN.temp[standard.CN.temp$Comment=="STANDARD",]))
      d13C.vs.Area44.lm.coeff <- coefficients(lm(d13C ~ Area.44, data=standard.CN.temp[standard.CN.temp$Comment=="STANDARD",]))

      lm.temp <- data.frame(Model=c("d13C.vs.Area44", "d15N.vs.Area28", "MassC.vs.Area44", "MassN.vs.Area28"),
                rbind(d13C.vs.Area44.lm.coeff, d15N.vs.Area28.lm.coeff, C.mass.vs.Area44.lm.coeff, N.mass.vs.Area28.lm.coeff))
      colnames(lm.temp) <- c("Model","Intercept", "Slope")
      standard.coefficients <- tibble(lm.temp)


      # PLOT 3 -----------------------------
      # Delta vs. Row number in run
      # includes only  GA1 & GA2
      p5 <- ggplot(standard.CN.temp, aes(x = Row, y = d15N, group = group, color = group)) +
        geom_point() +
        labs(y = "d15N", title = "d15N vs Row Number") +
        ggpubr::stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
                         label.y.npc = "center") +
        ggpubr::stat_regline_equation(label.y.npc = "top") +
        geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
        theme_minimal() +
        theme(legend.position = "none")
      p6 <-  ggplot(standard.CN.temp, aes(x = Row, y = d13C, group = group, color = group)) +
        geom_point() +
        labs(y = "d13C", title = "d13C vs Row Number") +
        ggpubr::stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
                         label.y.npc = "center") +
        ggpubr::stat_regline_equation(label.y.npc = "top") +
        theme_minimal() +
        geom_smooth(method = lm, formula = y ~ x, se = FALSE)
      ggsave("Isotopes_vs_RowNumber.png", gridExtra::grid.arrange(p6, p5, ncol = 2))
      p65 <- str_c(results$processed.data.dir, "/Isotopes_vs_RowNumber.png")

      #Save linear models for corrections later
      d15N.vs.Area28.lm.coeff <- coefficients(lm(standard.CN.temp$d15N ~ standard.CN.temp$Area.28))
      d13C.vs.Area44.lm.coeff <- coefficients(lm(standard.CN.temp$d13C ~ standard.CN.temp$Area.44))

      temp <- data.frame(Model=c("d13C.vs.Area44", "d15N.vs.Area28", "MassC.vs.Area44", "MassN.vs.Area28"),
                         rbind(d13C.vs.Area44.lm.coeff, d15N.vs.Area28.lm.coeff, C.mass.vs.Area44.lm.coeff, N.mass.vs.Area28.lm.coeff))
      colnames(temp) <- c("Model","Intercept", "Slope")
      standard.coefficients <- tibble(temp)

      return(list(standard.plots=list(p21,p65), standard.coefficients))
}
