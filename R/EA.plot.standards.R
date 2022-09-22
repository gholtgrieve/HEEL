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
  drift.correct.flag <- results$drift.correct.flag
  blank.correct.flag <- results$blank.correct.flag

  # Pick the correct data depending on which corrections were performed.
  if(str_detect(drift.correct.flag, "both|BOTH|Both")) {
      d13C <- standard.CN$d.13C.12C.drift[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
      d15N <- standard.CN$d.15N.14N.drift[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
  } else if(str_detect(drift.correct.flag, "none|NONE|None")){
    d13C <- standard.CN$d.13C.12C[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
    d15N <- standard.CN$d.15N.14N[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
  } else if(str_detect(drift.correct.flag, "C|c")){
    d13C <- standard.CN$d.13C.12C.drift[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
    d15N <- standard.CN$d.15N.14N[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
  } else if(str_detect(drift.correct.flag, "N|n")){
    d15N <- standard.CN$d.15N.14N.drift[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
    d13C <- standard.CN$d.13C.12C[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
  } else if (blank.correct.flag){
    d13C <- standard.CN$d.13C.12C.blank[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
    d15N <- standard.CN$d.15N.14N.blank[standard.CN$group=="GA1"|standard.CN$group=="GA2"]
  }
  temp <- standard.CN[standard.CN$group=="GA1"|standard.CN$group=="GA2", c("Area.28", "Area.44", "group")]
  temp <- data.frame(d13C=d13C, d15N=d15N, temp)

  # PLOT 1 -----------------------------
  # Area Response vs. Measured Element Mass for GA1 & GA2 standards
    p1 <- ggplot(standard.CN[standard.CN$group=="GA1"|standard.CN$group=="GA2",], aes(x = mass.N.mg, y = Area.28)) +
      geom_point() +
      labs(x = "mass (mg)", title = "Area Response vs. Input N") +
      geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
               label.y.npc = "center") +
      ggpubr::stat_regline_equation()
    p2 <- ggplot(standard.CN[standard.CN$group=="GA1"|standard.CN$group=="GA2",], aes(x = mass.C.mg, y = Area.44)) +
      geom_point() +
      labs(x = "mass (mg)", title = "Area Response vs. Input C") +
      geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
      ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
               label.y.npc = "center") +
      theme_minimal() +
      ggpubr::stat_regline_equation()
    p21 <- gridExtra::grid.arrange(p2, p1, ncol = 2)

    #Save linear models for mass calculations later
    temp2 <- standard.CN[standard.CN$group == "GA2" | standard.CN$group == "GA1",]
    N.mass.vs.Area28.lm.coeff <- coefficients(lm(temp2$mass.N.mg ~ temp$Area.28))
    C.mass.vs.Area44.lm.coeff <- coefficients(lm(temp2$mass.C.mg ~ temp$Area.44))


    # PLOT 2 -----------------------------
    # Delta vs. Area of QTY standards
    # includes only  GA1 & GA2
      p5 <- ggplot(temp, aes(x = Area.28, y = d15N, group = group, color = group)) +
        geom_point() +
        labs(y = "d15N", title = "d15N vs m/z 28") +
        ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                label.y.npc = "center") +
        ggpubr::stat_regline_equation(label.y.npc = "top") +
        geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
        theme_minimal() +
        theme(legend.position = "none")
      p6 <-  ggplot(temp, aes(x = Area.44, y = d13C, group = group, color = group)) +
        geom_point() +
        labs(y = "d13C", title = "d13C vs m/z 44 ") +
        ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                label.y.npc = "center") +
        ggpubr::stat_regline_equation(label.y.npc = "top") +
        theme_minimal() +
        geom_smooth(method = lm, formula = y ~ x, se = FALSE)
      p65 <- gridExtra::grid.arrange(p6, p5, ncol = 2)

      #Save linear models for corrections later
      d15N.vs.Area28.lm.coeff <- coefficients(lm(temp$d15N ~ temp$Area.28))
      d13C.vs.Area44.lm.coeff <- coefficients(lm(temp$d13C ~ temp$Area.44))

      temp <- data.frame(Model=c("d13C.vs.Area44", "d15N.vs.Area28", "MassC.vs.Area44", "MassN.vs.Area28"),
                rbind(d13C.vs.Area44.lm.coeff, d15N.vs.Area28.lm.coeff, C.mass.vs.Area44.lm.coeff, N.mass.vs.Area28.lm.coeff))
      colnames(temp) <- c("Model","Intercept", "Slope")
      standard.coefficients <- tibble(temp)

      return(list(standard.plots=list(p21,p65), standard.coefficients))
}
