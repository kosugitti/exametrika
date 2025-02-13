% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_exametrikaPlot.R
\name{plot.exametrika}
\alias{plot.exametrika}
\title{Plotting functions for the exametrika package of class "exametrika"}
\usage{
\method{plot}{exametrika}(
  x,
  type = c("IIC", "ICC", "TIC", "IRP", "TRP", "LCD", "CMP", "FRP", "RMP", "LRD", "Array",
    "FieldPIRP", "LDPSR", "ScoreFreq", "ScoreRank", "ICRP", "ICBR"),
  items = NULL,
  students = NULL,
  nc = 1,
  nr = 1,
  ...
)
}
\arguments{
\item{x}{exametrika Class object}

\item{type}{Plot type.The selectable type names are as follows: IIC, ICC, TIC, IRP, TRP,
LCD, CMP, FRP, RMP, LRD, Array, FieldPRIP, LDPSR.
\describe{
\item{ICC}{Item Characteristic Curve. For \link{IRT} model}
\item{IIC}{Item Information Curve. For \link{IRT} model. When specifying the item numbers
with the \code{items} option, giving 0 will make it TIC.}
\item{TIC}{Test Information Curve. For \link{IRT} model}
\item{IRP}{Item Reference Profile.IRP is a line graph with items and latent classes/ranks
on the horizontal axis, and membership probability on the vertical axis. This type can be
selected when using \link{LCA},\link{LRA},\link{Biclustering} and \link{LDB} model.}
\item{TRP}{Test Reference Profile. TRP is a representation that uses the latent classes/ranks
on the horizontal axis. It simultaneously displays the number of members belonging to each
class/rank) as a bar graph and the expected test scores as a line graph.This type can be
selected for all models except IRT.}
\item{LCD}{Latent Class Distribution. LCD is a graph that takes latent classes on the horizontal
axis, represents the number of members belonging to each class with a bar graph, and plots the
cumulative predicted membership probability with a line graph. It can be selected for all
models except IRT.}
\item{LRD}{Latent Rank Distribution. The difference between LRD and LCD is whether the horizontal
axis represents classes or ranks.}
\item{CMP}{Class Membership Profile.CMP is a line graph representing the class membership
probabilities of students. Since one graph is drawn for each student, using the 'students'
option allows you to specify which students to display. Additionally, with the 'nr' and 'nc'
options, you should ensure the ability to display multiple figures.}
\item{RMP}{Rank Membership Profile. The difference between RMP and CMP is whether the horizontal
axis represents classes or ranks.}
\item{ScoreFreq}{A frequency polygon corresponding to the score frequency distribution,
plotted together with rank thresholds.}
\item{ScoreRank}{A heatmap representing the probabilities of score membership for each rank.}
\item{ICRP}{A visualization that displays the relationship between ranks (x-axis)
and category response probabilities (y-axis). For each item, multiple lines are plotted,
each representing the probability of selecting a specific response category. These lines
show how the likelihood of choosing each response category changes across different ranks.}
\item{ICBR}{A visualization that shows the relationship between ranks (x-axis)
and cumulative category probabilities (y-axis). For each item, multiple boundary lines are plotted,
each representing the probability of scoring at or above a specific category threshold.
These lines illustrate how the cumulative probabilities of reaching each category boundary
change across different ranks.}
\item{FRP}{Field Reference Profile. "FRP is a diagram depicting the correspondence between the field
and the latent class/rank. It represents the expected correct answer rate of members belonging to
a particular latent class/rank using a line graph.}
\item{Array}{Array plot for Biclustering/Ranklustering.An Array plot is a diagram coloring the
matrix cells, in which the larger the cell value, the darker the cell color. In this plot of the binary
raw data, the corrected responses are shaded in black, and the black-and-white pattern appears to be
random.However, after being classified by biclustering, students' answer patterns and items' answer
patterns are each sorted based on similarity. Thus, the divisions made by the clustering are visually
evident.}
\item{FieldPIRP}{This type can only be selected in the \link{LDB} model. The horizontal axis represents the
number of correct answers in the parent field, while the vertical axis represents the correct response
rate in the specified rank. A line graph is drawn for each item included in the field.}
\item{LDPSR}{Latent Dependence Passing Student Rate shows that is a graph that takes items in field j
on the horizontal axis and represents the passing rates of both parent and child classes on the graph.}
}}

\item{items}{Specify the items you want to plot as a vector. If not specifically designated,
all items will be included.When the type is IIC, if the specified item is 0, it returns a TIC
representing the entire test.}

\item{students}{Specify the numbers of the students you want to plot as a vector.
If not specifically designated, all students will be included.}

\item{nc}{Specifying the number of columns when there are many plots to be drawn. The default is 1.}

\item{nr}{Specifying the number of rows when there are many plots to be drawn. The default is 1.}

\item{...}{other options}
}
\value{
Produces different types of plots depending on the class of the input object and the specified type:
\itemize{
\item For IRT models: ICC (Item Characteristic Curves), IIC (Item Information Curves),
or TIC (Test Information Curves)
\item For LCA/LRA models: IRP (Item Reference Profile), TRP (Test Reference Profile),
LCD/LRD (Latent Class/Rank Distribution), CMP/RMP (Class/Rank Membership Profile)
\item For Biclustering/IRM models: Array plots showing clustering patterns
\item For LDLRA/LDB/BINET models: Various network and profile plots specific to each model
}
The function returns NULL invisibly.
}
\description{
The calculation results of the exametrika package have an exametrika class attribute.
In addition, the class name of the analysis model is also assigned.
The models are listed as follows: IRT, LCA, LRA, Biclustering, IRM, LDLRA, LDB,
BINET. A plot is made for each model. Although the analysis results are visualized
from various perspectives, they correspond by specifying the 'type' variable when plotting.
}
\details{
\itemize{
\item "IRT": Can only have types "ICC", "IIC", "TIC".
\item "LCA": Can only have types "IRP", "FRP", "TRP", "LCD", "CMP".
\item "LRA": Can only have types "IRP", "FRP", "TRP", "LRD", "RMP".
\item "Biclustering": Can only have types "IRP", "FRP", "LCD", "LRD", "CMP", "RMP", "Array".
\item "IRM": Can only have types "FRP", "TRP", "Array".
\item "LDLRA": Can only have types "IRP", "TRP", "LRD", "RMP".
\item "LDB": Can only have types "FRP", "TRP", "LRD", "RMP", "Array", "FieldPIRP".
\item "BINET": Can only have types "FRP", "TRP", "LRD", "RMP", "Array", "LDPSR".
}
}
