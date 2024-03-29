% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_definition.R
\docType{class}
\name{Radiomics-class}
\alias{Radiomics-class}
\title{A radiomics class}
\description{
A radiomics class
}
\section{Slots}{

\describe{
\item{\code{step_pre0}}{preProcess class}

\item{\code{step_pre1}}{preProcess class}

\item{\code{names.dt}}{the feature names after nearZeroVar function}

\item{\code{useful_name}}{the feature names of the final model}

\item{\code{fit}}{a glmnet output fit model}

\item{\code{cvfit}}{a cv.glmnet output fit model}

\item{\code{s}}{the best lambda value}

\item{\code{threshold}}{the model threshold}
}}

\examples{
model <- new('Radiomics')
}
