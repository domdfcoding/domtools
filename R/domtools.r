# Copyright 2018 Dominic Davis-Foster, except where noted below
#' v1.1'

#' Clear Terminal Function
#'
#' This function clears the terminal on Unix systems.
#' Does not work on Windows.
#' Run makeActiveBinding("clear", clear, environment()) 
#'	to be able to type "clear"
#' @keywords clear
#' @export
#' @examples
#' clear()
clear <- function() cat(c("\033[2J","\033[0;0H"));


#' Newline Function
#'
#' This function starts a new line on the terminal.
#' Can also be used to print a blank line.
#' Run makeActiveBinding("newline", newline, environment())
#'	to be able to type "newline
#' @keywords newline
#' @export
#' @examples
#' newline()
newline <- function() cat("\n");

#' Input Prompt Function
#'
#' This function creates an input prompt with customisable text.
#' Multiple inputs are possible with "lines" option".
#'
#' @keywords input
#' @export
#' @examples
#' input("Press [Enter] to Continue")
#' name = input("Enter your name: ", lines=2)
input <- function(text='', lines=1) {
	cat(text);
	a <- readLines("stdin",n=lines);
	return(a)
}

#' Type Function
#'
#' Synonym for builtin "mode" function.
#'
#' @keywords type mode
#' @export
#' @examples
#' type(LETTERS)
#' 
type <- function(variable) {
	a <- mode(variable);
	return(a)
}

#' Mode Average Function
#'
#' Function to calculate the modal average.
#'
#' @keywords mode average
#' @export
#' @examples
#' modes(sample(1:10, 10, replace=TRUE))
#'	
modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#' Capitalise First Letter Function
#'
#' Function to capitalise the first letter of a character string.
#' From https://stackoverflow.com/questions/18509527/first-letter-to-upper-case/18509816
#'
#' @keywords capitalise letter character string
#' @export
#' @examples
#' firstup("rStudio")
#' 
firstup <- function(x) {
   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
x
}

#' Shapiro-Wilk Test for Data Frames
#'
#' Function to perform Shapiro-Wilk test for each column of a Data Frame.
#' Can provide interpretation for p-value with "interpret=TRUE".
#' Modified from https://stackoverflow.com/questions/45226457/r-loop-performing-ks-tests-across-data-frame-stored-in-matrix
#'
#' @keywords Shapiro Shapiro-Wilk data.frame normality
#' @export
#' @examples
#' colShapiro(dataframe, interpret=TRUE)
#' 
colShapiro <- function(x, print=TRUE, interpret=FALSE) {	
	res <- sapply(x, function(y) {
	  shapiro <- shapiro.test(y)
	if (interpret) {
		c(statistic=shapiro$statistic, p.value=shapiro$p.value, normal=shapiro$p.value>0.05)
		setNames(c(shapiro$statistic, shapiro$p.value, normal=shapiro$p.value>0.05), c("statistic", "p.value", "normal"))
	}
	else {
		c(statistic=shapiro$statistic, p.value=shapiro$p.value)
		setNames(c(shapiro$statistic, shapiro$p.value), c("statistic", "p.value"))
	}
	})
	
	if (print) {
		print("Shapiro-Wilk Test")
	}
	return(res)
}

#' Kolmogorov-Smirnov Normal Distribution Test for Data Frames
#'
#' Function to perform Kolmogorov-Smirnov test for normal distribution 
#' on each column of a Data Frame.
#' Can provide interpretation for p-value with "interpret=TRUE".
#' Modified from https://stackoverflow.com/questions/45226457/r-loop-performing-ks-tests-across-data-frame-stored-in-matrix
#'
#' @keywords Kolmogorov-Smirnov data.frame normalty KS
#' @export
#' @examples
#' colKS(dataframe, interpret=TRUE)
#' 
colKS <- function(x, print = TRUE, interpret=FALSE) {
	res <- sapply(x, function(y) {
	ks <- ks.test(y, pnorm, mean(y), sd(y))
	if (interpret) {
		c(statistic=ks$statistic, p.value=ks$p.value, normal=ks$p.value>0.05)
		setNames(c(ks$statistic, ks$p.value, normal=ks$p.value>0.05), c("statistic", "p.value", "normal"))
	}
	else {
		c(statistic=ks$statistic, p.value=ks$p.value)
		setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
	}
	})

	if (print) {
		print("Kolmogorov-Smirnov Test")
	}
	return(res)
}

#' Standard Deviation for Data Frames
#'
#' Function to calculate Standard Deviation for each column of a Data Frame.
#' Modified from https://stackoverflow.com/questions/37806387/r-calculate-standard-deviation-in-cols-in-a-data-frame-despite-of-na-values
#'
#' @keywords Standard Deviation std sd stdev data.frame
#' @export
#' @examples
#' colSD(dataframe)
#' 
# data frame sd
colSD <- function(x, print=TRUE) {
	if (print) {
		print("Standard Deviation")
	}
	sapply(x, sd, na.rm = TRUE)
}

#' Median for Data Frames
#'
#' Function to calculate Median for each column of a Data Frame.
#' Modified from https://stackoverflow.com/questions/37806387/r-calculate-standard-deviation-in-cols-in-a-data-frame-despite-of-na-values
#'
#' @keywords Median data.frame
#' @export
#' @examples
#' colMedian(dataframe)
#' 
# data frame sd
colMedian <- function(x) {
	print("Median")
	sapply(x, median, na.rm = TRUE)
}
