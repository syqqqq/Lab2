# Define a function to make a boxplot

#' payment_boxplot
#'
#' makes a boxplot of payments by DRG code
#'
#' @param payment what payment to make a boxplot (Average.Total.Payments/Average.Covered.Charges/Average.Medicare.Payments)
#'
#' @return a boxplot of payments by DRG code
#' @export
#'
#' @examples
#' result <- payment_boxplot("Average.Total.Payments")
payment_boxplot <- function(payment = c("Average.Total.Payments", "Average.Covered.Charges", "Average.Medicare.Payments")){

  # Check if the input is one of the settings
  payment <- match.arg(payment)

  # Extract the DRG code from the DRG.Definition
  MSDRG <- MSDRG %>%
    mutate(DRG.Code = substring(DRG.Definition, 1, 3))

  # Make a boxplot
  MSDRG %>%
    ggplot(aes_string(payment, "DRG.Code")) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", gsub("[.]", " ", payment), "by DRG Code"),
         x = gsub("[.]", " ", payment),
         y = "DRG Code")
}

# Define a function to calculate the mean, median, sd of average medicare payments

#' Statistics
#'
#' calculates statistics over all of the DRG codes for average Medicare payments
#'
#' @param method what to calculate (mean/median/sd)
#'
#' @return  a table of the DRG code and the corresponding result
#' @export
#'
#' @examples
#' result <- statistics("mean")
statistics <- function(method = c("mean", "median", "sd")){

  # Check if the input is one of the settings
  method <- match.arg(method)

  # Extract the DRG code from the DRG.Definition
  MSDRG <- MSDRG %>%
    mutate(DRG.Code = substring(DRG.Definition, 1, 3))

  # Get the needed function
  func <- get(method)

  # Caluculate statistics
  result <- MSDRG %>%
    group_by(DRG.Code) %>%
    summarise(result = func(Average.Medicare.Payments))

  # rename the result column
  names(result)[2] <- method

  return(result)
  }





