#' Active Variable Selection (Via First Order Linear Model)
#'
#' @description A method for selecting the active variables of a model by fitting a first-order linear model
#' using some stepwise selection criteria, and choosing those that are chosen within the model.
#'
#' @param x the set of training points in the input space, given as a dataframe.
#' @param fx experiment/model evaluation at the training points, given as a vector.
#' @param scale specifies the estimate of the error variance.  The default, \code{0}, indicates that this value is to be selected via maximum likelihood.
#' \code{scale_maxlrm1} indicates that it is to be estimated as the variance parameter of the full model.
#' @param k allows specification of the k parameter of the information criterion.
#' The default, \code{2}, is AIC.  \code{log(n)} is BIC.  Can also specify strings \code{"BIC"} or \code{"logn/2"}.
#' @inheritParams stats::step
#'
#' @return
#' \item{stepwise_selected_model}{the stepwise selected model from \code{step}}
#' \item{active_variables}{the active variables, given by those selected for \code{stepwise_selected_model}}
#' @export
#'
#' @seealso
#' \code{\link[stats]{step}}
#'
#' @examples
#' data(USArrests)
#' X <- USArrests[,c("Assault", "UrbanPop")]
#' y <- USArrests[,"Murder"]
#' AVS( X, y )
AVS <- function(x, fx, scale = 0, k = 2, trace = 0){

  # Check input conditions.
  if( is.data.frame( x ) == FALSE ){ stop( "x must be a dataframe." ) }
  if( is.vector( fx ) == FALSE ){ stop( "fx must be a vector." ) }

  # Number of training points for fitting the linear model.
  n <- nrow( x )

  # The minimum model for selecting active variables is the constant model.
  minlrm1 <- stats::lm( fx ~ 1, data = x )

  # The maximum model contains each input linearly.
  maxlrm1 <- stats::lm( fx ~ ., data = x )

  # Variable names.
  variable_names <- names( maxlrm1$coefficients )[-1]

  # An option to pick the scale parameter as the variance parameter of the full first-order linear model.
  if(scale == "scale_maxlrm1"){
    scale_maxlrm1 <- summary( maxlrm1 )$sigma^2
    scale <- scale_maxlrm1
  }

  # An option for picking the multiple of the number of regressors for the Information criterion
  # used for selecting models in the step function as log(n) (BIC).
  if(k=="BIC"){
    k <- log(n)
  }

  # An option for picking the multiple of the number of regressors for the Information criterion used
  # for selecting models in the step function as log(n)/2.
  if(k=="logn/2"){
    k <- log(n)/2
  }

  # Perform the step function, starting from the full model of all input variables included linearly,
  # with possibility of going in both directions (after the third iteration only of course!).
  stepwise_selected_model <- stats::step( maxlrm1, scale = scale, k = k,
    scope = list(lower=minlrm1, upper=maxlrm1), trace = trace, direction = "both" )

  # The active variable names as a vector.
  active_variables <- names( stepwise_selected_model$coefficients )[-1]

  # Return the selected model and the active variables.
  return( list( "stepwise_selected_model" = stepwise_selected_model, "active_variables" = active_variables ) )

}
