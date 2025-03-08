# read a spatially-temporally varying coefficient model from a symbolic description
parseFormula2 <- function(formula, data, intercept = TRUE, justX = FALSE) {
  
  # extract Y, X, and variable names for model formula and frame
  mt <- terms(formula, data = data)
  if (missing(data))
    data <- sys.frame(sys.parent())
  mf <- match.call(expand.dots = FALSE)
  mf$intercept <- mf$justX <- NULL
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, sys.frame(sys.parent()))
  if (!intercept) {
    attributes(mt)$intercept <- 0
  }
  
  # null model support
  X <- if (!is.empty.model(mt))
    model.matrix(mt, mf)
  X <- as.matrix(X)  # X matrix
  xvars <- dimnames(X)[[2L]]  # X variable names
  xobs <- dimnames(X)[[1L]]  # X observation names
  if (justX) {
    Y <- NULL
  } else {
    Y <- as.matrix(model.response(mf, "numeric"))  # Y matrix
  }
  
  # Parse for variables in parentheses
  # Match the part of the formula inside parentheses
  matches <- regmatches(as.character(formula)[3], gregexpr("\\((.*?)\\)",
                                                           as.character(formula)[3]))
  if (length(matches[[1]]) > 0) {
    # Filter out invalid cases like (0)
    inner_parts <- matches[[1]]
    inner_parts <- inner_parts[inner_parts != "(0)"]  # Remove "(0)"
    
    if (length(inner_parts) > 0) {
      # Construct a new formula for valid parentheses content
      inner_formula <- as.formula(paste("~", paste(inner_parts, collapse = "+")))
      inner_terms <- terms(inner_formula, data = data)
      mf_inner <- model.frame(inner_terms, data, drop.unused.levels = TRUE)
      X_tilde <- as.matrix(model.matrix(inner_terms, mf_inner))
    } else {
      # No valid content in parentheses
      X_tilde <- NULL
    }
  } else {
    # No parentheses, X_tilde is NULL
    X_tilde <- NULL
  }
  X_tilde_vars <- dimnames(X_tilde)[2L]
  
  return(list(Y, X, xvars, xobs, X_tilde, X_tilde_vars))
  
}

# internal function: checks if input is integer
is_integer <- function(x) {
  is.numeric(x) && (floor(x) == x)
}