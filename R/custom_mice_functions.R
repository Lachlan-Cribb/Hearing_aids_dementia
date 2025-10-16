#### mice elastic net PMM function ####

mice.impute.elasticnet.pmm <- function (y,
                                        ry,
                                        x,
                                        wy = NULL,
                                        nfolds = 5,
                                        ...)
{
  if (is.null(wy))
    wy <- !ry
  x_glmnet <- cbind(1, x)
  xobs <- x_glmnet[ry, , drop = FALSE]
  xmis <- x[wy, ]
  yobs <- y[ry]
  cv_lasso <- glmnet::cv.glmnet(
    x = xobs,
    y = yobs,
    family = "gaussian",
    nfolds = nfolds,
    alpha = 0.5
  )
  glmnet_coefs <- as.matrix(coef(cv_lasso, s = "lambda.min"))[, 1]
  AS <- which((glmnet_coefs != 0)[-1])
  xas <- x_glmnet[, AS, drop = FALSE]
  vec <- mice.impute.pmm(
    y = y,
    ry = ry,
    x = xas,
    wy = wy,
    ...
  )
  vec
}
