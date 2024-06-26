# These functions are tested indirectly when the models are used.
# It is executed on package startup, and can't be executed for testing
# since they are already in the parsnip model database.

# coverage stats for this reason.

# nocov

bayesian_make <- function(modes = c("classification", "regression")) {
  model <- "bayesian"
  engine <- "brms"

  fitfunc <- c(pkg = "bayesian", fun = "bayesian_fit")
  predfunc <- c(pkg = "bayesian", fun = "bayesian_predict")

  dependpkgs <- unique(c("brms", fitfunc["pkg"], predfunc["pkg"]))
  dependpkgs <- dependpkgs[!is.na(dependpkgs)]

  parsnip::set_new_model(model)

  for (mode in modes) {
    parsnip::set_model_mode(model = model, mode = mode)

    # -------------------------------------------------------------------------

    parsnip::set_model_engine(model = model, mode = mode, eng = engine)

    # -------------------------------------------------------------------------

    for (pkg in dependpkgs) {
      parsnip::set_dependency(model, engine, pkg = pkg, mode = mode)
    }

    # -------------------------------------------------------------------------

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "formula.override",
      original = "formula.override",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "family",
      original = "family",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "prior",
      original = "prior",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "sample_prior",
      original = "sample_prior",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "knots",
      original = "knots",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "stanvars",
      original = "stanvars",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "fit",
      original = "fit",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "init",
      original = "init",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "chains",
      original = "chains",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "iter",
      original = "iter",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "warmup",
      original = "warmup",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "thin",
      original = "thin",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "cores",
      original = "cores",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "threads",
      original = "threads",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "algorithm",
      original = "algorithm",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "backend",
      original = "backend",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "stan_args",
      original = "stan_args",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "control",
      original = "control",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "save_pars",
      original = "save_pars",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "save_model",
      original = "save_model",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "file",
      original = "file",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "file_refit",
      original = "file_refit",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "normalize",
      original = "normalize",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "future",
      original = "future",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "seed",
      original = "seed",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "silent",
      original = "silent",
      func = fitfunc,
      has_submodel = FALSE
    )

    # -------------------------------------------------------------------------

    parsnip::set_fit(
      model = model,
      eng = engine,
      mode = mode,
      value = list(
        interface = "formula",
        protect = c("formula", "data", "weights"),
        func = fitfunc,
        defaults = list()
      )
    )

    # -------------------------------------------------------------------------

    parsnip::set_encoding(
      model = model,
      eng = engine,
      mode = mode,
      options = list(
        predictor_indicators = "none",
        compute_intercept = FALSE,
        remove_intercept = FALSE,
        allow_sparse_x = FALSE
      )
    )

    # -------------------------------------------------------------------------

    if (mode == "classification") {
      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "class",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(dim(results)) != 2) {
              rlang::warn('Use `type = "raw"` for multivariate predictions!')
              return(results)
            }
            threshold <- getOption("class_pred.threshold", 0.5)
            if (length(object$lvl) == 2) {
              if (is.array(results)) {
                results <- as.vector(results)
              }
              if (length(threshold) != 1) {
                rlang::abort("Probability threshold should be a single value.")
              }
              if (is.numeric(threshold)) {
                if (!dplyr::between(threshold, 0, 1)) {
                  rlang::abort("Probability threshold is out of 0-1 range.")
                }
              } else {
                rlang::abort("Probability threshold should be numeric.")
              }
              if (length(dim(results)) < 2) {
                res <- results
              } else {
                res <- results[, 1]
              }
              results <- ifelse(
                res >= threshold,
                object$lvl[2],
                object$lvl[1]
              )
            } else if (
              length(object$lvl) > 2 &&
                length(object$lvl) == ncol(results)
            ) {
              if (length(threshold) == ncol(results)) {
                results <- sweep(results, 2, threshold, FUN = "/")
              }
              results <- object$lvl[apply(results, 1, which.max)]
            } else {
              rlang::abort("Unexpected model predictions!")
            }
            unname(results)
          },
          func = predfunc,
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = TRUE
          )
        )
      )

      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "prob",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(dim(results)) != 2) {
              rlang::warn('Use `type = "raw"` for multivariate predictions!')
              return(results)
            }

            if (length(object$lvl) == 2) {
              if (is.array(results)) {
                results <- as.vector(results)
              }
              if (length(dim(results)) < 2) {
                res <- results
              } else {
                res <- results[, 1]
              }
              results <- tibble::tibble(
                v1 = 1 - res,
                v2 = res
              )
              colnames(results) <- object$lvl
            } else if (
              length(object$lvl) > 2 &&
                length(object$lvl) == ncol(results)
            ) {
              colnames(results) <- object$lvl
              results <- tibble::as_tibble(results)
            } else {
              rlang::abort("Unexpected model predictions!")
            }
            results
          },
          func = predfunc,
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = TRUE
          )
        )
      )

      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "conf_int",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(dim(results)) != 2) {
              rlang::warn('Use `type = "raw"` for multivariate predictions!')
              return(results)
            }

            lo_nms <- paste0(".pred_lower_", object$lvl)
            hi_nms <- paste0(".pred_upper_", object$lvl)
            se_nms <- paste0(".std_error_", object$lvl)
            if (length(object$lvl) == 2) {
              res_2 <-
                tibble::tibble(
                  lo = parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$conf_int$extras$level
                  ),
                  hi = parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$conf_int$extras$level,
                    lower = FALSE
                  )
                )
              res_1 <- res_2
              res_1$lo <- 1 - res_2$hi
              res_1$hi <- 1 - res_2$lo
              colnames(res_1) <- c(lo_nms[1], hi_nms[1])
              colnames(res_2) <- c(lo_nms[2], hi_nms[2])
              res <- dplyr::bind_cols(res_1, res_2)

              if (object$spec$method$pred$conf_int$extras$std_error) {
                res$.std_error <- apply(results, 2, stats::sd, na.rm = TRUE)
              }
            } else if (
              length(object$lvl) > 2 &&
                length(object$lvl) == ncol(results)
            ) {
              lo <- parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$conf_int$extras$level
              )
              colnames(lo) <- lo_nms
              lo <- tibble::as_tibble(lo)
              hi <- parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$conf_int$extras$level,
                lower = FALSE
              )
              colnames(hi) <- hi_nms
              hi <- tibble::as_tibble(hi)
              if (object$spec$method$pred$conf_int$extras$std_error) {
                se <- apply(results, 2, stats::sd, na.rm = TRUE)
                colnames(se) <- se_nms
                se <- tibble::as_tibble(se)
              } else {
                se <- tibble()
              }
              res <- dplyr::bind_cols(lo, hi, se)
            } else {
              rlang::abort("Unexpected model predictions!")
            }
            res
          },
          func = predfunc,
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            interval = "confidence",
            level = rlang::expr(level),
            summary = FALSE
          )
        )
      )

      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "pred_int",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(dim(results)) != 2) {
              rlang::warn('Use `type = "raw"` for multivariate predictions!')
              return(results)
            }

            lo_nms <- paste0(".pred_lower_", object$lvl)
            hi_nms <- paste0(".pred_upper_", object$lvl)
            se_nms <- paste0(".std_error_", object$lvl)
            if (length(object$lvl) == 2) {
              res_2 <-
                tibble::tibble(
                  lo = parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$pred_int$extras$level
                  ),
                  hi = parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$pred_int$extras$level,
                    lower = FALSE
                  )
                )
              res_1 <- res_2
              res_1$lo <- 1 - res_2$hi
              res_1$hi <- 1 - res_2$lo
              colnames(res_1) <- c(lo_nms[1], hi_nms[1])
              colnames(res_2) <- c(lo_nms[2], hi_nms[2])
              res <- dplyr::bind_cols(res_1, res_2)

              if (object$spec$method$pred$pred_int$extras$std_error) {
                res$.std_error <- results$se.fit
              }
            } else if (
              length(object$lvl) > 2 &&
                length(object$lvl) == ncol(results)
            ) {
              lo <- parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$pred_int$extras$level
              )
              colnames(lo) <- lo_nms
              lo <- tibble::as_tibble(lo)
              hi <- parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$pred_int$extras$level,
                lower = FALSE
              )
              colnames(hi) <- hi_nms
              hi <- tibble::as_tibble(hi)
              if (object$spec$method$pred$pred_int$extras$std_error) {
                se <- results$se.fit
                colnames(se) <- se_nms
                se <- tibble::as_tibble(se)
              } else {
                se <- tibble()
              }
              res <- dplyr::bind_cols(lo, hi, se)
            } else {
              rlang::abort("Unexpected model predictions!")
            }
            res
          },
          func = predfunc,
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            interval = "prediction",
            level = rlang::expr(level),
            summary = FALSE
          )
        )
      )
    } else {
      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "numeric",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(dim(results)) != 2) {
              rlang::warn('Use `type = "raw"` for multivariate predictions!')
              return(results)
            }

            if (length(dim(results)) < 2) {
              res <- results
            } else {
              res <- results[, 1]
            }

            tibble::tibble(.pred = res)
          },
          func = predfunc,
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = TRUE
          )
        )
      )

      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "conf_int",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(dim(results)) != 2) {
              rlang::warn('Use `type = "raw"` for multivariate predictions!')
              return(results)
            }

            res <-
              tibble::tibble(
                .pred_lower = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$conf_int$extras$level
                ),
                .pred_upper = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$conf_int$extras$level,
                  lower = FALSE
                ),
              )

            if (object$spec$method$pred$conf_int$extras$std_error) {
              res$.std_error <- apply(results, 2, stats::sd, na.rm = TRUE)
            }
            res
          },
          func = predfunc,
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            interval = "confidence",
            level = rlang::expr(level),
            summary = FALSE
          )
        )
      )

      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "pred_int",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(dim(results)) != 2) {
              rlang::warn('Use `type = "raw"` for multivariate predictions!')
              return(results)
            }

            res <-
              tibble::tibble(
                .pred_lower = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$pred_int$extras$level
                ),
                .pred_upper = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$pred_int$extras$level,
                  lower = FALSE
                ),
              )

            if (object$spec$method$pred$pred_int$extras$std_error) {
              res$.std_error <- apply(results, 2, stats::sd, na.rm = TRUE)
            }
            res
          },
          func = predfunc,
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            interval = "prediction",
            level = rlang::expr(level),
            summary = FALSE
          )
        )
      )
    }

    parsnip::set_pred(
      model = model,
      eng = engine,
      mode = mode,
      type = "raw",
      value = list(
        pre = NULL,
        post = NULL,
        func = predfunc,
        args = list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data)
        )
      )
    )

    parsnip::set_pred(
      model = model,
      eng = engine,
      mode = mode,
      type = "quantile",
      value = list(
        pre = NULL,
        post = function(results, object) {
          if (length(dim(results)) != 2) {
            rlang::warn('Use `type = "raw"` for multivariate predictions!')
            return(results)
          }

          tibble::as_tibble(results)
        },
        func = predfunc,
        args = list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          summary = TRUE
        )
      )
    )
  }
}

# nocov end
