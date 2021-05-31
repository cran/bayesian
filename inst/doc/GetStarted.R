## ----results='hide', message=FALSE, warning=FALSE-----------------------------
library(bayesian)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
library(recipes)
library(workflows)

## -----------------------------------------------------------------------------
epi_recipe <- epilepsy %>%
  recipe() %>%
  update_role(count, new_role = "outcome") %>%
  update_role(Trt, Age, Base, patient, new_role = "predictor") %>%
  add_role(patient, new_role = "group") %>%
  step_normalize(Age, Base)

## -----------------------------------------------------------------------------
print(epi_recipe)

## -----------------------------------------------------------------------------
epi_model <- bayesian(
    family = poisson()
  ) %>%
  set_engine("brms") %>%
  set_mode("regression")

## -----------------------------------------------------------------------------
print(epi_model)

## -----------------------------------------------------------------------------
epi_model <- epi_model %>%
  update(family = poisson())

## -----------------------------------------------------------------------------
epi_workflow <- workflow() %>%
  add_recipe(epi_recipe) %>%
  add_model(
    spec = epi_model,
    formula = count ~ Trt + Base + Age + (1 | patient)
  )

## -----------------------------------------------------------------------------
print(epi_workflow)

## ----results='hide', echo = FALSE---------------------------------------------
run_on_linux <- grepl("linux", R.Version()$os, ignore.case = TRUE)

## ----results='hide', eval = run_on_linux--------------------------------------
epi_workflow_fit <- epi_workflow %>%
  fit(data = epilepsy)

## ----eval = run_on_linux------------------------------------------------------
print(epi_workflow_fit)

## ----eval = run_on_linux------------------------------------------------------
epi_fit <- epi_workflow_fit %>%
  pull_workflow_fit()

## ----eval = run_on_linux------------------------------------------------------
epi_brmsfit <- epi_fit$fit

## ----eval = run_on_linux------------------------------------------------------
class(epi_brmsfit)

## -----------------------------------------------------------------------------
newdata <- epilepsy[1:5, ]

## ----eval = run_on_linux------------------------------------------------------
epi_workflow_fit %>%
  predict(
    new_data = newdata,
    type = "conf_int"
  )

