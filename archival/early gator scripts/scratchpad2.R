mt = mtcars
mt$cyl = factor(mt$cyl)

mod = lm(mpg ~ disp + cyl, data = mt)

mod.betas = coef(mod)
mod.vcov = vcov(mod)

# simulate betas

newdata = mt[1:4, ]
newdata$mpg = newdata$mpg + rnorm(4)
newdata$disp = newdata$disp + rnorm(4)

new.mm = model.matrix(object = mpg ~ disp + cyl, data = newdata)

new.mm

simulation = new.mm %*% mod.betas
simulation

### real example

arm::invlogit(cbind(1, as.matrix(nd)) %*% coeffs[1, 1:5, 1])
arm::invlogit(cbind(1, as.matrix(nd)) %*% coeffs[1, 1:5, 2])

test_data_mm = model.matrix(object = outcome ~ sex + age + income + iq,
                    data = test_data)

add_logit_mm = multinom(outcome ~ sexfemale + age + income + iq, 
                        data = test_data, Hess=TRUE)


predict(add_logit, newdata = nd)
