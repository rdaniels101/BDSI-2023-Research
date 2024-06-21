sa_ctrl <- safsControl(functions = rfSA,
                       method="cv", number=10)
set.seed(07272023)
rf_sa <- safs(x = pathway.scores_train, y = P_train,
              iters = 10,
              safsControl = sa_ctrl)

ctrl <- safsControl(functions = caretSA,
                    method="cv", number=10)
lm_sa <- safs(x = pathway.scores_train, y = P_train,
              iters = 10,
              safsControl = sa_ctrl,
            ## Now pass options to `train`
            method = "lm")
