import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pickle

from sklearn.ensemble import RandomForestRegressor,HistGradientBoostingClassifier
from sklearn.ensemble import RandomForestClassifier

from sklearn.experimental import enable_halving_search_cv
from sklearn.model_selection import HalvingRandomSearchCV,GridSearchCV
from sklearn.metrics import ConfusionMatrixDisplay, confusion_matrix

###########################################################
### main
def main():
    data=pd.read_csv('X.csv')
    # info=pd.read_csv('info.csv')
    # reduced=pd.read_csv('reduced.csv')
    X=np.c_[data.to_numpy()[:,1:]]

    target=pd.read_csv('Lead.csv')
    y = target['lag'].to_numpy()

    ix1 = y<1
    ix2 = y>0
    training= ix1*ix2

    estimator = HistGradientBoostingClassifier()


    year=data['year'].to_numpy()


    y = np.delete(y, training, 0)

    y=y>1

    X = np.delete(X, training, 0)
    year = np.delete(year, training, 0)

    training=year!=2021
    testing=year==2021

    y_train=y[training]
    y_test=y[testing]
    #
    X_train=X[training,:]
    X_test=X[testing,:]


    ##### Hyperparameters
    max_leaf_nodes = np.arange(11, 101, 5).tolist()
    max_leaf_nodes.append(None)
    param_distributions = {
        "learning_rate": np.linspace(.05, 2, num=200).tolist(),
        "max_leaf_nodes": max_leaf_nodes,
        "min_samples_leaf": [10, 20, 30, 40, 50, 60],
        "l2_regularization": np.linspace(.0, 3., num=20).tolist()
    }

    # cv = GridSearchCV(
    #     estimator=estimator,
    #     cv=5,
    #     param_grid=param_distributions,
    #     n_jobs=-1,
    #     error_score='raise')

    n_iter = 150
    cv = HalvingRandomSearchCV(
        estimator=estimator,
        cv=5,
        param_distributions=param_distributions,
        max_resources=n_iter,
        n_jobs=-1,
        error_score='raise')

    cv_results = cv.fit(X_train, y_train)
    estimator_hat = cv_results.best_estimator_

    # file = open('model', 'wb')
    # pickle.dump(estimator_hat, file)
    # file.close()

    y_pred = estimator_hat.predict(X_train)
    y_true = y_train

    confmx = confusion_matrix(y_true, y_pred)
    disp = ConfusionMatrixDisplay(confusion_matrix=confmx)
    disp.plot()
    plt.savefig('confusionmatrix_train.png')
    plt.close()

    y_pred = estimator_hat.predict(X_test)
    y_true = y_test

    print(np.mean(y_true==y_pred))

    confmx = confusion_matrix(y_true, y_pred)
    disp = ConfusionMatrixDisplay(confusion_matrix=confmx)
    disp.plot()
    plt.savefig('confusionmatrix_test.png')
    plt.close()


###########################################################
### start main
if __name__ == "__main__":
    main()