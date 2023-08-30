import optuna
from optuna.samplers import TPESampler
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix
from sklearn.metrics import ConfusionMatrixDisplay
from sklearn.experimental import enable_halving_search_cv
from sklearn.model_selection import HalvingRandomSearchCV,RandomizedSearchCV
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from sklearn.linear_model import LinearRegression






import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.metrics import accuracy_score, precision_score, recall_score
from sklearn.model_selection import cross_val_score

def impute_values(X: pd.DataFrame):
    imputer = IterativeImputer(estimator=LinearRegression())
    X_imputed = imputer.fit_transform(X)
    return pd.DataFrame(X_imputed, columns=X.columns)
def classification_metrics(y_test, y_pred):
    accuracy = accuracy_score(y_test, y_pred)
    precision = precision_score(y_test, y_pred, average='macro')  # or use None if we want to check for all classes
    recall = recall_score(y_test, y_pred, average='macro')

    print("Accuracy:", accuracy)
    print("Precision:", precision)
    print("Recall:", recall)

    return accuracy, precision, recall


def est_performance(model, X_train, X_test, y_train, y_test):
    """
    Purpose: Estimate the performance of the models using rolling-origin-update evaluation and
             rolling-origin-recalibration evaluation
    Args:
        model: model to make predictions with
        X_train: Training data independent variables
        X_test: Test data independent variables
        y_train: Training data dependent / explanatory variables
        y_test: Test data dependent / explanatory variables
        classification (bool): If True use metrics for classification, if false use metrics for regression

    Returns:
        results (dictionary): evaluation metrics

    """

    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    accuracy, precision, recall = classification_metrics(y_test, y_pred)

    return accuracy, precision, recall


def objective_rf(trial, X_train, y_train, cv_folds, n_estimators, classification=False):
    max_mtry = X_train.shape[1]
    param_grid = {'max_features': trial.suggest_int('max_features', 1, max_mtry, 2)}

    if classification:
        estimator = RandomForestClassifier(n_estimators=n_estimators, **param_grid)
        cv_scores = cross_val_score(estimator, X_train, y_train, scoring='weighted-average', cv=cv_folds)
        metric = np.mean(cv_scores)

    return metric


def perform_optuna_rf(X_train, y_train, X_test, y_test, cv_folds, n_estimators=500, n_trials=100):
    sampler = TPESampler(seed=10)
    study = optuna.create_study(direction='maximize', sampler=sampler)

    study.optimize(lambda trial: objective_rf(trial, X_train, y_train, cv_folds, n_estimators),
                   n_trials=n_trials)
    print("Best trial:")
    trial = study.best_trial

    print("  Value: {}".format(trial.value))
    print("  Params: ")
    for key, value in trial.params.items():
        print("    {}: {}".format(key, value))

    best_params = trial.params
    rmse_train = trial.value

    model = RandomForestClassifier(n_estimators=n_estimators, max_features=best_params['max_features'])
    results = est_performance(model, X_train, X_test, y_train, y_test)

    results['rmse_train'] = rmse_train

    return results




############ Feature importance
def calculate_feature_importance(model, X_train, y_train, n_features_show):
    # TODO: Check which measures are used to calculate the feature importance
    model.fit(X_train, y_train)
    feature_importances = pd.Series(model.feature_importances_, index=X_train.columns).sort_values(ascending=False)
    most_important_features = feature_importances.iloc[0:n_features_show]

    plt.figure(figsize=(10, 6))  # Set the figure size to (10, 6) inches (or any other desired value)

    # Create bar plot
    most_important_features.plot.bar()

    # Set x-axis label font size and rotation
    plt.xticks(fontsize=10, rotation=45, ha='right')  # Set the font size, rotation, and alignment of x-axis labels

    # Set plot title and axis labels
    plt.title('Feature Importances')
    plt.xlabel('Features')

    return



######### From ML.py

def main():
    data=pd.read_csv('X.csv')
    data2 = impute_values(data)
    # info=pd.read_csv('info.csv')
    reduced=pd.read_csv('reduced.csv')
    X=data2

    target=pd.read_csv('y.csv')
    y = target['target'].to_numpy()

    estimator = RandomForestClassifier()


    year=data['year'].to_numpy()
    training=year!=2022
    testing=year==2022

    y_train=y

    X_train=X


    ##### Hyperparameters
    max_leaf_nodes = np.arange(11, 101, 10).tolist()
    max_leaf_nodes.append(None)
    param_distributions = {'bootstrap': [True, False],
    'max_depth': [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, None],
    'max_features': ['sqrt'],
    'min_samples_leaf': [1, 2, 4],
    'min_samples_split': [2, 5, 10],
    'n_estimators': [200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000]}

    n_iter=700
    cv = RandomizedSearchCV(
        estimator=estimator,
        cv=5,
        param_distributions=param_distributions,
        n_jobs=-1,
        error_score='raise')

    cv_results = cv.fit(X_train, y_train)
    estimator_hat = cv_results.best_estimator_

    n_features_show=10

    feature_importances = pd.Series(estimator_hat.feature_importances_, index=X_train.columns).sort_values(ascending=False)
    most_important_features = feature_importances.iloc[0:n_features_show]

    plt.figure(figsize=(10, 6))  # Set the figure size to (10, 6) inches (or any other desired value)

    # Create bar plot
    most_important_features.plot.bar()

    # Set x-axis label font size and rotation
    plt.xticks(fontsize=10, rotation=45, ha='right')  # Set the font size, rotation, and alignment of x-axis labels

    # Set plot title and axis labels
    plt.title('Feature Importances')
    plt.xlabel('Features')
    plt.show()

    #
###########################################################
### start main
if __name__ == "__main__":
    main()



