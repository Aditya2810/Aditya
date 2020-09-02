id: unique identifier of a record. Not to be used as a feature
Col_* : Features to be used in the learning algorithm. You may use all or some of the features to your advantage
target : Binary (0/1) dependent variable

The dependent variable consists of lots of 0's and very less 1's(imbalanced dataset). The columns are masked to prevent disclosing the identity of the dataset.

Recursive feature extraction coupled with cross validation was employed to extract the necessary features. Outlier analysis, scaling and other data pre-processing steps were completed before running the logistic, SVM and random forest alogorithms for prediction. A thorough EDA was performed.

Confusion matrix with different scores were obtained to evaluate model performnce. I focused on recall score as the aim of the dataset to predict the positive class in the dataset. The ROC/AUC curves are also plotted.

Different boosting methods such as gradient boost and AdaBoost were employed to improve model performance.
