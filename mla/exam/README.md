# `code.zip` contents

The attached `code.zip` contains:

* code solutions to question 4 `q4.py`, including a pre-trained pytorch model in
  `code/linreg_pytorch_model.pt`.
* code solutions to question 5 in `q5.py`.
* a directory `data/` containing all of the training and testing data used in
    question 4 and question 5.
* a python module `my_stuff.py` containing my own implementations of PCA,
    k-means, k-NN (including grid search), and linear regression

# How to run

To run my code and reproduce my results, first decompress `code.zip`; then, from
within the `code/` directory, use `python run.py` to run all code solutions to
both question 4 and question 5.

To run code for question 4 or 5 individually, use `python q4.py` and `python
q5.py`, respectively.

To reproduce the k-NN grid search for optimal `k`, use
`python q4.py --do_gridsearch_k`.
