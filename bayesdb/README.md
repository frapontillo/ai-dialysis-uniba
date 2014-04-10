BayesDB
========

## Step 1: prepare BayesDB

Get **[BayesDB](http://probcomp.csail.mit.edu/bayesdb/)** and follow the instructions on their website to be up and running.

**Advice**: you can easily try BayesDB by downloading a [ready-for-use VirtualBox VM](http://probcomp.csail.mit.edu/bayesdb/#Download).

## Step 2: export and import the data

### Export from MySQL

Open MySQL Workbench and execute the query in the `bayesdb/learn_data_export.sql` script file.

This will save all learning and testing data (with headers) in a `csv` file in your MySQL data directory (e.g. under Windows 8: `C:\ProgramData\MySQL\MySQL Server 5.6\data\dialysisai`). Data will be composed of:
	* 100 positive examples
	* 100 negative examples
	* 100 fake-unknown examples (same positives as before)
	* 100 fake-unknown examples (same negative as before) 

### Import into BayesDB

To send the generated `csv` file to the BayesDB VM you need to use `scp` and the private key provided in the BayesDB VM.

Then, copy the following files:
	* `learn_data.csv`
	* `learn_data_import.py`
	* `learn_data_test.py`

Under Linux/Mac OS X:
```shell
$ scp -r -i vm_guest_id_rsa -p 2222 -o StrictHostKeyChecking=no path/to/file bayesdb@localhost:/home/bayesdb/file
```

Under Windows, use WinSCP to set up a SCP connection with the following parameters:
	* server: `localhost`
	* port: `2222`
	* username: `bayesdb`
	* password: you can find it in the BayesDB VM readme file
	* key: use the one provided in the BayesDB VM package, `vm_guest_id_rsa` (ignore all complaints about WinSCP not being compatible with Putty)

Then, execute:
```shell
$ python /home/bayesdb/learn_data_import.py
```

If all is well, the `csv` file will be imported into BayesDB, the model will be created and analyzed (100 iterations). This step will take some time.

**Note**: after importing and analyzing data into BayesDB, you can export a ready-for-use file by executing the following Python code:

```python
from bayesdb.client import Client
client = Client()
client('EXPORT SAMPLES FROM dialysisai TO dialysisai')
```

## Step 3: classify data

All of the following queries must be executed by a python shell, e.g.:
```python
from bayesdb.client import Client
client = Client()
client('SELECT * FROM dialysisai')
```

### Estimate dependence probabilities

We can estimate all dependence probabilities:
```
ESTIMATE DEPENDENCE PROBABILITIES FROM dialysisai;
```

We can also estimate the dependence probabilities for the target attribute with a given confidence value:
```
ESTIMATE DEPENDENCE PROBABILITIES FROM dialysisai REFERENCING symptom_id WITH CONFIDENCE 0.9;
```
to get a good idea of which attributes are most related to the symptom ID.

### Infer symptoms

To execute a test classification on the test set, execute the query:
```
INFER symptom_id FROM dialysisai WITH CONFIDENCE 0.9;
```

### Evaluate the prediction model

To evaluate the prediction model provided by BayesDB, we can calculate:
	* TP Rate
	* TN Rate
	* FP Rate
	* FN Rate
	* accuracy
	* recall
	* F-Measure

To do that, execute on the BayesDB virtual machine the following:

```shell
$ python learn_data_test.py
```

The script will output the test results and save them to a `learn_data_test.log` file.