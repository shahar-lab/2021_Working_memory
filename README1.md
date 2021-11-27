# 0004_Ido_Set-size_effects
This experiment is aimed at studying the effect of WM load manipulation over outcome-irrelevant RL.
You may find here a simple simulation and a replication of a similar paradigm.
Under new_exp folder you will find the JavaScript implementation of the paradigm for an online experiment using jspsych.

The raw_data of the subjects in json format appears in myfolder->data->raw_data
All the functions needed to clean the data appear in myfolder->functions->.
To convert the json raw_data use the convert_json functions.
The aggregated data of the subjects appears in myfolder->data->agg_data->agg_not_clean
To clean the data use the clean functions.
To prepare the data use the prepare_data function.
Some basic analysis may be found under "descriptive_analysis"
The Bayesian models may be found under regression_analysis.
Demographic data may be found in the data folder.
=======
This experiment is aimed at studying <b>the effect of WM load manipulation on outcome-irrelevant value assignment</b>.

You will find here a JavaScript implementation of the paradigm containing a <b> dual task of a  2-armed bandit and a change detection task</b>.

You will also find here a simple simulation and a replication of a conceptually similar paradigm.

Under <b>experiment_01_21</b> folder you will find the JavaScript implementation of the paradigm for an online experiment using jspsych.

The 01/21 experiment is described here: https://osf.io/kchq4

Under <b>replication_06_21</b> folder you will find the JS implementation of the new paradigm for the second experiment in this project.

The 06/21 experiment is described here: https://osf.io/6cz29

The <b> WM_RL_2017 </b> folder contains a replication of a basic TD(0) model.

The replication (pdf in folder) is based on: https://pubmed.ncbi.nlm.nih.gov/28651789/

Finally, you can also use the data editing file which helps you organize the raw data you get from the server.
