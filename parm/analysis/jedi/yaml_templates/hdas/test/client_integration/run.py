# This script is based on
# global-workflow/ush/python/pygfs/task/analysis.py

import yaml
from jcb import render
from wxflow import (parse_j2yaml, FileHandler, rm_p, logit,
                    Task, Executable, WorkflowException, to_fv3time, to_YMD,
                    Template, TemplateConstants)

jedi_yaml = "_TARGET_YAML_"

# Step 1: fill templates of the jcb base YAML file (use predefined for now)
jcb_config = "hdas-atmosphere-templates.yaml"
with open(jcb_config, "r") as yaml_file:
    task_config = yaml.safe_load(yaml_file)
jcb_config = parse_j2yaml(jcb_config, task_config)

# Step 2: (optional) fill templates of algorithm override YAML and merge
#jcb_config['algorithm'] = algorithm

# Step 3: generate the JEDI Yaml using JCB driving YAML
jedi_config = render(jcb_config)

with open(jedi_yaml, 'w') as f:
    yaml.dump(jedi_config, f, default_flow_style=False, sort_keys=False)
