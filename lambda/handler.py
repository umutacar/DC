import json,os,subprocess

def mtlcompile(event, context):
    # TODO project name (corresponding to the name of the folder where the
    # compiler should be executed) should be part of the input (as well as
    # the actual project content - e.g., base64-encoded tgz file)
    project = "genome"
    ROOT = os.environ["LAMBDA_TASK_ROOT"]
    # update PATH so that pandoc can be found
    os.environ["PATH"] = os.path.join(ROOT,"bin")+":"+os.environ["PATH"]
    COMPILER = "texmlt.native"
    MAIN = "main.tex"
    PREAMBLE = "preamble.tex"
    OUTPUT = os.path.join("/tmp",project+".xml")
    cmd = "%s %s -preamble %s -o %s" %\
          (os.path.join(ROOT,"bin",COMPILER), MAIN, PREAMBLE, OUTPUT)
    p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                       cwd=project, timeout=30, shell=True)
    output = {"return": p.returncode,
              "stdout": p.stdout.decode("utf-8"),
              "stderr": p.stderr.decode("utf-8"),
             }
    body = {}
    body["output"] = output
    with open(OUTPUT) as f:
        body["result"] = f.read()
    response = {
        "statusCode": 200,
        "body": json.dumps(body)
    }

    return response
