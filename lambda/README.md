# Deploying MTL TEX2XML Compiler on Lambda

## Prerequisites

 - serverless.com framework (`npm install -g serverless`)
 - notice that the `bin` folder currently contains the compiler binary (which may need to be updated as the compiler code evolves)

## Deploying the Lambda function

```
$ sls deploy
```
Note: first time it will take few minutes. consequent redeployments will take about 30 seconds

The Lambda function is configured with an HTTP trigger. Observe the reported endpoint URL.

## Verify that it works

Open the reported endpoint in the browser. The resulting JSON comprising stdout as well as the resulting XML will be returned in 10-15 seconds.

## Making changes
Once you make changes (handler, yaml manifest, binaries, etc) - just run `sls deploy` again

# Troubleshooting
If you think that the function is not working properly, refer to CloudWatch logs (e.g., accessible via the Lambda Web console).

