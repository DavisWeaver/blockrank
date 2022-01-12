#! bin/bash

ssh -i ~/.ssh/aws-dev.cer ubuntu@ec2-52-21-108-142.compute-1.amazonaws.com "cd blockrank; git pull"