#!/usr/bin/env bash
eval $(ssh-agent -s)
ssh-add ~/.ssh/id_rsa
# ssh-add ~/.ssh/id_rsa.pub
ssh-add ~/.ssh/aws_anma04.pem

# ssh ubuntu@34.247.136.231 -t "/usr/bin/python3 $@"
ssh root@54.171.123.29 -t "/usr/bin/python3 $@"
# ssh ubuntu@34.247.136.231 -t "/home/anma04/anaconda3/bin/python $@"
# ssh anma04@34.247.136.231 -t "/home/anma04/anaconda3/bin/python $@"
# ssh ubuntu@52.30.223.42 -t "python3 $@"
# ssh -o StrictHostKeyChecking=no -i "C:\workspace\aws_anma04.pem" ubuntu@54.171.123.29 python3
