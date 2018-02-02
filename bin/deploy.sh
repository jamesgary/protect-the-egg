#!/bin/bash
aws s3 cp public s3://<YOUR BUCKET NAME HERE>.com/ --recursive --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers
