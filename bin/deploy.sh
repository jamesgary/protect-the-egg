#!/bin/bash
aws s3 cp public s3://elm-game-jam-feb-2017 --recursive --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers
