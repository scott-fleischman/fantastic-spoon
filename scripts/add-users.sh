#!/bin/bash

# With the server running, use curl to create some users.
# Note: this is not needed anymore because the server itself will create data upon startup if not present.

curl --header "Content-Type: application/json" --request POST --data '{"userName":"peter","userPassword":"pwd","userAge":40}' http://localhost:3000/user
curl --header "Content-Type: application/json" --request POST --data '{"userName":"frank","userPassword":"pwd","userAge":18}' http://localhost:3000/user
curl --header "Content-Type: application/json" --request POST --data '{"userName":"bob","userPassword":"pwd","userAge":45}' http://localhost:3000/user
curl --header "Content-Type: application/json" --request POST --data '{"userName":"zachary","userPassword":"pwd","userAge":20}' http://localhost:3000/user
curl --header "Content-Type: application/json" --request POST --data '{"userName":"emily","userPassword":"pwd","userAge":25}' http://localhost:3000/user
curl --header "Content-Type: application/json" --request POST --data '{"userName":"megan","userPassword":"pwd","userAge":39}' http://localhost:3000/user
curl --header "Content-Type: application/json" --request POST --data '{"userName":"elizabeth","userPassword":"pwd","userAge":30}' http://localhost:3000/user
curl --header "Content-Type: application/json" --request POST --data '{"userName":"katie","userPassword":"pwd","userAge":28}' http://localhost:3000/user
