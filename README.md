# Distribution_tool

#THIS IS NOW WORKING ON OCAL HOST WITH THESE FILES AND DOCKERFILE COMMANDS

docker build -t distribution_tool . ### BUILDS IMAGE
docker run -d --rm -p 5532:5532 distribution_tool ## RUNS IMAGE IN CONTAINER

