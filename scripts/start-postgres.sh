#!/bin/bash

# Start a local instance of PostgreSQL using Docker

sudo docker run -d -p 5432:5432 -e POSTGRES_USER=test -e POSTGRES_PASSWORD=test postgres:alpine
