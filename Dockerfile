FROM       ubuntu:20.04
MAINTAINER Tor Henrik Hanken <torhenrik@gmail.com>

# OS
RUN apt-get update && apt-get install -y sbcl

# Copy files
COPY . /app
WORKDIR /app

# Startup 

CMD /usr/bin/sbcl --load "/app/elevator.lisp" --eval "(elevator-test)"
