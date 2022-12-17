#!/bin/bash

set -ex

sudo apt update
sudo apt upgrade -y
sudo apt install emacs -y

emacs --version
