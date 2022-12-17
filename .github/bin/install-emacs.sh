#!/bin/bash

set -ex

apt update
apt upgrade -y
apt install emacs -y

emacs --version
