#!/bin/bash

git log --pretty=format:"## %h(%H)%n%an(%ae)%n%B%n" ${@}

