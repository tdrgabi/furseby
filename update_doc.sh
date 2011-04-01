#! /bin/bash

if [ -d gh-pages ]
then
  cd gh-pages
fi

git commit -am "documentation update"
git push git@github.com:tdrgabi/furseby.git
