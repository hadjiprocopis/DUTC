#!/bin/bash

#rm -rf push_to_GIT
mkdir push_to_GIT >& /dev/null
cp -r bin lib test push_to_GIT
cp -r original_data cleaned_data push_to_GIT
cp -r correlation_between_2_columns_histogram_based push_to_GIT
#cp -r seasonality_analysis push_to_GIT
cp -r RMARKDOWN_IN RMARKDOWN_OUT_REPORTS push_to_GIT
cp README.md push_to_GIT
cd push_to_GIT
cat>.gitignore<<EOC
ABOUT-NLS
config.*
.libs/
.deps/
*.a
*.o
*.so.*
*.la
*.in
EOC

git init
git add .
git commit -am "first commit"
SSH_AUTH_SOCK= git push -f git@github.com:hadjiprocopis/DUTC.git --all --verbose
