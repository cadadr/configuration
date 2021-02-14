# install-configs.bash

for file in $(find etc/ -type f -o -type l -not -name \*.in); do
    mkdir -vp "$(dirname /$file)"  || echo Failed\!
    cp -Pv --preserve=mode --backup=numbered "$file" "/$file" || echo Failed\!
done
