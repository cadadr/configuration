# install-configs.sh

for file in $(find etc/ -type f -o -type l -not -name \*.in); do
    mkdir -p $(dirname /$file)
    cp -Pv --preserve=mode --backup=numbered $file /$file || echo Failed\!
done
