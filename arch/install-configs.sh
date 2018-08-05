# install-configs.sh

for file in $(find etc/ -type f -not -name \*.in); do
    printf "Installing $file -> /$file... "
    cp -Pv --preserve=mode --backup=numbered $file /$file || echo Failed\!
done
