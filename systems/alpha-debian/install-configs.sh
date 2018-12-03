# install-configs.sh

for file in $(find etc/ -type f -o -type l -not -name \*.in); do
    mkdir -p $(dirname /$file)
    cp -Pvu --preserve=mode --backup=numbered $file /$file || echo Failed\!
done

# Update system
locale-gen
update-initramfs -u
