# install raspberry pi configuration.

boot=/media/g/boot
root=/media/g/734d7a6d-e6b2-4d91-8978-192f695c8e5e

# Enable sshd on pi.
touch $boot/ssh

for f in $( (cd root; find . -type f) ); do
    mkdir -vp $(dirname $root/$f)
    install -Cv -m 644 -o root -g root root/$f $root/$f
done
