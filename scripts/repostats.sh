set -e

out=$(mktemp)

cat <<EOF>$out
Hi, this is how your vcs repos' stata look like:

EOF

find /igk -name '.hg' -type d | grep -vi attic | while read repo; do
	(cd $repo/..; echo $PWD \(hg\); hg st) >> $out;
done

find /igk -name '.git' -type d | grep -vi attic | while read repo; do
	(cd $repo/..; echo $PWD \(git\); git status -s) >> $out;
done

if [ x$DEBUG = xy ]; then
    less $out
else
    mail -s 'Status of your local VCS repos' -t $MAILTO < $out
fi
