# setup.sh --- post-install setup

crontab cron/$(hostname).crontab

update-desktop-database ./share/applications/

# scripts won't be in $PATH before reloading profile, so stop bugging me
# 'bout it ffs.
pip3 install --no-warn-script-location -r requirements.txt
gem install --user-install bundler

. lib/profile/paths.sh

bundle
bundle update --bundler
gem rdoc --all

