# setup.sh --- post-install setup

crontab cron/$(hostname).crontab

update-desktop-database ~/.local/share/applications/

pip3 install -r requirements.txt
gem install --user-install bundler

. lib/profile/paths.sh

bundle
bundle update --bundler
gem rdoc --all

