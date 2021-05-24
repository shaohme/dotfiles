#!/bin/bash
set -x
# set -e

# MAILDIR=$HOME/MyMail

# # Move a message file while removing its UID-part
# function safeMove { s=${1##*/}; s=${s%%,*}; mv -f $1 $2/$s; }

# # Move all deleted messages to the Trash folder
# echo Moving $(notmuch count --output=files --exclude=false tag:deleted AND NOT folder:Trash) \
#      deleted messages to the Trash folder
# for i in $(notmuch search --exclude=false --output=files tag:deleted AND NOT folder:Trash); do
#     safeMove $i ${MAILDIR}/Trash/cur
# done

# # Move all spam messages to the Spam folder
# echo Moving $(notmuch count --output=files tag:spam AND NOT folder:Spam) \
#      spam-marked messages to the Spam folder
# for i in $(notmuch search --output=files tag:spam AND NOT folder:Spam); do
#     safeMove $i ${MAILDIR}/Spam/cur
# done

# # Move all archived messages from Inbox to Archive folder
# echo Moving $(notmuch count --output=files folder:Inbox AND NOT tag:inbox) \
#      archived messages from Inbox to Archive folder
# for i in $(notmuch search --output=files folder:Inbox AND NOT tag:inbox); do
#     safeMove $i ${MAILDIR}/Arkiv/cur
# done

# echo Syncing with Webmail
# mbsync twoway

# echo Updating notmuch database
# notmuch new --no-hooks


mbsync -a

# mu index

# notmuch new

# notmuch tag --input=$HOME/.tagmail.notmuch

# immediately archive all messages from "me"
# notmuch tag -new -- tag:new and from:me@example.com

# delete all messages from a spammer:
# notmuch tag +deleted -- tag:new and from:spam@spam.com

# tag all message from notmuch mailing list
# notmuch tag +notmuch +list -- tag:new and to:notmuch@notmuchmail.org
# notmuch tag +emacs-devel +list -- tag:new and to:emacs-devel@gnu.org

# finally, retag all "new" messages "inbox" and "unread"
# notmuch tag +inbox +unread -new -- tag:new
