#!/bin/bash

# New York
LOC="40.7142:-74.0064"
# Stockholm
#LOC="59.3300:18.0700"
# Tel Aviv
#LOC="32.0833:34.8000"
if [ -x /usr/bin/redshift ]; then
redshift -x
killall redshift
#redshift -l $LOC -t 5500:5000 &
redshift -l $LOC -t 5500:4700 &
else
echo "redshift not found"
fi
