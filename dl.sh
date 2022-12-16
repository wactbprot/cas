#!/bin/sh

P="couchapp/_attachments"

# css
curl https://cdn.jsdelivr.net/npm/uikit@3.7.0/dist/css/uikit.min.css -o $P/css/uikit.css
# js
curl https://code.jquery.com/jquery-3.6.0.min.js -o $P/js/jquery.js
curl https://cdn.jsdelivr.net/npm/uikit@3.7.0/dist/js/uikit.min.js -o $P/js/uikit.js
curl https://cdn.jsdelivr.net/npm/uikit@3.7.0/dist/js/uikit-icons.min.js -o $P/js/uikit-icons.js
