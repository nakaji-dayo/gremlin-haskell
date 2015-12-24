cred=~/.stack/upload/credentials.json
if [ ! -f "${cred}" ]; then
    if [ ! -e ~/.stack/upload ]; then
	mkdir -p ~/.stack/upload
    fi
    echo "{\"username\":\"${HACKAGE_USERNAME}\",\"password\":\"${HACKAGE_PASSWD}\"}" > ${cred}
fi
