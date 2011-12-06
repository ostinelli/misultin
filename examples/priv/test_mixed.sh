curl -k --header "Content-Type: multipart/mixed" \
        --header "MIME-version: 1.0" \
        --form "fileupload=@sample.xml;type=text/xml" \
        --form "fileupload2=@sample.wav;type=audio/wav" \
        --trace-ascii h.txt -A Test http://localhost:2345/
