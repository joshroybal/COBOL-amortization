amort.cgi: amort.cbl
	cobc -o amort.cgi -x amort.cbl

install:
	sudo -u apache -g apache cp amort.html /srv/httpd/htdocs
	sudo -u apache -g apache cp amort.js /srv/httpd/htdocs
	sudo -u apache -g apache cp amort.cgi /srv/httpd/cgi-bin
