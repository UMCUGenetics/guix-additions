#!/bin/bash
# Vincent van der Sluis
# V.vanderSluis@umcutrecht.nl
package="$1"

version=$(wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html|egrep "<td>Version</td>" -A1 |tail -n1 |sed -e 's/<td>//g' -e 's/<\/td>//g')
licence=$(wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html|egrep "<td>License</td>" -A1|tail -n1| sed 's/<td>//g'| sed 's/<\/td>//g'| tr [:upper:] [:lower:]|tr -d ">\-\(\'\=\ )" | grep '[a-z].*' -o)
name=$(wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html|egrep "<h1>|</h1>"| sed -e 's/<h1>//g' -e 's/<\/h1>//g')
bioc_link=$(wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html|egrep "Package Short Url" -A1|tail -n1|tr ">" "\n"|egrep "http"|sed 's/<\/a//g')
title=$(wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html|egrep "<h2>|</h2>"|sed -e 's/<h2>//g' -e 's/<\/h2>//g'|grep '[A-Za-z].*' -o)
long_bio=$(wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html|grep '<p>.*</p>$'|head -n1| sed -e 's/<p>//g' -e 's/<\/p>//g'| grep '[A-Za-z].*' -o)
imports=$( wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html| egrep "<td>Imports</td>" -A1|tail -n1| tr "," "\n"| grep '\.html.*</a' -o|grep '>.*<' -o |tr -d ">|<"| tr '[:upper:]' '[:lower:]'|awk '{print "        (\"r-" $1 "\", r-" $1 ")"}')
depends=$(wget -qO- https://www.bioconductor.org/packages/release/bioc/html/${package}.html| egrep "<td>Depends</td>" -A1|tail -n1| tr "," "\n"| grep '\.html.*</a' -o|grep '>.*<' -o |tr -d ">|<"| tr '[:upper:]' '[:lower:]'|awk '{print "        (\"r-" $1 "\", r-" $1 ")"}')






printf "(define-public r-%s\n" ${name,,}
printf "  (package\n"
printf '  (name "r-%s")\n' ${name,,}
printf '  (version "%s")\n' ${version}
printf '  (source (origin\n'
printf '            (method url-fetch)\n'
printf '            (uri (bioconductor-uri "%s" version))\n' ${name}
printf '            (sha256\n'
printf '             (base32\n'
printf '              "1p87ngk0lfbb86hwx63x4xjnw77xslh5a7136l1dwia24r9dccls"))))\n'
printf '  (build-system r-build-system)\n'
printf '  (propagated-inputs\n'
printf '     `(\n'
printf '%s\n' "${imports}"
printf '%s\n' "${depends}"
printf '     ))\n'
printf '  (home-page "%s")\n' ${bioc_link}
printf '  (synopsis "%s")\n' "${title}"
printf '  (description "%s")\n' "${long_bio}"
printf '  (license license:%s)))\n' ${licence}

