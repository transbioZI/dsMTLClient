#!/bin/sh

# Install "dsMTLBase_0.9" package to Armadillo

# curl -u admin:admin -H 'Content-Type: multipart/form-data; boundary=boundary' -H 'Content-Disposition: form-data; name="file"; filename="dsMTLBase_0.9.tar.gz"' -F "file=@dsMTLBase_0.9.tar.gz" -X POST http://localhost:8080/install-package

# Add "dsMTLBase" package to Armadillo's package whitelist

#for p in dsBase dsMediation dsMTLBase dsSurvival dsExposome dsOmics dsML resourcer
for p in dsBase dsMTLBase resourcer
do
    curl -u admin:admin -X POST http://localhost:8080/whitelist/${p}
done
