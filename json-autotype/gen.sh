# https://developers.google.com/discovery

function gen() {
  local api=$1
  local json=$2
  local hs=$3
  local toplevel=$4

  curl -o $json $api
  stack exec json-autotype -- --toplevel $toplevel --output $hs $json
  sed \
    -e 's/^module SrcDiscovery/module Discovery./' \
    -e '/^module.*where$/a\
    import RIO\
    import RIO.ByteString.Lazy as BSL' \
    -e 's/^import.*System/-- &/' \
    -e 's/^import.*Data.ByteString.Lazy.Char8/-- &/' \
    -e 's/^import.*Data.Text/-- &/' \
    -e '/^-- | Use parser to get .* object$/a\
    {--' \
    -e '$a\
    --}
    ' \
    -i '' \
    $hs
}

gen https://www.googleapis.com/discovery/v1/apis src/Discovery/list.json src/Discovery/List.hs List
gen https://www.googleapis.com/discovery/v1/apis/storage/v1/rest src/Discovery/document.json src/Discovery/Document.hs Document
