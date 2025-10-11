cabal run -- registrar \
  -d 'postgresql://localhost/jasliq?user=postgres&password=postgres&application_name=registrar ' \
  -p 6969 -m -f './data/dbdata'
  # -d 
